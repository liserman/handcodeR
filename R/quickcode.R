# handcodeR text annotation app - single-variable quickcoding
# Lukas Isermann and Dennis Klingenspohr, 2026
# nolint start

# ============================================================================ #
# Quickcode Input Validation                                                   #
# ---------------------------------------------------------------------------- #
# stop() checks that only the quickcode() entry point performs                 #
# ============================================================================ #

.check_quickcode_session <- function(interactive_mode, data) {
  if (!interactive_mode) stop("quickcode() can only be used in an interactive R session.")
  if (!is.data.frame(data) && !is.character(data)) {
    stop("data must be a character vector or data frame from a previous quickcode() session.")
  }
}

.check_quickcode_args <- function(arg_list, missing) {
  # One variable only: the whole screen is a single keypad for a single question, and every
  # level already claims one of its keys, so a second variable would have no keys left.
  if (length(arg_list) != 1) stop("exactly one classification variable must be provided.")
  # Keys 1-9 address the levels by position; the numpad offers no tenth key.
  if (!is.character(arg_list[[1]]) || length(arg_list[[1]]) < 2 || length(arg_list[[1]]) > 9) {
    stop("the classification variable must be a character vector with 2 to 9 values.")
  }
  # Empty string is the internal sentinel for "not yet coded" — cannot be a valid category.
  if ("" %in% arg_list[[1]]) stop("empty strings are not allowed as category values.")
  # Duplicates within a variable would collapse to one button, silently dropping a category.
  if (length(unique(arg_list[[1]])) < length(arg_list[[1]])) stop("duplicate categories are not allowed.")
  # missing renders as its own button; overlap would make a value ambiguous between the two.
  if (any(missing %in% arg_list[[1]])) stop("missing values cannot overlap with category values.")
}

.check_quickcode_params <- function(missing, advance_delay) {
  # Quickcode mode uses one shared missing button on key 0, so only a single label is valid.
  if (length(missing) != 1) stop("missing argument must be a single value in quickcode annotation.")
  if (!is.numeric(advance_delay) || length(advance_delay) != 1 || is.na(advance_delay) || advance_delay < 0) {
    stop("advance_delay must be a single non-negative number of seconds.")
  }
}

.check_quickcode_levels <- function(factor_levels) {
  # Resumed sessions never re-pass ..., so the one-variable / 2-to-9-levels contract is
  # re-checked against the levels reconstructed from the data frame itself.
  if (length(factor_levels) != 1) stop("exactly one classification variable must be provided.")
  n_levels <- length(factor_levels[[1]])
  if (n_levels < 2 || n_levels > 9) {
    stop("the classification variable must be a character vector with 2 to 9 values.")
  }
}

# ============================================================================ #
# Quickcode Styles                                                             #
# ---------------------------------------------------------------------------- #
# CSS builder for the quickcode key grid, kept out of the shared style set     #
# ============================================================================ #

.quickcode_styles <- function() {
  # Every level is an equally weighted TRUE button: neutral slate at rest, green hover as the
  # affordance, dark green fill once chosen.
  shiny::tags$style(shiny::HTML("
    .quickcode-grid { display: flex; flex-direction: column; gap: 14px; }
    .quickcode-row { display: flex; gap: 14px; }
    .quickcode-slot { flex: 1 1 0; min-width: 0; display: flex; flex-direction: column; align-items: center; gap: 8px; }
    .quickcode-filler { flex: 1 1 0; min-width: 0; }
    .qc-button { width: 100%; box-sizing: border-box; padding: 20px 12px; text-align: center; border: 2px solid #cbd5e1; border-radius: 8px; background: #f8fafc; color: #334155; font-size: 1.2rem; cursor: pointer; transition: all 0.08s ease; }
    .qc-button:hover { border-color: #10b981; background: #e2f6ef; color: #10b981; }
    .qc-button.selected { background: #0a7853 !important; color: #fff !important; border-color: #0a7853 !important; }
    .qc-key { width: 32px; height: 32px; display: flex; align-items: center; justify-content: center; border: 1px solid #cbd5e1; border-radius: 6px; background: #fff; color: #64748b; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: 0.9rem; font-weight: 600; }
    .missing-button { width: 100%; box-sizing: border-box; padding: 10px; text-align: center; border: 2px solid #cbd5e1; border-radius: 8px; background: #f1f5f9; color: #64748b; font-size: 1rem; cursor: pointer; transition: all 0.08s ease; }
    .missing-button:hover { background: #cbd5e1; color: #334155; }
    .missing-button.selected { background: #64748b; color: #fff; border-color: #64748b; }
  "))
}

# ============================================================================ #
# Quickcode Mode                                                               #
# ---------------------------------------------------------------------------- #
# Entry point, UI, server, and runner for single-variable quickcoding          #
# ============================================================================ #

#' Single-Variable Quickcoding
#'
#' Launch a Shiny app for high-throughput hand-coding of texts into exactly
#' one classification variable. Every level of that variable is rendered as
#' its own button with a number key printed underneath; pressing the key or
#' clicking the button records the level and advances to the next row after
#' \code{advance_delay} seconds. Keys follow the physical numpad layout, so
#' the bottom button row is always \code{1}-\code{2}-\code{3}.
#'
#' @param data A character vector of texts, or a data frame with a
#'   \code{texts} column (and optionally a pre-existing classification
#'   column to resume coding).
#' @param ... Exactly one named character vector defining the
#'   classification variable. Its name becomes the output column, its
#'   entries the levels. Between 2 and 9 levels are allowed, because keys
#'   \code{1}-\code{9} address the levels by position.
#' @param start Row to start coding at. \code{"first_empty"} (default)
#'   begins at the first row with no completed classification;
#'   \code{"all_empty"} filters the workload to uncoded rows only and
#'   restarts at row 1; a numeric value is an explicit row index.
#' @param randomize Logical. If \code{TRUE}, shuffle the display order of
#'   uncoded rows only; the returned data frame keeps the original row
#'   order. Default \code{FALSE}.
#' @param context Context display mode. \code{TRUE} always shows the
#'   preceding/following texts, \code{FALSE} (default) never shows them,
#'   and \code{"FLEX"} adds a runtime checkbox to toggle context while
#'   coding. Supplying any of \code{pre}, \code{post},
#'   \code{pre_comparison} or \code{post_comparison} upgrades
#'   \code{FALSE} to \code{TRUE}; without them, context is taken from the
#'   neighbouring rows.
#' @param missing Single label for missing/not-applicable values.
#'   Quickcode mode uses one shared missing button bound to key \code{0},
#'   so exactly one label is allowed. Default \code{"Not applicable"}.
#' @param pre Optional character vector of texts to prepend as context
#'   (one per row). Turns context on automatically when
#'   \code{context = FALSE}. Supplied without \code{post}, only the
#'   preceding context is shown.
#' @param post Optional character vector of texts to append as context
#'   (one per row). Turns context on automatically when
#'   \code{context = FALSE}. Supplied without \code{pre}, only the
#'   following context is shown.
#' @param comparison Optional character vector for paired-text comparison
#'   workflows. Both texts are shown side by side above the same button
#'   grid.
#' @param pre_comparison Optional context-before vector for the comparison
#'   text. Turns context on automatically when \code{context = FALSE}, and
#'   follows the same one-sided rule as \code{pre}.
#' @param post_comparison Optional context-after vector for the comparison
#'   text. Turns context on automatically when \code{context = FALSE}, and
#'   follows the same one-sided rule as \code{post}.
#' @param snapshot_dir Either \code{NULL} (default; no Save Snapshot button is
#'   shown) or a character path to an existing directory. When a directory
#'   is given, a Save Snapshot button is shown that writes timestamped
#'   \code{<name>_snapshot_<timestamp>.RData} snapshots there. The
#'   directory must already exist. \code{FALSE} is accepted as a synonym
#'   for \code{NULL}.
#' @param notes Logical. If \code{TRUE}, show a free-text notes input
#'   in the UI. Default \code{FALSE}.
#' @param advance_delay Seconds to wait after a selection before the app
#'   advances to the next row. A single non-negative number; \code{0}
#'   advances immediately. Default \code{0.7}. Independently of the
#'   number keys, \code{Space} goes to the previous and \code{Enter} to
#'   the next text.
#'
#' @return A data frame containing the original texts plus one factor
#'   column holding the selected level per row, and, where used, the
#'   \code{notes}, \code{pre}/\code{post} and comparison columns, so it can
#'   be passed back as \code{data} to resume coding. Closing the app
#'   (Save & Exit or otherwise) returns the annotated data to the calling
#'   R session.
#'
#' @examples
#' \dontrun{
#' texts <- c("I love this product", "Worst purchase ever", "Okay I guess")
#' result <- quickcode(
#'   texts,
#'   sentiment = c("positive", "neutral", "negative")
#' )
#' }
#'
#' @seealso \code{\link{handcode}} for multi-variable categorial annotation.
#' @export
quickcode <- function(data, ..., start = "first_empty", randomize = FALSE,
                      context = FALSE, missing = c("Not applicable"),
                      pre = NULL, post = NULL,
                      comparison = NULL,
                      pre_comparison = NULL, post_comparison = NULL,
                      snapshot_dir = NULL, notes = FALSE,
                      advance_delay = 0.7) {
  arg_list <- list(...)
  original_name <- deparse(substitute(data))
  .check_quickcode_session(.interactive(), data)

  # CRAN policy: writes to user filespace only when snapshot_dir names an existing directory.
  # An invalid/non-existent path stops here with a clear message before the app launches.
  save_loc <- .snapshot_dir_setup(snapshot_dir, original_name)
  has_comparison <- !is.null(comparison) || (is.data.frame(data) && "comparison" %in% names(data))
  # A resumed session's notes column re-activates notes UI even if notes wasn't passed again.
  has_notes <- notes || (is.data.frame(data) && "notes" %in% names(data))

  # Char-vector path: validate the single ... entry before promoting to a data frame.
  if (is.character(data)) {
    .check_quickcode_args(arg_list, missing)
    if (has_comparison) .check_comparison_args(comparison, data)
  }

  .check_quickcode_params(missing, advance_delay)

  prep <- .prepare_app_data(
    data, arg_list, missing, has_comparison, comparison,
    start, randomize, context, pre, post, pre_comparison, post_comparison
  )
  prepared_data <- prep$prepared_data
  factor_levels <- prep$factor_levels

  # Re-checked after preparation because a resumed data frame carries its variable in the
  # columns rather than in ..., where .check_quickcode_args() would never see it.
  .check_quickcode_levels(factor_levels)

  # app_data is the immutable runtime bundle passed into UI/server builders.
  app_data <- list(
    data            = prepared_data$data,
    original_data   = prepared_data$original_data,
    start_val       = prepared_data$start_val,
    context         = prep$context,
    classifications = factor_levels,
    missing         = missing,
    original_name   = original_name,
    notes           = has_notes,
    save_loc        = save_loc,
    advance_delay   = advance_delay
  )

  # UI execution is isolated in the app runner; this function only prepares and returns result data.
  result <- if (has_comparison) {
    .run_quickcode_comparison_app(app_data)
  } else {
    .run_quickcode_app(app_data)
  }
  message("\nYour data was returned to the R workspace.\n\n", .citation())
  result
}

#' @noRd
.quickcode_key <- function(i, n) {
  # Keys mirror the physical numpad so the coding hand never has to look down: the bottom
  # button row is always 1-2-3. i is 0-based. Up to 5 levels stay in a single left-to-right
  # row; beyond that the grid is three columns wide and the key rows run bottom-up.
  i <- as.integer(i)
  if (n <= 5) {
    return(i + 1L)
  }
  rows <- as.integer(ceiling(n / 3))
  (rows - 1L - i %/% 3L) * 3L + i %% 3L + 1L
}

#' @noRd
.quickcode_button_ids <- function(app_data) {
  # One id list drives both the clear-all selector and the key map, so the two can never
  # disagree about which buttons exist.
  var_name <- names(app_data$classifications)[1]
  safe_id <- .sanitize_id(var_name)
  c(
    paste0("btn_", safe_id, "_", seq_along(app_data$classifications[[var_name]])),
    paste0("btn_", safe_id, "_missing")
  )
}

.build_quickcode_grid <- function(app_data, var_name, current_val) {
  # Slots are laid out to match .quickcode_key(): one row up to 5 levels, three columns
  # beyond. A short final row is padded with invisible fillers so the buttons that are there
  # keep their column width instead of stretching or centring.
  safe_id <- .sanitize_id(var_name)
  choices <- app_data$classifications[[var_name]]
  n <- length(choices)
  per_row <- if (n <= 5) n else 3L
  row_idx <- split(seq_len(n), (seq_len(n) - 1L) %/% per_row)
  rows <- lapply(row_idx, function(idx) {
    slots <- lapply(idx, function(i) {
      shiny::div(
        class = "quickcode-slot",
        shiny::actionButton(
          inputId = paste0("btn_", safe_id, "_", i),
          label   = choices[i],
          class   = paste("qc-button", if (current_val == choices[i]) "selected" else "")
        ),
        shiny::div(class = "qc-key", as.character(.quickcode_key(i - 1L, n)))
      )
    })
    fillers <- replicate(per_row - length(idx), shiny::div(class = "quickcode-filler"), simplify = FALSE)
    do.call(shiny::div, c(list(class = "quickcode-row"), slots, fillers))
  })
  do.call(shiny::div, c(list(class = "quickcode-grid"), unname(rows)))
}

.setup_quickcode_panel <- function(output, values, app_data) {
  # The grid is rendered once and kept in sync client-side afterwards, so a selection never
  # costs a server round-trip re-render in the middle of a keystroke burst.
  output$quickcode_panel <- shiny::renderUI({
    shiny::isolate({
      var_name <- names(app_data$classifications)[1]
      safe_id <- .sanitize_id(var_name)
      current_val <- .get_current_value(values, var_name, values$counter)
      shiny::div(
        class = "classification-card",
        shiny::h5(var_name),
        .build_quickcode_grid(app_data, var_name, current_val),
        # Missing sits below the grid across the full width on key 0, outside the level keys.
        shiny::div(
          class = "quickcode-row", style = "margin-top: 14px;",
          shiny::div(
            class = "quickcode-slot",
            shiny::actionButton(
              inputId = paste0("btn_", safe_id, "_missing"),
              label = app_data$missing[1],
              class = paste(
                "missing-button",
                if (current_val == .format_NA(app_data$missing[1])) "selected" else ""
              )
            ),
            shiny::div(class = "qc-key", "0")
          )
        )
      )
    })
  })
}

.build_quickcode_keyboard_script <- function(app_data) {
  # Space/Enter keep their global meaning. Digits are mapped to button ids here, in R, so
  # .quickcode_key() stays the single source of truth for the layout and the JS never has to
  # re-derive it. Auto-advance is scheduled once per selection: the click delegate ignores
  # jQuery-triggered clicks (detail 0), which is what the key handler fires, so a keypress
  # advances exactly once while a real mouse click still advances too.
  var_name <- names(app_data$classifications)[1]
  safe_id <- .sanitize_id(var_name)
  n <- length(app_data$classifications[[var_name]])
  keys <- vapply(seq_len(n), function(i) .quickcode_key(i - 1L, n), integer(1))
  key_map <- paste0(
    "{",
    paste0(sprintf("'%d': 'btn_%s_%d'", keys, safe_id, seq_len(n)), collapse = ", "),
    sprintf(", '0': 'btn_%s_missing'}", safe_id)
  )
  paste0(
    "$(document).on('keyup', function(e) {",
    " if (hcInField()) return;",
    " if (e.key === ' ') { e.preventDefault(); $('#prev').click(); }",
    " if (e.key === 'Enter') $('#next').click();",
    "});",
    sprintf('
      var qcKeys = %s;
      var qcDelay = %s;
      function qcAdvance() { setTimeout(function() { $("#next").click(); }, qcDelay); }
      $(document).on("click", ".qc-button, .missing-button", function(e) {
        if (e.detail > 0) qcAdvance();
      });
      $(document).on("keydown", function(e) {
        if (hcInField()) return;
        var id = qcKeys[e.key];
        if (!id) return;
        e.preventDefault();
        $("#" + id).click();
        qcAdvance();
      });
    ', key_map, format(app_data$advance_delay * 1000, scientific = FALSE))
  )
}

.build_quickcode_ui <- function(app_data) {
  body_slot <- shiny::div(
    class = "text-display",
    shiny::uiOutput("context_before"),
    shiny::div(class = "current-text", shiny::htmlOutput("current_text")),
    shiny::uiOutput("context_after")
  )
  .build_app_shell(app_data, "handcodeR - Quickcode", body_slot,
    shiny::uiOutput("quickcode_panel"),
    extra_styles = .quickcode_styles(),
    keyboard_script = .build_quickcode_keyboard_script(app_data)
  )
}

.make_quickcode_handler <- function(input, session, values, app_data) {
  # Mirrors .make_categorial_handler(): registers observers, returns save_current + refresh_ui.
  # Both closures capture input/session/values/app_data via lexical scope.
  var_name <- names(app_data$classifications)[1]
  safe_id <- .sanitize_id(var_name)
  choices <- app_data$classifications[[var_name]]
  missing_val <- .format_NA(app_data$missing[1])
  clear_js <- paste0("$('", paste0("#", .quickcode_button_ids(app_data), collapse = ", "), "').removeClass('selected');")
  select_js <- function(button_id) sprintf("%s $('#%s').addClass('selected');", clear_js, button_id)

  save_current <- function() {
    # Levels are written the moment a button fires; this hook only persists notes.
    if (app_data$notes && !is.null(input$note_text)) {
      values$notes[values$counter] <- input$note_text
    }
  }
  refresh_ui <- function() {
    # Button classes are recomputed on navigation so visual state always matches stored values.
    current_val <- .get_current_value(values, var_name, values$counter)
    choice_idx <- match(current_val, choices)
    selected_id <- if (!is.na(choice_idx)) {
      paste0("btn_", safe_id, "_", choice_idx)
    } else if (current_val == missing_val) {
      paste0("btn_", safe_id, "_missing")
    } else {
      ""
    }
    shinyjs::runjs(if (nzchar(selected_id)) select_js(selected_id) else clear_js)
    if (app_data$notes) {
      # Notes field follows row navigation to maintain per-row note continuity.
      shiny::updateTextAreaInput(session, "note_text", value = values$notes[values$counter])
    }
  }
  # One observer per level plus the missing button. Writing the chosen label is what makes the
  # row one-hot: the column holds exactly one level, so every other level is implicitly FALSE.
  lapply(seq_along(choices), function(i) {
    shiny::observeEvent(input[[paste0("btn_", safe_id, "_", i)]], {
      values$annotations[[var_name]][values$counter] <- choices[i]
      # Only update CSS classes client side
      shinyjs::runjs(select_js(paste0("btn_", safe_id, "_", i)))
    })
  })
  shiny::observeEvent(input[[paste0("btn_", safe_id, "_missing")]], {
    values$annotations[[var_name]][values$counter] <- missing_val
    # Only update CSS classes client side
    shinyjs::runjs(select_js(paste0("btn_", safe_id, "_missing")))
  })
  list(save_current = save_current, refresh_ui = refresh_ui)
}

.quickcode_server <- function(app_data) {
  # Quickcode server delegates panel rendering and observer registration to shared helpers,
  # mirroring the structure of .categorial_server() for consistency.
  function(input, output, session) {
    values <- .init_server_values(app_data)
    .setup_common_outputs(input, output, session, values, app_data)
    .setup_quickcode_panel(output, values, app_data)
    handler <- .make_quickcode_handler(input, session, values, app_data)
    .setup_nav_handler(input, session, values, handler$save_current, handler$refresh_ui)
    .setup_save_handler(input, session, values, app_data, handler$save_current)
  }
}

.run_quickcode_app <- function(app_data) {
  # Quickcode equivalent of .run_categorial_app - same launch pattern, different UI/server.
  shiny::runApp(shiny::shinyApp(
    ui     = .build_quickcode_ui(app_data),
    server = .quickcode_server(app_data)
  ))
}

# ============================================================================ #
# Quickcode Comparison Mode                                                    #
# ---------------------------------------------------------------------------- #
# UI, server, and runner for quickcoding with side-by-side comparison          #
# ============================================================================ #

.build_quickcode_comparison_ui <- function(app_data) {
  body_slot <- shiny::div(
    class = "text-display",
    shiny::div(
      class = "row",
      shiny::div(
        class = "col-6 comparison-col",
        shiny::div(class = "comparison-label", "Statement"),
        shiny::uiOutput("context_before"),
        shiny::div(class = "current-text", shiny::htmlOutput("current_text")),
        shiny::uiOutput("context_after")
      ),
      shiny::div(
        class = "col-6 comparison-col",
        shiny::div(class = "comparison-label", "Comparison"),
        shiny::uiOutput("context_before_comparison"),
        shiny::div(class = "current-text", shiny::htmlOutput("comparison_text")),
        shiny::uiOutput("context_after_comparison")
      )
    )
  )
  # Both style sets are required: quickcode for the button grid, comparison for the two-column
  # layout divider. The button grid itself is identical in either mode.
  .build_app_shell(app_data, "handcodeR - Quickcode (Comparison)", body_slot,
    shiny::uiOutput("quickcode_panel"),
    extra_styles = shiny::tagList(.quickcode_styles(), .comparison_styles()),
    keyboard_script = .build_quickcode_keyboard_script(app_data)
  )
}

.quickcode_comparison_server <- function(app_data) {
  # Structure mirrors .comparison_server(): init, common outputs, comparison outputs,
  # quickcode panel, nav/save with context rename.
  function(input, output, session) {
    values <- .init_server_values(app_data)
    .setup_common_outputs(input, output, session, values, app_data)
    .setup_comparison_outputs(output, values)
    .setup_quickcode_panel(output, values, app_data)
    handler <- .make_quickcode_handler(input, session, values, app_data)
    .setup_nav_handler(input, session, values, handler$save_current, handler$refresh_ui)
    .setup_save_handler(input, session, values, app_data, handler$save_current,
      extra_cleanup_function = .rename_comparison_context_columns
    )
  }
}

.run_quickcode_comparison_app <- function(app_data) {
  # Quickcode comparison equivalent of .run_quickcode_app - same launch, different UI/server.
  shiny::runApp(shiny::shinyApp(
    ui     = .build_quickcode_comparison_ui(app_data),
    server = .quickcode_comparison_server(app_data)
  ))
}

# nolint end
