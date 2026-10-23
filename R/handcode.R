# handcodeR text annotation app - categorial and comparison coding
# Lukas Isermann and Dennis Klingenspohr, 2026
# nolint start

# ============================================================================ #
# Categorial Input Validation                                                  #
# ---------------------------------------------------------------------------- #
# stop() checks that only the handcode() entry point performs                  #
# ============================================================================ #

.check_cat_session <- function(interactive_mode, arg_list, data) {
  if (!interactive_mode) stop("handcode() can only be used in an interactive R session.")
  if (!is.data.frame(data) && !is.character(data)) {
    stop("data must be a character vector or data frame from a previous handcode() session.")
  }
}

.check_cat_args <- function(arg_list, missing) {
  if (length(arg_list) < 1) stop("at least one classification variable must be provided.")
  if (!all(vapply(arg_list, is.character, logical(1)))) {
    stop("all classification arguments must be character vectors.")
  }
  # Empty string is the internal sentinel for "not yet coded" — cannot be a valid category.
  if (any(vapply(arg_list, function(x) "" %in% x, logical(1)))) {
    stop("empty strings are not allowed as category values.")
  }
  # Duplicates within a variable would collapse to one button, silently dropping a category.
  for (i in seq_along(arg_list)) {
    if (length(unique(arg_list[[i]])) < length(arg_list[[i]])) {
      stop("duplicate categories are not allowed.")
    }
  }
  # missing labels render as their own buttons; overlap would make a value ambiguous between the two.
  for (cat_vec in arg_list) {
    if (any(missing %in% cat_vec)) stop("missing values cannot overlap with category values.")
  }
}

.check_cat_keyboard_param <- function(keyboard_shortcuts, arg_list) {
  if (!is.logical(keyboard_shortcuts) || length(keyboard_shortcuts) != 1) {
    stop("keyboard_shortcuts must be a single logical value.")
  }
  # Keys 1–9 map to variables by position; more than 9 would exceed the available key range.
  if (keyboard_shortcuts && length(arg_list) > 9) {
    stop("keyboard_shortcuts = TRUE supports at most 9 classification variables.")
  }
}

# ============================================================================ #
# Categorial Server Setup                                                      #
# ---------------------------------------------------------------------------- #
# Radio-button panels and the save/refresh closures for categorial coding      #
# ============================================================================ #

.setup_categorial_panels <- function(output, values, app_data) {
  # Panels are rendered per variable so category sets can differ while sharing one interaction pattern.
  output$classification_panels <- shiny::renderUI({
    classifications <- app_data$classifications
    if (length(classifications) == 0) {
      return(NULL)
    }
    panels <- lapply(seq_along(names(classifications)), function(i) {
      var_name <- names(classifications)[i]
      choices <- classifications[[var_name]]
      current_val <- .get_current_value(values, var_name, values$counter)
      var_label <- if (isTRUE(app_data$keyboard_shortcuts)) paste0(i, ". ", var_name) else var_name
      shiny::div(
        class = "classification-card",
        shiny::h5(var_label),
        shiny::radioButtons(
          inputId = paste0("class_", var_name),
          label = NULL,
          # choiceNames/choiceValues instead of choices: missing labels need markup to be dimmed.
          choiceNames = c(
            list(" "),
            as.list(unname(choices)),
            lapply(app_data$missing, function(m) shiny::span(class = "missing-choice", m))
          ),
          choiceValues = as.list(c("", unname(choices), .format_NA(app_data$missing))),
          selected = current_val
        )
      )
    })
    do.call(shiny::tagList, panels)
  })
}

.make_categorial_handler <- function(input, session, values, app_data) {
  # save_current and refresh_ui close over input/session/values/app_data via lexical scope so
  # callers can pass them around without re-threading the four arguments through every hook.
  # save_current captures the current row state before any navigation or session close event.
  save_current <- function() {
    for (var_name in names(app_data$classifications)) {
      input_id <- paste0("class_", var_name)
      if (!is.null(input[[input_id]])) {
        values$annotations[[var_name]][values$counter] <- input[[input_id]]
      }
    }
    if (app_data$notes && !is.null(input$note_text)) {
      values$notes[values$counter] <- input$note_text
    }
  }
  refresh_ui <- function() {
    # refresh_ui rehydrates controls when row index changes to keep UI aligned with stored values.
    for (var_name in names(app_data$classifications)) {
      input_id <- paste0("class_", var_name)
      current_val <- .get_current_value(values, var_name, values$counter)
      shiny::updateRadioButtons(session, input_id, selected = current_val)
    }
    if (app_data$notes) {
      shiny::updateTextAreaInput(session, "note_text", value = values$notes[values$counter])
    }
  }
  list(save_current = save_current, refresh_ui = refresh_ui)
}

# ============================================================================ #
# Categorial & Comparison Mode                                                 #
# ---------------------------------------------------------------------------- #
# Entry point, UI, server, and runner for categorial and comparison annotation #
# ============================================================================ #

#' Categorial Text Annotation
#'
#' Launch a Shiny app for hand-coding texts into one or more categorial
#' classification variables. Each variable is defined by name and its
#' set of allowed categories passed via \code{...}.
#'
#' @param data A character vector of texts, or a data frame with a
#'   \code{texts} column (and optionally pre-existing classification
#'   columns to resume coding).
#' @param ... Named character vectors defining classification variables.
#'   Each name becomes a variable, each vector its category levels.
#' @param start Row to start coding at. \code{"first_empty"} (default)
#'   begins at the first row with no completed classifications across all
#'   variables; \code{"all_empty"} filters the workload to uncoded rows
#'   only and restarts at row 1; a numeric value is an explicit row index.
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
#' @param missing Character vector of labels for missing/not-applicable
#'   values. Default \code{c("Not applicable")}.
#' @param pre Optional character vector of texts to prepend as context
#'   (one per row). Turns context on automatically when
#'   \code{context = FALSE}. Supplied without \code{post}, only the
#'   preceding context is shown.
#' @param post Optional character vector of texts to append as context
#'   (one per row). Turns context on automatically when
#'   \code{context = FALSE}. Supplied without \code{pre}, only the
#'   following context is shown.
#' @param comparison Optional character vector for paired-text comparison
#'   workflows.
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
#' @param keyboard_shortcuts Logical. If \code{TRUE}, keys \code{1}-\code{9}
#'   cycle through the radio choices of the variable at that position
#'   (at most 9 classification variables), \code{Space} goes to the
#'   previous and \code{Enter} to the next text. Default \code{FALSE}.
#'
#' @return A data frame containing the original texts plus one factor
#'   column per classification variable, and, where used, the
#'   \code{notes}, \code{pre}/\code{post} and comparison columns, so it can
#'   be passed back as \code{data} to resume coding. Closing the app
#'   (Save & Exit or otherwise) returns the annotated data to the calling
#'   R session.
#'
#' @examples
#' \dontrun{
#' texts <- c("I love this product", "Worst purchase ever", "It's okay")
#' result <- handcode(
#'   texts,
#'   sentiment = c("positive", "neutral", "negative")
#' )
#' }
#'
#' @seealso \code{\link{quickcode}} for single-variable quickcoding.
#' @export
handcode <- function(data, ..., start = "first_empty", randomize = FALSE,
                     context = FALSE, missing = c("Not applicable"),
                     pre = NULL, post = NULL,
                     comparison = NULL,
                     pre_comparison = NULL, post_comparison = NULL,
                     snapshot_dir = NULL, notes = FALSE,
                     keyboard_shortcuts = FALSE) {
  arg_list <- list(...)
  original_name <- deparse(substitute(data))
  .check_cat_session(.interactive(), arg_list, data)

  # CRAN policy: writes to user filespace only when snapshot_dir names an existing directory.
  # An invalid/non-existent path stops here with a clear message before the app launches.
  save_loc <- .snapshot_dir_setup(snapshot_dir, original_name)
  has_comparison <- !is.null(comparison) || (is.data.frame(data) && "comparison" %in% names(data))
  # A resumed session's notes column re-activates notes UI even if notes wasn't passed again.
  has_notes <- notes || (is.data.frame(data) && "notes" %in% names(data))

  # Char-vector path: validate ... category specs before promoting to a data frame.
  if (is.character(data)) {
    .check_cat_args(arg_list, missing)
    if (has_comparison) .check_comparison_args(comparison, data)
  }

  prep <- .prepare_app_data(
    data, arg_list, missing, has_comparison, comparison,
    start, randomize, context, pre, post, pre_comparison, post_comparison
  )
  prepared_data <- prep$prepared_data
  factor_levels <- prep$factor_levels

  .check_cat_keyboard_param(keyboard_shortcuts, factor_levels)

  # app_data is the immutable runtime bundle passed into UI/server builders.
  app_data <- list(
    data               = prepared_data$data,
    original_data      = prepared_data$original_data,
    start_val          = prepared_data$start_val,
    context            = prep$context,
    classifications    = factor_levels,
    missing            = missing,
    original_name      = original_name,
    notes              = has_notes,
    save_loc           = save_loc,
    keyboard_shortcuts = keyboard_shortcuts
  )

  # UI execution is isolated in the app runner; this function only prepares and returns result data.
  result <- if (has_comparison) {
    .run_comparison_app(app_data)
  } else {
    .run_categorial_app(app_data)
  }
  message("\nYour data was returned to the R workspace.\n\n", .citation())
  result
}

.build_cat_keyboard_script <- function(app_data) {
  # Keys 1-9 cycle the radio choices of the variable at that position. Only fires outside form
  # fields. Sibling of .build_quickcode_keyboard_script(); cap of 9 matches the keyboard_shortcuts limit.
  var_names_js <- paste0('["', paste(names(app_data$classifications), collapse = '","'), '"]')
  paste0(
    "$(document).on('keyup', function(e) {",
    " if (hcInField()) return;",
    " if (e.key === ' ') { e.preventDefault(); $('#prev').click(); }",
    " if (e.key === 'Enter') $('#next').click();",
    "});",
    sprintf('
      var numericVars = %s;
      $(document).on("keydown", function(e) {
        if (hcInField()) return;
        var num = parseInt(e.key);
        if (!isNaN(num) && num >= 1 && num <= numericVars.length) {
          e.preventDefault();
          var varName = numericVars[num - 1];
          var radios = $("input[name=\'class_" + varName + "\']");
          var currentIdx = radios.index(radios.filter(":checked"));
          if (currentIdx === -1) currentIdx = 0;
          var nextIdx = (currentIdx + 1) %% radios.length;
          radios.eq(nextIdx).prop("checked", true).trigger("change");
        }
      });
    ', var_names_js)
  )
}

.build_categorial_ui <- function(app_data) {
  body_slot <- shiny::div(
    class = "text-display",
    shiny::uiOutput("context_before"),
    shiny::div(class = "current-text", shiny::htmlOutput("current_text")),
    shiny::uiOutput("context_after")
  )
  .build_app_shell(
    app_data, "handcodeR - Categorial", body_slot,
    shiny::uiOutput("classification_panels"),
    keyboard_script = if (isTRUE(app_data$keyboard_shortcuts)) .build_cat_keyboard_script(app_data) else NULL
  )
}

.categorial_server <- function(app_data) {
  # Wires shared outputs, category panels, and nav/save handlers into one server function.
  function(input, output, session) {
    values <- .init_server_values(app_data)
    .setup_common_outputs(input, output, session, values, app_data)
    .setup_categorial_panels(output, values, app_data)
    handler <- .make_categorial_handler(input, session, values, app_data)
    .setup_nav_handler(input, session, values, handler$save_current, handler$refresh_ui)
    .setup_save_handler(input, session, values, app_data, handler$save_current)
  }
}

.run_categorial_app <- function(app_data) {
  # Wraps UI + server into a Shiny app and blocks until the user closes the session.
  # Return value is the annotated data frame propagated up via stopApp() in .setup_save_handler.
  shiny::runApp(shiny::shinyApp(
    ui     = .build_categorial_ui(app_data),
    server = .categorial_server(app_data)
  ))
}

.build_comparison_ui <- function(app_data) {
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
  .build_app_shell(app_data, "handcodeR - Comparison", body_slot,
    shiny::uiOutput("classification_panels"),
    extra_styles = .comparison_styles(),
    keyboard_script = if (isTRUE(app_data$keyboard_shortcuts)) .build_cat_keyboard_script(app_data) else NULL
  )
}

.comparison_server <- function(app_data) {
  # Comparison server extends categorial with a second text channel and renames runtime context
  # columns to pre_comparison/post_comparison on save.
  function(input, output, session) {
    values <- .init_server_values(app_data)
    .setup_common_outputs(input, output, session, values, app_data)
    .setup_comparison_outputs(output, values)
    .setup_categorial_panels(output, values, app_data)
    handler <- .make_categorial_handler(input, session, values, app_data)
    .setup_nav_handler(input, session, values, handler$save_current, handler$refresh_ui)

    .setup_save_handler(input, session, values, app_data, handler$save_current,
      extra_cleanup_function = .rename_comparison_context_columns
    )
  }
}

.run_comparison_app <- function(app_data) {
  # Comparison-mode equivalent of .run_categorial_app — same launch pattern, different UI/server.
  shiny::runApp(shiny::shinyApp(
    ui     = .build_comparison_ui(app_data),
    server = .comparison_server(app_data)
  ))
}

# nolint end
