# handcodeR text annotation app
# Lukas Isermann and Dennis Klingenspohr, 2026
# nolint start
#' @importFrom stats setNames
NULL

# Single source for the citation shown on exit (modal and console). Keep in sync with inst/CITATION.
# Version is read from DESCRIPTION at call time so it never drifts out of sync.
.citation <- function() {
  paste0(
    "Please cite: Isermann, Lukas and Klingenspohr, Dennis. 2026. ",
    "handcodeR: Text annotation app. R package version ",
    getNamespaceVersion("handcodeR"),
    ". https://github.com/liserman/handcodeR"
  )
}

# ============================================================================ #
# Snapshot Setup                                                               #
# ---------------------------------------------------------------------------- #
# Resolves snapshot_dir into a save location for the Save Snapshot button      #
# ============================================================================ #

# CRAN policy forbids writing to user filespace without explicit user direction.
# The snapshot_dir argument doubles as that direction: NULL disables it, a directory path enables it
# and names the target. The filename prefix is derived from the data variable name and sanitized
# for filesystem use. A bare TRUE carries no location and is therefore rejected. FALSE stays
# accepted as a synonym for NULL so calls written against earlier versions keep working.

.snapshot_dir_setup <- function(snapshot_dir, default_name) {
  if (is.null(snapshot_dir) || isFALSE(snapshot_dir)) {
    return(NULL)
  }
  if (!is.character(snapshot_dir) || length(snapshot_dir) != 1 || !nzchar(trimws(snapshot_dir))) {
    stop("snapshot_dir must be NULL or a path to an existing directory.")
  }
  dir_out <- normalizePath(trimws(snapshot_dir), mustWork = FALSE)
  if (!dir.exists(dir_out)) {
    stop(sprintf("snapshot_dir path does not exist: '%s'", dir_out))
  }
  list(dir = dir_out, prefix = .sanitize_id(default_name))
}

# ============================================================================ #
# Styles                                                                       #
# ---------------------------------------------------------------------------- #
# CSS builders shared across all app modes                                     #
# ============================================================================ #

.common_styles <- function() {
  # Common styles centralize shared layout tokens so all app modes keep one visual baseline.
  shiny::tags$style(shiny::HTML("
    .app-container { padding: 20px 2.5%; }
    .text-display { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 12px; padding: 12px 24px 24px; margin-bottom: 20px; min-height: calc(7.04rem + 5px); display: flex; flex-direction: column; resize: vertical; overflow: auto; }
    .current-text { font-size: 1.1rem; line-height: 1.6; color: #1e293b; }
    .context-text { color: #94a3b8; font-size: 0.95rem; }
    .classifications-container { display: flex; gap: 16px; margin-bottom: 20px; flex-wrap: wrap; }
    .classification-card { flex: 1 1 0px; min-width: 200px; background: white; border: 1px solid #e2e8f0; border-radius: 12px; padding: 16px; }
    .classification-card h5 { color: #1e293b; font-weight: 600; margin-bottom: 10px; border-bottom: 2px solid #e2e8f0; padding-bottom: 6px; }
    .save-button-container { display: flex; justify-content: center; gap: 12px; margin-top: 16px; }
    .missing-choice { opacity: 0.5; }
  "))
}

.quickcode_styles <- function() {
  # Every level is an equally weighted TRUE button: neutral slate at rest, green hover as the
  # affordance, dark green fill once chosen. No left/right color coding, hence no colors argument.
  shiny::tags$style(shiny::HTML("
    .quickcode-grid { display: flex; flex-direction: column; gap: 14px; }
    .quickcode-row { display: flex; gap: 14px; }
    .quickcode-slot { flex: 1 1 0; min-width: 0; display: flex; flex-direction: column; align-items: center; gap: 8px; }
    .quickcode-filler { flex: 1 1 0; min-width: 0; }
    .qc-button { width: 100%; box-sizing: border-box; padding: 20px 12px; text-align: center; border: 2px solid #cbd5e1; border-radius: 8px; background: #f8fafc; color: #334155; font-size: 1.2rem; cursor: pointer; transition: all 0.08s ease; }
    .qc-button:hover { border-color: #10b981; background: #e2f6ef; color: #10b981; }
    .qc-button.selected { background: #0a7853 !important; color: #fff !important; border-color: #0a7853 !important; animation: hcPulse 0.5s ease-out; }
    .qc-key { width: 32px; height: 32px; display: flex; align-items: center; justify-content: center; border: 1px solid #cbd5e1; border-radius: 6px; background: #fff; color: #64748b; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: 0.9rem; font-weight: 600; }
    .missing-button { width: 100%; box-sizing: border-box; padding: 10px; text-align: center; border: 2px solid #cbd5e1; border-radius: 8px; background: #f1f5f9; color: #64748b; font-size: 1rem; cursor: pointer; transition: all 0.08s ease; }
    .missing-button:hover { background: #cbd5e1; color: #334155; }
    .missing-button.selected { background: #64748b; color: #fff; border-color: #64748b; }
    @keyframes hcPulse { 0% { box-shadow: 0 0 0 0 rgba(10,120,83,0.45); } 100% { box-shadow: 0 0 0 14px rgba(10,120,83,0); } }
  "))
}

.comparison_styles <- function() {
  # Comparison styles focus on side-by-side readability and visual separation of both text channels.
  shiny::tags$style(shiny::HTML("
    .comparison-label { font-size: 0.75rem; font-weight: 600; color: #64748b; text-transform: uppercase; letter-spacing: 0.05em; margin-bottom: 6px; }
    .comparison-col { padding: 0 12px; }
    .comparison-col:first-child { border-right: 1px solid #e2e8f0; }
    .text-display > .row { flex: 1; align-items: stretch; }
    .text-display > .row > .comparison-col { display: flex; flex-direction: column; }
  "))
}

# ============================================================================ #
# Utilities                                                                    #
# ---------------------------------------------------------------------------- #
# ID sanitizer, interactive() wrapper for testability, missing formatter       #
# ============================================================================ #

#' @noRd
.sanitize_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

.interactive <- function() interactive()

#' @noRd
.format_NA <- function(missing) paste0("_", missing, "_")

#' @noRd
.get_current_value <- function(values, var_name, counter) {
  # Normalizes NA/empty stored values to "" so the UI renders no spurious selection for uncoded rows.
  val <- values$annotations[[var_name]][counter]
  if (!is.na(val) && val != "") val else ""
}

# ============================================================================ #
# Input Validation                                                             #
# ---------------------------------------------------------------------------- #
# All stop() checks for both entry points, separated by mode                   #
# ============================================================================ #

.check_common_params <- function(data, start, randomize, context, pre, post) {
  # Shared by both entry points: validates the start/randomize/context/pre/post args common to either mode.
  if (length(start) > 1) stop("start must be a single value.")
  if (!is.numeric(start) && !start %in% c("first_empty", "all_empty")) stop("start must be numeric, first_empty, or all_empty.")
  if (!is.logical(randomize) || length(randomize) != 1) stop("randomize must be a single logical value.")
  # "FLEX" is a third context mode that renders a runtime toggle instead of a fixed on/off state.
  if (!(isTRUE(context) || isFALSE(context) || identical(context, "FLEX"))) stop("context must be TRUE, FALSE, or \"FLEX\".")
  n_rows <- if (is.data.frame(data)) nrow(data) else length(data)
  if (!is.null(pre) && length(pre) != n_rows) stop("pre must have the same length as data.")
  if (!is.null(post) && length(post) != n_rows) stop("post must have the same length as data.")
}

.check_data_first_col <- function(data) {
  # All modes share one data contract: first column named "texts", character type.
  if (names(data)[1] != "texts" || !is.character(data[[1]])) {
    stop("first column must be texts and contain character data.")
  }
}

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

.check_arg_names <- function(arg_list) {
  # Shared by both entry points and by char-vector and resumed-data-frame sessions.
  # Unnamed ... arguments used to be auto-named cat1/bin1: output columns whose names the
  # caller never chose, and silently ignored entirely on the resume path.
  if (length(arg_list) > 0 && (is.null(names(arg_list)) || !all(nzchar(names(arg_list))))) {
    stop("all classification variables must be named, e.g. sentiment = c(\"positive\", \"negative\").")
  }
}

.check_comparison_args <- function(comparison, data) {
  # Only called on the char-vector path; df sessions carry the comparison column already.
  if (is.null(comparison)) stop("comparison must be provided when data is a character vector.")
  if (!is.character(comparison)) stop("comparison must be a character vector.")
  if (length(comparison) != length(data)) stop("comparison must have the same length as data.")
}

.check_comparison_col <- function(data) {
  # Separate from .check_comparison_args because df resume sessions don't re-pass the vector.
  if (!"comparison" %in% names(data)) stop("data frame must contain a comparison column.")
}

.check_comparison_context <- function(pre_comparison, post_comparison, n_rows) {
  # pre/post_comparison are optional; validate length only when the caller supplies them.
  if (!is.null(pre_comparison) && length(pre_comparison) != n_rows) {
    stop("pre_comparison must have the same length as data.")
  }
  if (!is.null(post_comparison) && length(post_comparison) != n_rows) {
    stop("post_comparison must have the same length as data.")
  }
}

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
# Data Preparation                                                             #
# ---------------------------------------------------------------------------- #
# Normalizes input data, applies start/randomize rules, inits annotation state #
# ============================================================================ #

#' @noRd
.prepare_data <- function(data, start, randomize, context, pre, post, extra_exclude = character(0)) {
  # Returns list(data, original_data, start_val, class_cols). data may be filtered/reordered by
  # start and randomize rules; original_data is preserved at full row count for save-back merge.
  .check_data_first_col(data)
  if (!isFALSE(context)) {
    # A resumed session carries the prior save's pre/post columns; restore them to the working
    # before/after names instead of regenerating, so they aren't duplicated on the next save.
    if ("pre" %in% names(data) || "post" %in% names(data)) {
      if ("pre" %in% names(data)) {
        data$before <- data$pre
        data$pre <- NULL
      }
      if ("post" %in% names(data)) {
        data$after <- data$post
        data$post <- NULL
      }
    } else if (!("before" %in% names(data)) && !("after" %in% names(data))) {
      # Neighboring rows fill both channels only when the caller supplies neither vector.
      # Supplying one side means only that side is wanted: the other column is left out
      # entirely, and the UI renders nothing for a column that isn't there.
      if (is.null(pre) && is.null(post)) {
        data$before <- c("", data$texts[-nrow(data)])
        data$after <- c(data$texts[-1], "")
      } else {
        if (!is.null(pre)) data$before <- pre
        if (!is.null(post)) data$after <- post
      }
    }
  }
  data$id <- seq_len(nrow(data))
  original_data <- data
  class_cols <- setdiff(names(data), c("texts", "id", "before", "after", "notes", extra_exclude))
  start_val <- 1L
  if (is.numeric(start)) {
    # Numeric start is treated as an explicit operator override.
    start_val <- as.integer(start)
  } else if (start == "first_empty" && length(class_cols) > 0) {
    # first_empty resumes from the first row with no completed classifications.
    empty_rows <- which(apply(data[, class_cols, drop = FALSE], 1, function(x) all(x == "" | is.na(x))))
    if (length(empty_rows) > 0) start_val <- empty_rows[1]
  } else if (start == "all_empty" && length(class_cols) > 0) {
    # all_empty filters the workload to uncoded rows only and restarts progress at row one.
    empty_rows <- which(apply(data[, class_cols, drop = FALSE], 1, function(x) all(x == "" | is.na(x))))
    if (length(empty_rows) > 0) {
      data <- data[empty_rows, ]
      start_val <- 1L
    }
  }
  if (randomize && length(class_cols) > 0) {
    # Randomization only applies to uncoded rows to avoid re-reviewing completed work.
    empty_rows <- which(apply(data[, class_cols, drop = FALSE], 1, function(x) all(x == "" | is.na(x))))
    if (length(empty_rows) > 0) data <- data[sample(empty_rows), ]
    start_val <- 1L
  }
  list(data = data, original_data = original_data, start_val = start_val, class_cols = class_cols)
}

.init_annotations <- function(app_data) {
  # Annotation buffers are prefilled to guarantee stable indexing during navigation updates.
  annotations <- list()
  for (var_name in names(app_data$classifications)) {
    annotations[[var_name]] <- if (var_name %in% names(app_data$data)) {
      as.character(app_data$data[[var_name]])
    } else {
      rep("", nrow(app_data$data))
    }
  }
  annotations
}

# ============================================================================ #
# Shiny Server Setup                                                           #
# ---------------------------------------------------------------------------- #
# Registers outputs, observers, and save handlers shared across app modes      #
# ============================================================================ #

.init_server_values <- function(app_data) {
  # Bundles all mutable session state into one reactiveValues store shared by every observer.
  shiny::reactiveValues(
    counter       = as.integer(app_data$start_val),
    data          = app_data$data,
    original_data = app_data$original_data,
    annotations   = .init_annotations(app_data),
    notes         = if ("notes" %in% names(app_data$data)) as.character(app_data$data$notes) else rep("", nrow(app_data$data)),
    show_context  = isTRUE(app_data$context)
  )
}

.setup_common_outputs <- function(input, output, session, values, app_data) {
  # Progress text and bar share one counter source to avoid inconsistent UI progress states.
  output$progress_text <- shiny::renderText({
    sprintf("Row %d of %d", as.integer(values$counter), as.integer(nrow(values$data)))
  })
  shiny::observe({
    progress <- round((as.integer(values$counter) / as.integer(nrow(values$data))) * 100)
    shinyWidgets::updateProgressBar(session, "progress_bar", value = progress)
  })
  output$current_text <- shiny::renderUI({
    shiny::HTML(as.character(values$data$texts[values$counter]))
  })
  output$context_toggle <- shiny::renderUI({
    if (identical(app_data$context, "FLEX")) {
      shiny::checkboxInput("show_context_check", "Show context", value = values$show_context)
    }
  })
  shiny::observeEvent(input$show_context_check, {
    values$show_context <- input$show_context_check
  })
  output$context_before <- shiny::renderUI({
    if (values$show_context && values$counter > 1 && "before" %in% names(values$data)) {
      shiny::div(
        class = "context-text", style = "color: #94a3b8; margin-bottom: 8px;",
        shiny::HTML(as.character(values$data$before[values$counter]))
      )
    }
  })
  output$context_after <- shiny::renderUI({
    if (values$show_context && values$counter < nrow(values$data) && "after" %in% names(values$data)) {
      shiny::div(
        class = "context-text", style = "color: #94a3b8; margin-top: 8px;",
        shiny::HTML(as.character(values$data$after[values$counter]))
      )
    }
  })
}

.setup_nav_handler <- function(input, session, values, save_function, refresh_function) {
  # save_function() fires unconditionally on every navigation event, even at boundary (no-op row move).
  # Pending edits flush before the row index changes, so no annotation is silently lost.
  # The notes textarea is static UI (not rendered reactively like the radio/button panels), so it only
  # picks up a stored value via refresh_function(). Fire it once after the first flush so a resumed
  # session shows the existing note for the start row instead of an empty box. The onFlushed callback
  # runs outside a reactive context, so reads of values$ inside refresh_function() must be isolated.
  session$onFlushed(function() shiny::isolate(refresh_function()), once = TRUE)
  shiny::observeEvent(input$prev, {
    save_function()
    if (values$counter > 1) {
      values$counter <- values$counter - 1
      refresh_function()
    }
  })
  shiny::observeEvent(input[["next"]], {
    save_function()
    if (values$counter < nrow(values$data)) {
      values$counter <- values$counter + 1
      refresh_function()
    }
  })
}

.setup_snapshot_handler <- function(input, app_data, save_function) {
  # The Save Snapshot button is only rendered when snapshot_dir names a directory (app_data$save_loc set),
  # so this observer never fires without a save location.
  shiny::observeEvent(input$save_snapshot, {
    # Snapshot captures current progress without ending the annotation session.
    annotated <- save_function()
    # Timestamp naming keeps snapshots sortable and avoids overwriting prior checkpoints.
    snapshot_file <- file.path(
      app_data$save_loc$dir,
      paste0(app_data$save_loc$prefix, "_snapshot_", as.integer(Sys.time()), ".RData")
    )
    # assign() places the variable in this function's local env so save() can locate it by name.
    assign(app_data$save_loc$prefix, annotated, envir = environment())
    tryCatch(
      {
        save(list = app_data$save_loc$prefix, file = snapshot_file, envir = environment())
        shiny::showNotification(paste("Snapshot saved:", snapshot_file), type = "message", duration = 3)
      },
      error = function(e) shiny::showNotification(paste("Snapshot failed:", e$message), type = "error")
    )
  })
}

.setup_save_handler <- function(input, session, values, app_data, save_function, extra_cleanup_function = NULL) {
  # Two exit paths, both returning the annotated data to the R session via stopApp(): Save&Exit sets
  # intentional_close and shows a confirmation modal; browser close / kill / disconnect leaves
  # intentional_close = FALSE so onSessionEnded performs the same return. This is the safety net for
  # the whole annotation session, so in-progress work is never lost regardless of how the app closes.
  close_state <- shiny::reactiveValues(intentional_close = FALSE)

  do_save <- function() {
    save_function()
    .gen_output(
      original_data          = values$original_data,
      current_ids            = values$data$id,
      annotations            = values$annotations,
      notes                  = if (app_data$notes) values$notes else NULL,
      extra_cleanup_function = extra_cleanup_function
    )
  }

  .setup_snapshot_handler(input, app_data, do_save)

  shiny::observeEvent(input$save_exit, {
    close_state$intentional_close <- TRUE
    annotated <- do_save()
    shinyjs::runjs("document.head.insertAdjacentHTML('beforeend', '<style>#shiny-disconnected-overlay{display:none!important}</style>');")
    # Show dialog when save and exit is triggered to provide feedback that data was saved before the app closes.
    shiny::showModal(shiny::modalDialog(
      shiny::div(
        style = "min-height: 150px;",
        shiny::tags$small(
          style = "color:#64748b;",
          shiny::p("Your data was returned to the R workspace."),
          .citation()
        )
      ),
      title = "Data saved",
      footer = NULL,
      easyClose = FALSE,
      size = "l"
    ))
    shinyjs::delay(2500, shiny::stopApp(annotated))
  })

  session$onSessionEnded(function() {
    shiny::isolate({
      if (!close_state$intentional_close) {
        # Even without Save & Exit, the work is always returned to the R session via stopApp().
        shiny::stopApp(do_save())
      }
    })
  })
}

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
# Data Helpers                                                                 #
# ---------------------------------------------------------------------------- #
# Builds output data frame and converts character input to annotation schema   #
# ============================================================================ #

#' @noRd
.gen_output <- function(original_data, current_ids, annotations, notes = NULL,
                        extra_cleanup_function = NULL) {
  # Merges annotation buffers back into the full original dataset by id, so subsetting from
  # start = "all_empty" or randomize = TRUE never drops rows the user did not see.
  # extra_cleanup_function lets mode-specific runtime columns (e.g. comparison context) be
  # stripped or renamed before the annotated frame is returned to the caller.
  annotated <- original_data
  idx_map <- match(current_ids, annotated$id)
  valid_idx <- !is.na(idx_map)
  if (any(!valid_idx)) warning(sum(!valid_idx), " row ID(s) in current session not found in original data and were skipped.")

  for (annotation_name in names(annotations)) {
    if (annotation_name %in% names(annotated)) {
      annotated[[annotation_name]][idx_map[valid_idx]] <- annotations[[annotation_name]][valid_idx]
    }
  }
  if (!is.null(notes)) {
    notes_col <- if ("notes" %in% names(annotated)) as.character(annotated$notes) else rep("", nrow(annotated))
    notes_col[idx_map[valid_idx]] <- notes[valid_idx]
    annotated$notes <- notes_col
  }
  annotated$id <- NULL
  if ("before" %in% names(annotated)) names(annotated)[names(annotated) == "before"] <- "pre"
  if ("after" %in% names(annotated)) names(annotated)[names(annotated) == "after"] <- "post"
  if (!is.null(extra_cleanup_function)) annotated <- extra_cleanup_function(annotated)
  annotated
}

#' @noRd
.character_to_data <- function(data, arg_list, missing, comparison = NULL) {
  # Promotes a raw character vector to the annotation data-frame schema. Each classification
  # becomes a factor column with empty-string + missing sentinels + caller-supplied levels.
  # arg_list is guaranteed fully named by .check_arg_names() upstream.
  df <- data.frame(texts = data, stringsAsFactors = FALSE)
  if (!is.null(comparison)) df$comparison <- comparison
  for (i in seq_along(arg_list)) {
    df[[names(arg_list)[i]]] <- factor("", levels = c("", .format_NA(missing), arg_list[[i]]))
  }
  df
}

.relevel_data_factors <- function(data, arg_list, missing) {
  # Existing sessions can be extended: factor levels are additive — old levels remain valid,
  # new levels are appended. This prevents data loss when category lists change between calls.
  if (!is.data.frame(data) || length(arg_list) == 0) {
    return(data)
  }
  new_sentinels <- .format_NA(missing)
  for (i in seq_along(arg_list)) {
    var_name <- names(arg_list)[i]
    if (var_name %in% names(data)) {
      current_levels <- levels(data[[var_name]])
      # When resuming with a different missing argument, the new sentinel may not exist among
      # existing levels. Old missing values keep their previous label silently otherwise.
      if (length(current_levels) > 0) {
        missing_in_old <- new_sentinels[!new_sentinels %in% current_levels]
        if (length(missing_in_old) > 0) {
          warning(
            "Existing factor levels in '", var_name,
            "' do not contain new missing sentinel(s): ",
            paste(missing_in_old, collapse = ", "),
            ". Previously coded missing values keep their old label."
          )
        }
      }
      new_levels <- c("", new_sentinels, arg_list[[i]])
      all_levels <- unique(c(current_levels, new_levels))
      data[[var_name]] <- factor(as.character(data[[var_name]]), levels = all_levels)
    } else {
      # New variables start uncoded across all rows to avoid implicit assumptions.
      data[[var_name]] <- factor("", levels = c("", new_sentinels, arg_list[[i]]))
    }
  }
  data
}

.init_comparison_context <- function(data, context, pre_comparison, post_comparison) {
  # Comparison-side context columns mirror the primary text context: caller-supplied pre/post
  # take precedence, otherwise neighbouring rows fill before/after channels.
  n_rows <- nrow(data)
  .check_comparison_context(pre_comparison, post_comparison, n_rows)
  if (!isFALSE(context)) {
    # A resumed session carries the prior save's pre_comparison/post_comparison columns; restore
    # them to the working before_comparison/after_comparison names instead of regenerating, so
    # they aren't duplicated on the next save.
    if ("pre_comparison" %in% names(data) || "post_comparison" %in% names(data)) {
      if ("pre_comparison" %in% names(data)) {
        data$before_comparison <- data$pre_comparison
        data$pre_comparison <- NULL
      }
      if ("post_comparison" %in% names(data)) {
        data$after_comparison <- data$post_comparison
        data$post_comparison <- NULL
      }
    } else if (!("before_comparison" %in% names(data)) && !("after_comparison" %in% names(data))) {
      # Same one-sided rule as .prepare_data(): neighbors only when neither vector is given.
      if (is.null(pre_comparison) && is.null(post_comparison)) {
        data$before_comparison <- c("", data$comparison[-nrow(data)])
        data$after_comparison <- c(data$comparison[-1], "")
      } else {
        if (!is.null(pre_comparison)) data$before_comparison <- pre_comparison
        if (!is.null(post_comparison)) data$after_comparison <- post_comparison
      }
    }
  }
  data
}

.rename_comparison_context_columns <- function(df) {
  # Comparison context columns surface in output under the pre_comparison/post_comparison
  # argument names, mirroring how .gen_output renames before/after to pre/post.
  if ("before_comparison" %in% names(df)) names(df)[names(df) == "before_comparison"] <- "pre_comparison"
  if ("after_comparison" %in% names(df)) names(df)[names(df) == "after_comparison"] <- "post_comparison"
  df
}

#' @noRd
.prepare_app_data <- function(data, arg_list, missing, has_comparison, comparison,
                              start, randomize, context, pre, post,
                              pre_comparison, post_comparison) {
  # Shared prep path for both entry points: promote -> validate -> mutate -> prepare.
  # .check_common_params() must stay ahead of .init_comparison_context(), which branches on
  # context and writes columns, so context is never acted on before it has been validated.
  .check_arg_names(arg_list)

  # Supplying any pre/post vector means the caller wants context, so switch it on instead of
  # silently dropping the vectors. "FLEX" keeps priority: it is already a context-on mode.
  # Resolved here, ahead of validation, and returned so callers store the effective value.
  if (isFALSE(context) && (!is.null(pre) || !is.null(post) ||
    !is.null(pre_comparison) || !is.null(post_comparison))) {
    context <- TRUE
  }

  # Promote raw character vector to the schema-bearing annotation data frame.
  if (is.character(data)) {
    data <- .character_to_data(data, arg_list, missing,
      comparison = if (has_comparison) comparison else NULL
    )
  }

  data <- .relevel_data_factors(data, arg_list, missing)

  if (has_comparison) .check_comparison_col(data)

  .check_common_params(data, start, randomize, context, pre, post)

  if (has_comparison) {
    data <- .init_comparison_context(data, context, pre_comparison, post_comparison)
  }

  # Comparison columns are excluded from class-column detection so they aren't treated as
  # annotation targets by start/randomize logic in .prepare_data().
  prepared_data <- if (has_comparison) {
    .prepare_data(data, start, randomize, context, pre, post,
      extra_exclude = c("comparison", "before_comparison", "after_comparison")
    )
  } else {
    .prepare_data(data, start, randomize, context, pre, post)
  }

  # Reconstruct the per-variable choice list from factor levels, stripping the empty-string
  # placeholder and the _missing_ sentinels so only real categories are surfaced to the UI.
  factor_levels <- list()
  for (col in prepared_data$class_cols) {
    if (is.factor(prepared_data$data[[col]])) {
      lvls <- levels(prepared_data$data[[col]])
      lvls <- lvls[!grepl("^_.*_$|^$", lvls)]
      if (length(lvls) > 0) factor_levels[[col]] <- lvls
    }
  }

  list(prepared_data = prepared_data, factor_levels = factor_levels, context = context)
}

# ============================================================================ #
# App Shell Builder                                                            #
# ---------------------------------------------------------------------------- #
# Shared fluidPage wrapper consumed by all three build_X_ui functions          #
# ============================================================================ #

.build_resize_script <- function() {
  # The drag handle writes an inline height; making the hight persistent across cases and sessions.
  shiny::tags$script(shiny::HTML(paste0(
    "$(function() {",
    "  var box = document.querySelector('.text-display');",
    "  if (!box || !window.ResizeObserver) return;",
    "  var stored = localStorage.getItem('handcodeR_text_height');",
    "  if (stored) box.style.height = stored;",
    # Guarding on style.height keeps unrelated reflows (window resize, context toggle) from
    # overwriting the stored value with an empty string.
    "  new ResizeObserver(function() {",
    "    if (box.style.height) localStorage.setItem('handcodeR_text_height', box.style.height);",
    "  }).observe(box);",
    "});"
  )))
}

.build_app_shell <- function(app_data, title, body_slot, panels_slot,
                             extra_styles = NULL, keyboard_script = NULL) {
  # Default script guards via hcInField() so Space/Enter don't fire while typing in notes/inputs.
  default_script <- paste0(
    "$(document).on('keyup', function(e) {",
    " if (hcInField()) return;",
    " if (e.key === ' ') { e.preventDefault(); $('#prev').click(); }",
    " if (e.key === 'Enter') $('#next').click();",
    "});"
  )
  shiny::fluidPage(
    theme = bslib::bs_theme(version = 5, primary = "#2563eb"),
    shinyjs::useShinyjs(),
    shiny::tags$head(.common_styles(), extra_styles),
    shiny::div(
      class = "app-container",
      shiny::div(
        class = "d-flex justify-content-between align-items-center mb-4",
        shiny::h2(title),
        shiny::span(shiny::textOutput("progress_text"), class = "text-muted")
      ),
      body_slot,
      shiny::div(
        class = "classifications-container",
        panels_slot
      ),
      shiny::div(
        class = "d-flex justify-content-between align-items-center mb-3",
        shiny::actionButton("prev", "Previous", class = "btn btn-outline-primary"),
        shiny::div(
          style = "flex: 1; margin: 0 20px;",
          shinyWidgets::progressBar(id = "progress_bar", value = 0, display_pct = TRUE)
        ),
        shiny::actionButton("next", "Next", class = "btn btn-primary")
      ),
      shiny::div(
        class = "save-button-container",
        if (!is.null(app_data$save_loc)) shiny::actionButton("save_snapshot", "Save Snapshot", class = "btn btn-warning"),
        shiny::actionButton("save_exit", "Save and Exit", class = "btn btn-success")
      ),
      if (app_data$notes) {
        shiny::div(
          style = "margin-top: 20px;",
          shiny::tags$label("Notes", style = "font-weight: 600; color: #1e293b;"),
          shiny::textAreaInput("note_text",
            label = NULL, value = "", width = "100%", rows = 3,
            placeholder = "Add a note for this entry..."
          )
        )
      },
      shiny::uiOutput("context_toggle")
    ),
    # Shared focus helpers used by every keyboard script below.
    # hcInField(): true only where a keystroke has its own meaning (typing, dropdown navigation).
    # Radios and checkboxes are excluded on purpose - clicking one with the mouse leaves it as
    # document.activeElement, and the shortcuts have to keep working afterwards.
    # Click blur: a clicked <button> keeps DOM focus, so Space/Enter would re-activate it via the
    # browser default on top of the #prev/#next shortcut. e.detail > 0 limits this to real mouse
    # clicks; jQuery-triggered .click() (quickcode, numeric keys) leaves keyboard focus alone.
    shiny::tags$script(shiny::HTML(paste0(
      "window.hcInField = function() { return $(document.activeElement).is('",
      "textarea, select, input:not([type=radio]):not([type=checkbox]):not([type=button]):not([type=submit])",
      "'); };",
      "$(document).on('click', 'button', function(e) { if (e.detail > 0) this.blur(); });"
    ))),
    .build_resize_script(),
    shiny::tags$script(shiny::HTML(if (!is.null(keyboard_script)) keyboard_script else default_script))
  )
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
#'   directory must already exist.
#' @param notes Logical. If \code{TRUE}, show a free-text notes input
#'   in the UI. Default \code{FALSE}.
#' @param keyboard_shortcuts Logical. If \code{TRUE}, keys \code{1}-\code{9}
#'   cycle through the radio choices of the variable at that position
#'   (at most 9 classification variables). Default \code{FALSE}.
#'
#' @return A data frame containing the original texts plus one column per
#'   classification variable. Closing the app (Save & Exit or otherwise)
#'   returns the annotated data to the calling R session.
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

.setup_comparison_outputs <- function(output, values) {
  # Renders the second text channel and its before/after context. Shared by categorial-comparison
  # and quickcode-comparison servers since the two channels behave identically across modes.
  output$comparison_text <- shiny::renderUI({
    shiny::HTML(as.character(values$data$comparison[values$counter]))
  })
  output$context_before_comparison <- shiny::renderUI({
    if (values$show_context && values$counter > 1 && "before_comparison" %in% names(values$data)) {
      shiny::div(
        class = "context-text", style = "color: #94a3b8; margin-bottom: 8px;",
        shiny::HTML(as.character(values$data$before_comparison[values$counter]))
      )
    }
  })
  output$context_after_comparison <- shiny::renderUI({
    if (values$show_context && values$counter < nrow(values$data) && "after_comparison" %in% names(values$data)) {
      shiny::div(
        class = "context-text", style = "color: #94a3b8; margin-top: 8px;",
        shiny::HTML(as.character(values$data$after_comparison[values$counter]))
      )
    }
  })
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
#'   directory must already exist.
#' @param notes Logical. If \code{TRUE}, show a free-text notes input
#'   in the UI. Default \code{FALSE}.
#' @param advance_delay Seconds to wait after a selection before the app
#'   advances to the next row. A single non-negative number; \code{0}
#'   advances immediately. Default \code{0.7}.
#'
#' @return A data frame containing the original texts plus one factor
#'   column holding the selected level per row. Closing the app (Save &
#'   Exit or otherwise) returns the annotated data to the calling R
#'   session.
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

#' Binary Text Annotation (deprecated)
#'
#' Deprecated alias kept so existing scripts keep running. Forwards to
#' \code{\link{quickcode}}, which replaces it. The arguments
#' \code{quickcode}, \code{multifactorial}, \code{keyboard_shortcuts} and
#' \code{colors} no longer exist and are rejected with an error.
#'
#' @param data Passed through to \code{\link{quickcode}}.
#' @param ... Passed through to \code{\link{quickcode}}.
#' @param quickcode Removed. Quickcoding is the default behaviour now.
#' @param multifactorial Removed. Exactly one variable is coded.
#' @param keyboard_shortcuts Removed. Number keys are always active.
#' @param colors Removed. All buttons share one neutral style.
#'
#' @return See \code{\link{quickcode}}.
#'
#' @examples
#' \dontrun{
#' result <- handcode_binary(c("a", "b"), sentiment = c("positive", "negative"))
#' }
#'
#' @seealso \code{\link{quickcode}}, which supersedes this function.
#' @export
handcode_binary <- function(data, ..., quickcode, multifactorial,
                            keyboard_shortcuts, colors) {
  # The removed arguments are named formals rather than swallowed by ... so a stale call fails
  # loudly on the exact argument instead of silently coding under different semantics.
  .Deprecated("quickcode")
  if (!missing(quickcode)) stop("quickcode is no longer an argument; quickcoding is the default behaviour of quickcode().")
  if (!missing(multifactorial)) stop("multifactorial is no longer supported; quickcode() codes exactly one variable.")
  if (!missing(keyboard_shortcuts)) stop("keyboard_shortcuts is no longer supported; number keys are always active in quickcode().")
  if (!missing(colors)) stop("colors is no longer supported; all quickcode() buttons share one neutral style.")
  # Namespace-qualified because the removed `quickcode` formal shadows the function name here:
  # an unqualified call would force that missing promise instead of finding the function.
  handcodeR::quickcode(data, ...)
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
              label   = app_data$missing[1],
              class   = paste(
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
