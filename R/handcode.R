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
# Quicksave Setup                                                              #
# ---------------------------------------------------------------------------- #
# Resolves the quicksave argument into a save location for the Quicksave button #
# ============================================================================ #

# CRAN policy forbids writing to user filespace without explicit user direction.
# The quicksave argument doubles as that direction: NULL disables it, a directory path enables it
# and names the target. The filename prefix is derived from the data variable name and sanitized
# for filesystem use. A bare TRUE carries no location and is therefore rejected. FALSE stays
# accepted as a synonym for NULL so calls written against earlier versions keep working.

.quicksave_setup <- function(quicksave, default_name) {
  if (is.null(quicksave) || isFALSE(quicksave)) {
    return(NULL)
  }
  if (!is.character(quicksave) || length(quicksave) != 1 || !nzchar(trimws(quicksave))) {
    stop("quicksave must be NULL or a path to an existing directory.")
  }
  dir_out <- normalizePath(trimws(quicksave), mustWork = FALSE)
  if (!dir.exists(dir_out)) {
    stop(sprintf("quicksave path does not exist: '%s'", dir_out))
  }
  list(dir = dir_out, prefix = .sanitize_id(default_name))
}

# ============================================================================ #
# Styles & Colors                                                              #
# ---------------------------------------------------------------------------- #
# CSS builders and hex color utilities shared across all app modes             #
# ============================================================================ #

.common_styles <- function() {
  # Common styles centralize shared layout tokens so all app modes keep one visual baseline.
  shiny::tags$style(shiny::HTML("
    .app-container { padding: 20px 2.5%; }
    .text-display { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 12px; padding: 24px; margin-bottom: 20px; min-height: calc(14.08rem + 10px); display: flex; flex-direction: column; }
    .current-text { font-size: 1.1rem; line-height: 1.6; color: #1e293b; }
    .context-text { color: #94a3b8; font-size: 0.95rem; }
    .classifications-container { display: flex; gap: 16px; margin-bottom: 20px; flex-wrap: wrap; }
    .classification-card { flex: 1 1 0px; min-width: 200px; background: white; border: 1px solid #e2e8f0; border-radius: 12px; padding: 16px; }
    .classification-card h5 { color: #1e293b; font-weight: 600; margin-bottom: 10px; border-bottom: 2px solid #e2e8f0; padding-bottom: 6px; }
    .save-button-container { display: flex; justify-content: center; gap: 12px; margin-top: 16px; }
  "))
}

.binary_styles <- function() {
  # Left/right colors are fixed; the colors argument was removed as unneeded configurability.
  shiny::tags$style(shiny::HTML("
    .binary-button { display: inline-block; width: 45%; margin: 2%; padding: 20px; text-align: center; border-radius: 8px; cursor: pointer; font-size: 1.2rem; transition: all 0.08s ease; }
    .binary-left { border: 2px solid #10b981; background-color: #e2f6ef; color: #10b981; }
    .binary-left:hover { background-color: #10b981; color: white; }
    .binary-right { border: 2px solid #dc2626; background-color: #fae4e4; color: #dc2626; }
    .binary-right:hover { background-color: #dc2626; color: white; }
    .binary-left.selected { background-color: #0a7853 !important; color: white !important; border-color: #0a7853 !important; }
    .binary-right.selected { background-color: #8f1818 !important; color: white !important; border-color: #8f1818 !important; }
    .missing-button { width: 96%; margin: 2%; padding: 10px; text-align: center; border: 2px solid #cbd5e1; border-radius: 8px; cursor: pointer; font-size: 1rem; background-color: #f1f5f9; color: #64748b; transition: all 0.08s ease; }
    .missing-button:hover { background-color: #cbd5e1; color: #334155; }
    .missing-button.selected { background-color: #64748b; color: white; }
    .quickcode-card { width: 100%; box-sizing: border-box; }
    .quickcode-button-group { display: flex; flex-direction: row; width: 100%; gap: 2%; }
    .quickcode-button-group .binary-button { width: 32%; margin: 0; }
    .quickcode-button-group .missing-button { display: inline-block; width: 32%; margin: 0; padding: 20px; font-size: 1.2rem; box-sizing: border-box; }
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
  # pre/post are only used when context is on; reject the silent-drop case so the caller sets context.
  if (isFALSE(context) && (!is.null(pre) || !is.null(post))) {
    stop("context must be TRUE or \"FLEX\" when pre/post is supplied.")
  }
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

.check_comparison_context <- function(pre_comparison, post_comparison, n_rows, context) {
  # pre/post_comparison are only used when context is on; reject the silent-drop case like pre/post.
  if (isFALSE(context) && (!is.null(pre_comparison) || !is.null(post_comparison))) {
    stop("context must be TRUE or \"FLEX\" when pre_comparison/post_comparison is supplied.")
  }
  # pre/post_comparison are optional; validate length only when the caller supplies them.
  if (!is.null(pre_comparison) && length(pre_comparison) != n_rows) {
    stop("pre_comparison must have the same length as data.")
  }
  if (!is.null(post_comparison) && length(post_comparison) != n_rows) {
    stop("post_comparison must have the same length as data.")
  }
}

.check_bin_session <- function(interactive_mode, data) {
  if (!interactive_mode) stop("handcode_binary() can only be used in an interactive R session.")
  if (!is.data.frame(data) && !is.character(data)) {
    stop("data must be a character vector or data frame from a previous handcode_binary() session.")
  }
}

.check_binary_args <- function(arg_list) {
  if (length(arg_list) < 1) stop("at least one binary classification variable must be provided.")
  # Exactly 2 values enforced because UI hardcodes left/right button semantics per variable.
  if (!all(vapply(arg_list, function(x) is.character(x) && length(x) == 2, logical(1)))) {
    stop("all binary classification arguments must be character vectors with exactly two values.")
  }
  if (any(vapply(arg_list, function(x) "" %in% x, logical(1)))) {
    stop("empty strings are not allowed as binary values.")
  }
}

.check_binary_params <- function(missing, multifactorial, enable_numeric, arg_list, quickcode) {
  # Binary mode uses one shared missing button per variable, so only a single missing label is valid.
  if (length(missing) != 1) stop("missing argument must be a single value in binary annotation.")
  if (!is.logical(multifactorial) || length(multifactorial) != 1) stop("multifactorial must be a single logical value.")
  if (!is.logical(enable_numeric) || length(enable_numeric) != 1) stop("enable_numeric must be a single logical value.")
  # Keys 1–9 map to variables by position; more than 9 would exceed the available key range.
  if (enable_numeric && length(arg_list) > 9) stop("enable_numeric = TRUE supports at most 9 classification variables.")
  if (!is.logical(quickcode) || length(quickcode) != 1) stop("quickcode must be a single logical value.")
  if (quickcode && enable_numeric) stop("quickcode = TRUE and enable_numeric = TRUE are mutually exclusive.")
  if (quickcode && !multifactorial) stop("quickcode = TRUE and multifactorial = FALSE are mutually exclusive.")
  if (quickcode && length(arg_list) > 1) stop("quickcode = TRUE supports at most 1 classification variable.")
}

.check_cat_numeric_param <- function(enable_numeric, arg_list) {
  if (!is.logical(enable_numeric) || length(enable_numeric) != 1) {
    stop("enable_numeric must be a single logical value.")
  }
  # Keys 1–9 map to variables by position; more than 9 would exceed the available key range.
  if (enable_numeric && length(arg_list) > 9) {
    stop("enable_numeric = TRUE supports at most 9 classification variables.")
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
      # Caller-provided context takes precedence; otherwise context is generated from neighboring rows.
      if (!is.null(pre) && !is.null(post)) {
        data$before <- pre
        data$after <- post
      } else {
        data$before <- c("", data$texts[-nrow(data)])
        data$after <- c(data$texts[-1], "")
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

.setup_quicksave_handler <- function(input, app_data, save_function) {
  # The Quicksave button is only rendered when quicksave names a directory (app_data$save_loc set),
  # so this observer never fires without a save location.
  shiny::observeEvent(input$quicksave, {
    # Quicksave captures current progress without ending the annotation session.
    annotated <- save_function()
    # Timestamp naming keeps snapshots sortable and avoids overwriting prior checkpoints.
    quicksave_file <- file.path(
      app_data$save_loc$dir,
      paste0(app_data$save_loc$prefix, "_quicksave_", as.integer(Sys.time()), ".RData")
    )
    # assign() places the variable in this function's local env so save() can locate it by name.
    assign(app_data$save_loc$prefix, annotated, envir = environment())
    tryCatch(
      {
        save(list = app_data$save_loc$prefix, file = quicksave_file, envir = environment())
        shiny::showNotification(paste("Quicksaved:", quicksave_file), type = "message", duration = 3)
      },
      error = function(e) shiny::showNotification(paste("Quicksave failed:", e$message), type = "error")
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

  .setup_quicksave_handler(input, app_data, do_save)

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
      missing_choices <- setNames(
        .format_NA(app_data$missing),
        paste0("(", app_data$missing, ")")
      )
      var_label <- if (isTRUE(app_data$enable_numeric)) paste0(i, ". ", var_name) else var_name
      shiny::div(
        class = "classification-card",
        shiny::h5(var_label),
        shiny::radioButtons(
          inputId = paste0("class_", var_name),
          label = NULL,
          choices = c(" " = "", choices, missing_choices),
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
  .check_comparison_context(pre_comparison, post_comparison, n_rows, context)
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
      if (!is.null(pre_comparison) && !is.null(post_comparison)) {
        data$before_comparison <- pre_comparison
        data$after_comparison <- post_comparison
      } else {
        data$before_comparison <- c("", data$comparison[-nrow(data)])
        data$after_comparison <- c(data$comparison[-1], "")
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

  list(prepared_data = prepared_data, factor_levels = factor_levels)
}

# ============================================================================ #
# App Shell Builder                                                            #
# ---------------------------------------------------------------------------- #
# Shared fluidPage wrapper consumed by all three build_X_ui functions          #
# ============================================================================ #

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
        if (!is.null(app_data$save_loc)) shiny::actionButton("quicksave", "Quicksave", class = "btn btn-warning"),
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
#'   coding.
#' @param missing Character vector of labels for missing/not-applicable
#'   values. Default \code{c("Not applicable")}.
#' @param pre Optional character vector of texts to prepend as context
#'   (one per row). Requires \code{context = TRUE} or \code{"FLEX"}.
#' @param post Optional character vector of texts to append as context
#'   (one per row). Requires \code{context = TRUE} or \code{"FLEX"}.
#' @param comparison Optional character vector for paired-text comparison
#'   workflows.
#' @param pre_comparison Optional context-before vector for the comparison
#'   text.
#' @param post_comparison Optional context-after vector for the comparison
#'   text.
#' @param quicksave Either \code{NULL} (default; no Quicksave button is
#'   shown) or a character path to an existing directory. When a directory
#'   is given, a Quicksave button is shown that writes timestamped
#'   \code{<name>_quicksave_<timestamp>.RData} snapshots there. The
#'   directory must already exist.
#' @param notes Logical. If \code{TRUE}, show a free-text notes input
#'   in the UI. Default \code{FALSE}.
#' @param enable_numeric Logical. If \code{TRUE}, keys \code{1}-\code{9}
#'   cycle through the radio choices of the variable at that position
#'   (at most 9 classification variables). Default \code{FALSE}.
#'
#' @return A data frame containing the original texts plus one column per
#'   classification variable. Closing the app (Save & Exit or otherwise)
#'   returns the annotated data to the calling R session.
#'
#' @examples
#' \dontrun{
#'   texts <- c("I love this product", "Worst purchase ever", "It's okay")
#'   result <- handcode(
#'     texts,
#'     sentiment = c("positive", "neutral", "negative")
#'   )
#' }
#'
#' @seealso \code{\link{handcode_binary}} for two-choice annotation.
#' @export
handcode <- function(data, ..., start = "first_empty", randomize = FALSE,
                     context = FALSE, missing = c("Not applicable"),
                     pre = NULL, post = NULL,
                     comparison = NULL,
                     pre_comparison = NULL, post_comparison = NULL,
                     quicksave = NULL, notes = FALSE,
                     enable_numeric = FALSE) {
  arg_list <- list(...)
  original_name <- deparse(substitute(data))
  .check_cat_session(.interactive(), arg_list, data)

  # CRAN policy: writes to user filespace only when quicksave names an existing directory.
  # An invalid/non-existent path stops here with a clear message before the app launches.
  save_loc <- .quicksave_setup(quicksave, original_name)
  has_comparison <- !is.null(comparison) || (is.data.frame(data) && "comparison" %in% names(data))
  # A resumed session's notes column re-activates notes UI even if notes wasn't passed again.
  has_notes <- notes || (is.data.frame(data) && "notes" %in% names(data))

  # Char-vector path: validate ... category specs before promoting to a data frame.
  if (is.character(data)) {
    .check_cat_args(arg_list, missing)
    if (has_comparison) .check_comparison_args(comparison, data)
  }

  prep <- .prepare_app_data(data, arg_list, missing, has_comparison, comparison,
    start, randomize, context, pre, post, pre_comparison, post_comparison
  )
  prepared_data <- prep$prepared_data
  factor_levels <- prep$factor_levels

  .check_cat_numeric_param(enable_numeric, factor_levels)

  # app_data is the immutable runtime bundle passed into UI/server builders.
  app_data <- list(
    data            = prepared_data$data,
    original_data   = prepared_data$original_data,
    start_val       = prepared_data$start_val,
    context         = context,
    classifications = factor_levels,
    missing         = missing,
    original_name   = original_name,
    notes           = has_notes,
    save_loc        = save_loc,
    enable_numeric  = enable_numeric
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
  # fields. Sibling of .build_binary_keyboard_script(); cap of 9 matches the enable_numeric limit.
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
    keyboard_script = if (isTRUE(app_data$enable_numeric)) .build_cat_keyboard_script(app_data) else NULL
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
    keyboard_script = if (isTRUE(app_data$enable_numeric)) .build_cat_keyboard_script(app_data) else NULL
  )
}

.setup_comparison_outputs <- function(output, values) {
  # Renders the second text channel and its before/after context. Shared by categorial-comparison
  # and binary-comparison servers since the two channels behave identically across modes.
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
# Binary Mode                                                                  #
# ---------------------------------------------------------------------------- #
# Entry point, UI, server, and runner for two-choice binary annotation         #
# ============================================================================ #

#' Binary Text Annotation
#'
#' Launch a Shiny app for hand-coding texts into two-choice (left/right)
#' classification variables. Each variable is defined by name and a
#' length-2 character vector via \code{...}, where the first entry is
#' the left choice and the second is the right choice.
#'
#' @param data A character vector of texts, or a data frame with a
#'   \code{texts} column (and optionally pre-existing classification
#'   columns to resume coding).
#' @param ... Named length-2 character vectors defining binary
#'   classification variables. The first element labels the left choice,
#'   the second labels the right choice.
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
#'   coding.
#' @param missing Single label for missing/not-applicable values. Binary
#'   mode uses one shared missing button per variable, so exactly one
#'   label is allowed. Default \code{"Not applicable"}.
#' @param pre Optional character vector of texts to prepend as context
#'   (one per row). Requires \code{context = TRUE} or \code{"FLEX"}.
#' @param post Optional character vector of texts to append as context
#'   (one per row). Requires \code{context = TRUE} or \code{"FLEX"}.
#' @param comparison Optional character vector for paired-text comparison
#'   workflows.
#' @param pre_comparison Optional context-before vector for the comparison
#'   text.
#' @param post_comparison Optional context-after vector for the comparison
#'   text.
#' @param quicksave Either \code{NULL} (default; no Quicksave button is
#'   shown) or a character path to an existing directory. When a directory
#'   is given, a Quicksave button is shown that writes timestamped
#'   \code{<name>_quicksave_<timestamp>.RData} snapshots there. The
#'   directory must already exist. Not to be confused with
#'   \code{quickcode} below.
#' @param notes Logical. If \code{TRUE}, show a free-text notes input
#'   in the UI. Default \code{FALSE}.
#' @param enable_numeric Logical. If \code{TRUE}, keys \code{1}-\code{9}
#'   click the left button of the variable at that position (at most 9
#'   classification variables). Mutually exclusive with
#'   \code{quickcode = TRUE}. Default \code{FALSE}.
#' @param multifactorial Logical. If \code{TRUE} (default), each variable
#'   is coded independently. If \code{FALSE}, selecting the left value on
#'   one variable force-sets all other (non-missing) variables to their
#'   right value, enforcing a single positive-class assignment per row.
#' @param quickcode Logical. If \code{TRUE}, enable single-key quickcoding
#'   mode for one classification variable. Mutually exclusive with
#'   \code{enable_numeric = TRUE} and \code{multifactorial = FALSE}, and
#'   limited to a single \code{...} variable. Default \code{FALSE}.
#'
#' @return A data frame containing the original texts plus one column per
#'   binary classification variable. Closing the app (Save & Exit or
#'   otherwise) returns the annotated data to the calling R session.
#'
#' @examples
#' \dontrun{
#'   texts <- c("I love this product", "Worst purchase ever", "It's okay")
#'   result <- handcode_binary(
#'     texts,
#'     sentiment = c("positive", "negative")
#'   )
#' }
#'
#' @seealso \code{\link{handcode}} for multi-class categorial annotation.
#' @export
handcode_binary <- function(data, ..., start = "first_empty", randomize = FALSE,
                            context = FALSE, missing = c("Not applicable"),
                            pre = NULL, post = NULL,
                            comparison = NULL,
                            pre_comparison = NULL, post_comparison = NULL,
                            quicksave = NULL, notes = FALSE,
                            enable_numeric = FALSE,
                            multifactorial = TRUE,
                            quickcode = FALSE) {
  arg_list <- list(...)
  original_name <- deparse(substitute(data))
  .check_bin_session(.interactive(), data)

  # CRAN policy: writes to user filespace only when quicksave names an existing directory.
  # An invalid/non-existent path stops here with a clear message before the app launches.
  save_loc <- .quicksave_setup(quicksave, original_name)
  has_comparison <- !is.null(comparison) || (is.data.frame(data) && "comparison" %in% names(data))
  # A resumed session's notes column re-activates notes UI even if notes wasn't passed again.
  has_notes <- notes || (is.data.frame(data) && "notes" %in% names(data))

  # Char-vector path: validate that each ... entry is a length-2 character vector.
  if (is.character(data)) {
    .check_binary_args(arg_list)
    if (has_comparison) .check_comparison_args(comparison, data)
  }

  .check_binary_params(missing, multifactorial, enable_numeric, arg_list, quickcode)

  prep <- .prepare_app_data(data, arg_list, missing, has_comparison, comparison,
    start, randomize, context, pre, post, pre_comparison, post_comparison
  )
  prepared_data <- prep$prepared_data
  factor_levels <- prep$factor_levels

  # Binary-mode app_data carries extra UI flags: multifactorial, enable_numeric.
  app_data <- list(
    data            = prepared_data$data,
    original_data   = prepared_data$original_data,
    start_val       = prepared_data$start_val,
    context         = context,
    classifications = factor_levels,
    missing         = missing,
    original_name   = original_name,
    multifactorial  = multifactorial,
    enable_numeric  = enable_numeric,
    quickcode       = quickcode,
    notes           = has_notes,
    save_loc        = save_loc
  )

  # Execution delegates to binary app runtime after input normalization is complete.
  result <- if (has_comparison) {
    .run_binary_comparison_app(app_data)
  } else {
    .run_binary_app(app_data)
  }
  message("\nYour data was returned to the R workspace.\n\n", .citation())
  result
}

.build_binary_keyboard_script <- function(app_data) {
  # Keys 1-9 trigger the left-button click for the variable at that position. Only fires
  # outside form fields. Cap of 9 matches the enable_numeric length limit in handcode_binary().
  # quickcode binds 1/2/3 to left/right/missing of the single variable plus auto-advance.
  paste0(
    "$(document).on('keyup', function(e) {",
    " if (hcInField()) return;",
    " if (e.key === ' ') { e.preventDefault(); $('#prev').click(); }",
    " if (e.key === 'Enter') $('#next').click();",
    "});",
    if (app_data$quickcode) {
      safe_id_js <- .sanitize_id(names(app_data$classifications)[1])
      sprintf('
        $(document).on("keydown", function(e) {
          if (hcInField()) return;
          if (e.key === "1") {
            e.preventDefault();
            $("#btn_%s_1").click();
            setTimeout(function() { $("#next").click(); }, 700);
          } else if (e.key === "2") {
            e.preventDefault();
            $("#btn_%s_2").click();
            setTimeout(function() { $("#next").click(); }, 700);
          } else if (e.key === "3") {
            e.preventDefault();
            $("#btn_%s_missing").click();
            setTimeout(function() { $("#next").click(); }, 700);
          }
        });
      ', safe_id_js, safe_id_js, safe_id_js)
    } else if (app_data$enable_numeric) {
      var_names_js <- paste0('["', paste(sapply(names(app_data$classifications), .sanitize_id), collapse = '","'), '"]')
      sprintf('
        var numericVars = %s;
        $(document).on("keydown", function(e) {
          if (hcInField()) return;
          var num = parseInt(e.key);
          if (!isNaN(num) && num >= 1 && num <= numericVars.length) {
            e.preventDefault();
            $("#btn_" + numericVars[num - 1] + "_1").click();
          }
        });
      ', var_names_js)
    } else {
      ""
    }
  )
}

.build_binary_ui <- function(app_data) {
  body_slot <- shiny::div(
    class = "text-display",
    shiny::uiOutput("context_before"),
    shiny::div(class = "current-text", shiny::htmlOutput("current_text")),
    shiny::uiOutput("context_after")
  )
  .build_app_shell(app_data, "handcodeR - Binary", body_slot,
    shiny::uiOutput("binary_panels_container"),
    extra_styles = .binary_styles(),
    keyboard_script = .build_binary_keyboard_script(app_data)
  )
}

.setup_binary_panels <- function(output, values, app_data, comparison_layout = FALSE) {
  # Container rendering is split from panel rendering to keep variable-specific updates isolated.
  output$binary_panels_container <- shiny::renderUI({
    classifications <- app_data$classifications
    do.call(shiny::tagList, lapply(names(classifications), function(var_name) {
      shiny::uiOutput(paste0("panels_", .sanitize_id(var_name)))
    }))
  })
  # Each panel binds one variable to its left/right/missing control group.
  # local() creates a fresh scope per iteration so var_name/var_idx/safe_id are captured by
  # value inside each renderUI closure. Without it all panels close over the loop's final values.
  # comparison_layout = TRUE uses col-6/col-6 so btn_1 aligns with Statement, btn_2 with Comparison.
  classifications <- app_data$classifications
  for (i in seq_along(names(classifications))) {
    local({
      var_name <- names(classifications)[i]
      var_idx <- i
      safe_id <- .sanitize_id(var_name)
      output[[paste0("panels_", safe_id)]] <- shiny::renderUI({
        shiny::isolate({
          choices <- classifications[[var_name]]
          current_val <- .get_current_value(values, var_name, values$counter)
          panel_label <- if (app_data$enable_numeric) paste0(var_idx, ". ", var_name) else var_name
          button_group <- if (app_data$quickcode) {
            shiny::div(
              class = "quickcode-button-group",
              shiny::actionButton(
                inputId = paste0("btn_", safe_id, "_1"),
                label = paste0("1. ", choices[1]),
                class = paste("binary-button binary-left", if (current_val == choices[1]) "selected" else "")
              ),
              shiny::actionButton(
                inputId = paste0("btn_", safe_id, "_2"),
                label = paste0("2. ", choices[2]),
                class = paste("binary-button binary-right", if (current_val == choices[2]) "selected" else "")
              ),
              shiny::actionButton(
                inputId = paste0("btn_", safe_id, "_missing"),
                label = paste0("3. ", app_data$missing[1]),
                class = paste("missing-button", if (current_val == .format_NA(app_data$missing[1])) "selected" else "")
              )
            )
          } else if (comparison_layout) {
            shiny::div(
              class = "row",
              shiny::div(
                class = "col-6",
                shiny::actionButton(
                  paste0("btn_", safe_id, "_1"), choices[1],
                  class = paste(
                    "binary-button binary-left w-100",
                    if (current_val == choices[1]) "selected" else ""
                  )
                )
              ),
              shiny::div(
                class = "col-6",
                shiny::actionButton(
                  paste0("btn_", safe_id, "_2"), choices[2],
                  class = paste(
                    "binary-button binary-right w-100",
                    if (current_val == choices[2]) "selected" else ""
                  )
                )
              )
            )
          } else {
            shiny::div(
              class = "binary-button-group",
              shiny::actionButton(
                inputId = paste0("btn_", safe_id, "_1"),
                label = choices[1],
                class = paste("binary-button binary-left", if (current_val == choices[1]) "selected" else "")
              ),
              shiny::actionButton(
                inputId = paste0("btn_", safe_id, "_2"),
                label = choices[2],
                class = paste("binary-button binary-right", if (current_val == choices[2]) "selected" else "")
              )
            )
          }
          if (app_data$quickcode) {
            shiny::div(
              class = "classification-card quickcode-card",
              shiny::h5(panel_label),
              button_group
            )
          } else {
            shiny::div(
              class = "classification-card",
              shiny::h5(panel_label),
              button_group,
              shiny::actionButton(
                inputId = paste0("btn_", safe_id, "_missing"),
                label = "(missing)",
                class = paste(
                  if (comparison_layout) "missing-button w-100" else "missing-button",
                  if (current_val == .format_NA(app_data$missing[1])) "selected" else ""
                )
              )
            )
          }
        })
      })
    })
  }
}

.make_binary_handler <- function(input, session, values, app_data) {
  # Mirrors .make_categorial_handler(): registers observers, returns save_current + refresh_ui.
  # Both closures capture input/session/values/app_data via lexical scope.
  save_current <- function() {
    # Binary choices are written immediately by button events; this hook persists notes if enabled.
    if (app_data$notes && !is.null(input$note_text)) {
      values$notes[values$counter] <- input$note_text
    }
  }
  refresh_ui <- function() {
    # Button classes are recomputed on navigation so visual state always matches stored annotations.
    missing_val <- .format_NA(app_data$missing[1])
    for (var_name in names(app_data$classifications)) {
      safe_id <- .sanitize_id(var_name)
      choices <- app_data$classifications[[var_name]]
      current_val <- .get_current_value(values, var_name, values$counter)
      selected_id <- if (current_val == choices[1]) {
        paste0("btn_", safe_id, "_1")
      } else if (current_val == choices[2]) {
        paste0("btn_", safe_id, "_2")
      } else if (current_val == missing_val) {
        paste0("btn_", safe_id, "_missing")
      } else {
        ""
      }
      add_js <- if (nchar(selected_id) > 0) sprintf('$("#%s").addClass("selected");', selected_id) else ""
      shinyjs::runjs(sprintf(
        '$("#btn_%s_1, #btn_%s_2, #btn_%s_missing").removeClass("selected"); %s',
        safe_id, safe_id, safe_id, add_js
      ))
    }
    if (app_data$notes) {
      # Notes field follows row navigation to maintain per-row note continuity.
      shiny::updateTextAreaInput(session, "note_text", value = values$notes[values$counter])
    }
  }
  # Each variable gets three observers (left/right/missing). Each writes to values$annotations
  # and toggles CSS selected-class client-side without triggering a server re-render.
  # When multifactorial = FALSE, selecting left in one variable force-sets all others to right.
  lapply(names(app_data$classifications), function(var_name) {
    safe_id <- .sanitize_id(var_name)
    choices <- app_data$classifications[[var_name]]
    shiny::observeEvent(input[[paste0("btn_", safe_id, "_1")]], {
      values$annotations[[var_name]][values$counter] <- choices[1]
      # Only update CSS classes client side
      shinyjs::runjs(sprintf('
        $("#btn_%s_1, #btn_%s_2, #btn_%s_missing").removeClass("selected");
        $("#btn_%s_1").addClass("selected");
      ', safe_id, safe_id, safe_id, safe_id))
      if (!app_data$multifactorial) {
        # Non-multifactorial mode enforces one positive selection by setting all others to right-value.
        # Skip vars already marked as (missing) — their state was an explicit user decision.
        missing_sentinel <- .format_NA(app_data$missing[1])
        for (other_var in setdiff(names(app_data$classifications), var_name)) {
          if (values$annotations[[other_var]][values$counter] == missing_sentinel) next
          other_choices <- app_data$classifications[[other_var]]
          values$annotations[[other_var]][values$counter] <- other_choices[2]
          shinyjs::runjs(sprintf('
            $("#btn_%s_1, #btn_%s_2, #btn_%s_missing").removeClass("selected");
            $("#btn_%s_2").addClass("selected");
          ', .sanitize_id(other_var), .sanitize_id(other_var), .sanitize_id(other_var), .sanitize_id(other_var)))
        }
      }
    })
    shiny::observeEvent(input[[paste0("btn_", safe_id, "_2")]], {
      values$annotations[[var_name]][values$counter] <- choices[2]
      # Only update CSS classes client side
      shinyjs::runjs(sprintf('
        $("#btn_%s_1, #btn_%s_2, #btn_%s_missing").removeClass("selected");
        $("#btn_%s_2").addClass("selected");
      ', safe_id, safe_id, safe_id, safe_id))
    })
    shiny::observeEvent(input[[paste0("btn_", safe_id, "_missing")]], {
      values$annotations[[var_name]][values$counter] <- .format_NA(app_data$missing[1])
      # Only update CSS classes client side
      shinyjs::runjs(sprintf('
        $("#btn_%s_1, #btn_%s_2, #btn_%s_missing").removeClass("selected");
        $("#btn_%s_missing").addClass("selected");
      ', safe_id, safe_id, safe_id, safe_id))
    })
  })
  list(save_current = save_current, refresh_ui = refresh_ui)
}

.binary_server <- function(app_data) {
  # Binary server delegates panel rendering and observer registration to shared helpers,
  # mirroring the structure of .comparison_server() for consistency.
  function(input, output, session) {
    values <- .init_server_values(app_data)
    .setup_common_outputs(input, output, session, values, app_data)
    .setup_binary_panels(output, values, app_data)
    handler <- .make_binary_handler(input, session, values, app_data)
    .setup_nav_handler(input, session, values, handler$save_current, handler$refresh_ui)
    .setup_save_handler(input, session, values, app_data, handler$save_current)
  }
}

.run_binary_app <- function(app_data) {
  # Binary-mode equivalent of .run_categorial_app — same launch pattern, different UI/server.
  shiny::runApp(shiny::shinyApp(
    ui     = .build_binary_ui(app_data),
    server = .binary_server(app_data)
  ))
}

# ============================================================================ #
# Binary Comparison Mode                                                       #
# ---------------------------------------------------------------------------- #
# UI, server, and runner for binary annotation with side-by-side comparison    #
# ============================================================================ #

.build_binary_comparison_ui <- function(app_data) {
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
  # Binary and comparison styles are both required: binary for button colors, comparison for
  # the two-column layout divider. tagList merges them into a single tags$head injection.
  .build_app_shell(app_data, "handcodeR - Binary (Comparison)", body_slot,
    shiny::uiOutput("binary_panels_container"),
    extra_styles = shiny::tagList(.binary_styles(), .comparison_styles()),
    keyboard_script = .build_binary_keyboard_script(app_data)
  )
}

.binary_comparison_server <- function(app_data) {
  # Structure mirrors .comparison_server(): init → common outputs → comparison outputs →
  # binary panels (comparison_layout = TRUE) → nav/save with context rename.
  function(input, output, session) {
    values <- .init_server_values(app_data)
    .setup_common_outputs(input, output, session, values, app_data)
    .setup_comparison_outputs(output, values)
    .setup_binary_panels(output, values, app_data, comparison_layout = TRUE)
    handler <- .make_binary_handler(input, session, values, app_data)
    .setup_nav_handler(input, session, values, handler$save_current, handler$refresh_ui)
    .setup_save_handler(input, session, values, app_data, handler$save_current,
      extra_cleanup_function = .rename_comparison_context_columns
    )
  }
}

.run_binary_comparison_app <- function(app_data) {
  # Binary comparison equivalent of .run_binary_app — same launch pattern, different UI/server.
  shiny::runApp(shiny::shinyApp(
    ui     = .build_binary_comparison_ui(app_data),
    server = .binary_comparison_server(app_data)
  ))
}

# nolint end
