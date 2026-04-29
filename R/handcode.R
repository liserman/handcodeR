# handcodeR V2
# nolint start 
#' @importFrom utils menu
NULL

# ============================================================================ #
# Recovery Utilities                                                           #
# ---------------------------------------------------------------------------- #
# Helpers for loading RData files and counting annotations                     #
# ============================================================================ #

.load_rdata <- function(path, var_name) {
  # Recovery loads are fail-safe: unreadable files resolve to NULL instead of terminating the session flow.
  tryCatch({ env <- new.env(); load(path, envir = env); env[[var_name]] }, error = function(err) NULL)}

#' @keywords internal
#' @export
.count_annotations <- function(df) {
  # Annotation count excludes technical/context columns so progress reflects coding work only.
  cols <- setdiff(names(df), c("texts", "id", "before", "after", "notes"))
  if (length(cols) == 0) return(0L)
  # A row is considered annotated when at least one classification field contains a non-empty value.
  sum(apply(df[, cols, drop = FALSE], 1, function(x) any(x != "" & !is.na(x))))
}

# ============================================================================ #
# Resume Menu                                                                  #
# ---------------------------------------------------------------------------- #
# Offers autosave / quicksave recovery to the user at session start            #
# ============================================================================ #


.resume_menu <- function(data, original_name) {
  # Autosave is a single file overwritten each session; quicksave accumulates timestamped snapshots.
  # Menu presents annotation counts for passed data, autosave, and most-recent quicksave so the
  # operator can select the most complete recoverable state.
  # Resume handling applies only to data-frame sessions that already contain annotation structure.
  if (!is.data.frame(data) || !"texts" %in% names(data)) return(data)

  # Autosave is treated as a single recovery checkpoint for the current object name.
  autosave_path   <- paste0(original_name, "_autosave.RData")
  loaded_autosave <- if (file.exists(autosave_path)) .load_rdata(autosave_path, paste0(original_name, "_autosave")) else NULL
  loaded_autosave <- if (!is.null(loaded_autosave) && is.data.frame(loaded_autosave) && "texts" %in% names(loaded_autosave)) loaded_autosave else NULL

  # Quicksave can have multiple snapshots; latest modification time is used as recovery default.
  quicksave_pat    <- paste0("^", original_name, "_quicksave_[0-9]+\\.RData$")
  quicksave_files  <- list.files(pattern = quicksave_pat)
  quicksave_path   <- if (length(quicksave_files) > 0) quicksave_files[which.max(file.mtime(quicksave_files))] else NULL
  loaded_quicksave <- if (!is.null(quicksave_path)) .load_rdata(quicksave_path, original_name) else NULL
  loaded_quicksave <- if (!is.null(loaded_quicksave) && is.data.frame(loaded_quicksave) && "texts" %in% names(loaded_quicksave)) loaded_quicksave else NULL

  if (is.null(loaded_autosave) && is.null(loaded_quicksave)) return(data)

  n_passed <- .count_annotations(data)
  options <- list(
    list(
      label = paste0("Passed data frame (", n_passed, " of ", nrow(data), " rows annotated)"),
      data = data
    )
  )

  if (!is.null(loaded_autosave)) {
    n_autosave <- .count_annotations(loaded_autosave)
    options <- c(options, list(list(
      label = paste0("Autosave '", autosave_path, "' (", n_autosave, " of ", nrow(loaded_autosave), " rows annotated)"),
      data = loaded_autosave
    )))
  }
  if (!is.null(loaded_quicksave)) {
    n_quicksave <- .count_annotations(loaded_quicksave)
    quicksave_time <- format(file.mtime(quicksave_path), "%Y-%m-%d %H:%M")
    options <- c(options, list(list(
      label = paste0("Latest quicksave '", quicksave_path, "' (", n_quicksave, " of ", nrow(loaded_quicksave), " rows, saved ", quicksave_time, ")"),
      data = loaded_quicksave
    )))
  }
  options <- c(options, list(list(label = "Abort", data = NULL)))

  .check_aborted <- function(choice, n_options) {
    # choice == 0 means Escape/Ctrl+C; last option is always the explicit "Abort" entry.
    # Returns NULL on abort to allow graceful exit from interactive context.
    if (choice == 0 || choice == n_options) return(NULL)
    TRUE
  }

  # The menu surfaces annotation progress so users can choose the most complete recoverable state.
  labels <- sapply(options, function(opt) opt$label)
  choice <- menu(choices = labels, title = "\nSaved version(s) found. Which data do you want to use?")
  abort_check <- .check_aborted(choice, length(labels))
  if (is.null(abort_check)) return(data)  # Abort — return original data without error.
  options[[choice]]$data
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
    .text-display { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 12px; padding: 24px; margin-bottom: 20px; min-height: 100px; }
    .current-text { font-size: 1.1rem; line-height: 1.6; color: #1e293b; }
    .context-text { color: #94a3b8; font-size: 0.95rem; }
    .classifications-container { display: flex; gap: 16px; margin-bottom: 20px; flex-wrap: wrap; }
    .classification-card { flex: 1 1 0px; min-width: 200px; background: white; border: 1px solid #e2e8f0; border-radius: 12px; padding: 16px; }
    .classification-card h5 { color: #1e293b; font-weight: 600; margin-bottom: 10px; border-bottom: 2px solid #e2e8f0; padding-bottom: 6px; }
    .save-button-container { display: flex; justify-content: center; gap: 12px; margin-top: 16px; }
  "))
}

.binary_styles <- function(colors) {
  # Binary styles are generated from runtime colors to keep semantic left/right mapping configurable.
  shiny::tags$style(shiny::HTML(paste0("
    .binary-button { display: inline-block; width: 45%; margin: 2%; padding: 20px; text-align: center; border-radius: 8px; cursor: pointer; font-size: 1.2rem; transition: all 0.08s ease; }
    .binary-left { border: 2px solid ", colors$left, "; background-color: ", .lighten_hex(colors$left), "; color: ", colors$left, "; }
    .binary-left:hover { background-color: ", colors$left, "; color: white; }
    .binary-right { border: 2px solid ", colors$right, "; background-color: ", .lighten_hex(colors$right), "; color: ", colors$right, "; }
    .binary-right:hover { background-color: ", colors$right, "; color: white; }
    .binary-left.selected { background-color: ", .darken_hex(colors$left), " !important; color: white !important; border-color: ", .darken_hex(colors$left), " !important; }
    .binary-right.selected { background-color: ", .darken_hex(colors$right), " !important; color: white !important; border-color: ", .darken_hex(colors$right), " !important; }
    .missing-button { width: 96%; margin: 2%; padding: 10px; text-align: center; border: 2px solid #cbd5e1; border-radius: 8px; cursor: pointer; font-size: 1rem; background-color: #f1f5f9; color: #64748b; transition: all 0.08s ease; }
    .missing-button:hover { background-color: #cbd5e1; color: #334155; }
    .missing-button.selected { background-color: #64748b; color: white; }
  ")))
}

.comparison_styles <- function() {
  # Comparison styles focus on side-by-side readability and visual separation of both text channels.
  shiny::tags$style(shiny::HTML("
    .comparison-label { font-size: 0.75rem; font-weight: 600; color: #64748b; text-transform: uppercase; letter-spacing: 0.05em; margin-bottom: 6px; }
    .comparison-col { padding: 0 12px; }
    .comparison-col:first-child { border-right: 1px solid #e2e8f0; }
  "))
}

#' @keywords internal
#' @export
.darken_hex <- function(hex, factor = 0.65) {
  # Darkening is used for selected-button states to preserve color identity with stronger contrast.
  hex <- gsub("^#", "", hex)
  r <- strtoi(substr(hex, 1, 2), 16L)
  g <- strtoi(substr(hex, 3, 4), 16L)
  b <- strtoi(substr(hex, 5, 6), 16L)
  sprintf("#%02x%02x%02x", as.integer(r * factor), as.integer(g * factor), as.integer(b * factor))
}

#' @keywords internal
#' @export
.lighten_hex <- function(hex, factor = 0.88) {
  # Lightening is used for unselected-button backgrounds to keep emphasis on active choices.
  hex <- gsub("^#", "", hex)
  r <- strtoi(substr(hex, 1, 2), 16L)
  g <- strtoi(substr(hex, 3, 4), 16L)
  b <- strtoi(substr(hex, 5, 6), 16L)
  sprintf(
    "#%02x%02x%02x",
    as.integer(r + (255 - r) * factor),
    as.integer(g + (255 - g) * factor),
    as.integer(b + (255 - b) * factor)
  )
}

# ============================================================================ #
# Utilities                                                                    #
# ---------------------------------------------------------------------------- #
# ID sanitizer, interactive() wrapper for testability, missing formatter       #
# ============================================================================ #

#' @keywords internal
#' @export
.sanitize_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

.interactive <- function() interactive()

#' @keywords internal
#' @export
.format_NA <- function(missing) paste0("_", missing, "_")

#' @keywords internal
#' @export
.get_current_val <- function(values, var_name, counter) {
  val <- values$annotations[[var_name]][counter]
  if (!is.na(val) && val != "") val else ""
}

# ============================================================================ #
# Input Validation                                                             #
# ---------------------------------------------------------------------------- #
# All stop() checks for both entry points, separated by mode                   #
# ============================================================================ #

.check_common_params <- function(data, start, randomize, context, pre, post) {
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
  if (names(data)[1] != "texts" || !is.character(data[[1]]))
    stop("First column must be texts and contain character data.")
}

.check_cat_session <- function(interactive_mode, arg_list, data) {
  if (!interactive_mode) stop("handcode() can only be used in an interactive R session.")
  # Guard against accidentally passing binary-mode args (colors) to the categorial entry point.
  if ("colors" %in% names(arg_list)) stop("colors is not supported in categorial annotation.")
  if (!is.data.frame(data) && !is.character(data))
    stop("data must be a character vector or data frame from a previous handcode() session.")
}

.check_cat_args <- function(arg_list, missing) {
  if (length(arg_list) < 1) stop("At least one classification variable must be provided.")
  if (!all(vapply(arg_list, is.character, logical(1))))
    stop("All classification arguments must be character vectors.")
  # Empty string is the internal sentinel for "not yet coded" — cannot be a valid category.
  if (any(vapply(arg_list, function(x) "" %in% x, logical(1))))
    stop("Empty strings are not allowed as category values.")
  for (i in seq_along(arg_list)) {
    if (length(unique(arg_list[[i]])) < length(arg_list[[i]]))
      stop("Duplicate categories are not allowed.")
  }
  for (cat_vec in arg_list) {
    if (any(missing %in% cat_vec)) stop("Missing values cannot overlap with category values.")
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
  if (!"comparison" %in% names(data)) stop("Data frame must contain a comparison column.")
}

.check_comparison_context <- function(pre_comparison, post_comparison, n_rows) {
  # pre/post_comparison are optional; validate length only when the caller supplies them.
  if (!is.null(pre_comparison) && length(pre_comparison) != n_rows)
    stop("pre_comparison must have the same length as data.")
  if (!is.null(post_comparison) && length(post_comparison) != n_rows)
    stop("post_comparison must have the same length as data.")
}

.check_bin_session <- function(interactive_mode, data) {
  if (!interactive_mode) stop("handcode_binary() can only be used in an interactive R session.")
  if (!is.data.frame(data) && !is.character(data))
    stop("data must be a character vector or data frame from a previous handcode_binary() session.")
}

.check_binary_args <- function(arg_list) {
  if (length(arg_list) < 1) stop("At least one binary classification variable must be provided.")
  # Exactly 2 values enforced because UI hardcodes left/right button semantics per variable.
  if (!all(vapply(arg_list, function(x) is.character(x) && length(x) == 2, logical(1))))
    stop("All binary classification arguments must be character vectors with exactly two values.")
  if (any(vapply(arg_list, function(x) "" %in% x, logical(1))))
    stop("Empty strings are not allowed as binary values.")
}

.check_binary_params <- function(missing, multifactorial, enable_numeric, arg_list) {
  # Binary mode uses one shared missing button per variable, so only a single missing label is valid.
  if (length(missing) != 1) stop("Missing argument must be a single value in binary annotation.")
  if (!is.logical(multifactorial) || length(multifactorial) != 1) stop("multifactorial must be a single logical value.")
  if (!is.logical(enable_numeric) || length(enable_numeric) != 1) stop("enable_numeric must be a single logical value.")
  # Keys 1–9 map to variables by position; more than 9 would exceed the available key range.
  if (enable_numeric && length(arg_list) > 9) stop("enable_numeric = TRUE supports at most 9 classification variables.")
}

.check_colors_bin <- function(colors) {
  # Hex validation prevents malformed strings from breaking CSS color injection in .binary_styles().
  hex_pattern <- "^#[0-9A-Fa-f]{6}$"
  if (!grepl(hex_pattern, colors$left)) stop("colors$left must be a valid 6-digit hex color (e.g. '#10b981').")
  if (!grepl(hex_pattern, colors$right)) stop("colors$right must be a valid 6-digit hex color (e.g. '#dc2626').")
}

# ============================================================================ #
# Data Preparation                                                             #
# ---------------------------------------------------------------------------- #
# Normalizes input data, applies start/randomize rules, inits annotation state #
# ============================================================================ #

#' @keywords internal
#' @export
.prepare_data <- function(data, start, randomize, context, pre, post, extra_exclude = character(0)) {
  # Returns list(data, original_data, start_val, class_cols). data may be filtered/reordered by
  # start and randomize rules; original_data is preserved at full row count for save-back merge.
  .check_data_first_col(data)
  if (!isFALSE(context)) {
    # Caller-provided context takes precedence; otherwise context is generated from neighboring rows.
    # Skip generation if context columns already exist (prior session).
    if (!("before" %in% names(data)) && !("after" %in% names(data))) {
      if (!is.null(pre) && !is.null(post)) {
        data$before <- pre
        data$after  <- post
      } else {
        data$before <- c("", data$texts[-nrow(data)])
        data$after  <- c(data$texts[-1], "")
      }
    }
  }
  data$id       <- seq_len(nrow(data))
  original_data <- data
  class_cols    <- setdiff(names(data), c("texts", "id", "before", "after", "notes", extra_exclude))
  start_val     <- 1L
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
    if (length(empty_rows) > 0) { data <- data[empty_rows, ]; start_val <- 1L }
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

.setup_nav_handler <- function(input, values, save_function, refresh_function) {
  # save_function() fires unconditionally on every navigation event, even at boundary (no-op row move).
  # Pending edits flush before the row index changes, so no annotation is silently lost.
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
  shiny::observeEvent(input$quicksave, {
    # Quicksave captures current progress without ending the annotation session.
    annotated_df  <- save_function(perform_autosave = FALSE)
    # Timestamp naming keeps snapshots sortable and avoids overwriting prior checkpoints.
    quicksave_file <- paste0(app_data$original_name, "_quicksave_", as.integer(Sys.time()), ".RData")
    # assign() places the variable in this function's local env so save() can locate it by name.
    assign(app_data$original_name, annotated_df, envir = environment())
    tryCatch(
      {
        save(list = app_data$original_name, file = quicksave_file, envir = environment())
        shiny::showNotification(paste("Quicksaved:", quicksave_file), type = "message", duration = 3)
      },
      error = function(e) shiny::showNotification(paste("Quicksave failed:", e$message), type = "error")
    )
  })
}

.setup_save_handler <- function(input, session, values, app_data, autosave, save_function, extra_cleanup_function = NULL) {
  # Two exit paths: Save&Exit sets intentional_close, skips autosave, shows confirmation modal,
  # then calls stopApp(). Browser close / kill / disconnect leaves intentional_close = FALSE so
  # onSessionEnded writes an autosave file as the recovery checkpoint. This dual path is the
  # safety net for the whole annotation session.
  close_state <- shiny::reactiveValues(intentional_close = FALSE, autosave_written = FALSE)

  do_save <- function(perform_autosave = TRUE) {
    save_function()
    annotated_df <- .gen_output(
      original_data          = values$original_data,
      current_ids            = values$data$id,
      annotations            = values$annotations,
      notes                  = if (app_data$add_notes) values$notes else NULL,
      add_notes              = app_data$add_notes,
      extra_cleanup_function = extra_cleanup_function
    )
    if (perform_autosave && autosave && nchar(app_data$original_name) > 0 && !close_state$autosave_written) {
      # Autosave is reserved for unexpected termination to preserve explicit user exit behavior.
      # Guard prevents double-write if both Save&Exit and onSessionEnded fire.
      autosave_file <- paste0(app_data$original_name, "_autosave.RData")
      tryCatch(
        {
          autosave_var <- paste0(app_data$original_name, "_autosave")
          # assign() writes to local env so save() can locate the variable by name below.
          assign(autosave_var, annotated_df, envir = environment())
          save(list = autosave_var, file = autosave_file, envir = environment())
          close_state$autosave_written <- TRUE
          message(paste("Auto-saved to:", autosave_file))
        },
        error = function(e) warning("Auto-save failed: ", e$message)
      )
    }
    return(annotated_df)
  }

  .setup_quicksave_handler(input, app_data, do_save)

  shiny::observeEvent(input$save_exit, {
    close_state$intentional_close <- TRUE
    annotated_df <- do_save(perform_autosave = FALSE)
    shinyjs::runjs("document.head.insertAdjacentHTML('beforeend', '<style>#shiny-disconnected-overlay{display:none!important}</style>');")
    # Show dialog when save and exit is triggered to provide feedback that data was saved before the app closes.
    shiny::showModal(shiny::modalDialog(
      shiny::p("Your data was saved. You can close this tab now."),
      shiny::tags$small(style = "color:#64748b;",
        "Please cite: Isermann, Lukas and Klingenspohr, Dennis. 2026. handcodeR: Text annotation app. R package version 0.2.0. https://github.com/liserman/handcodeR"
      ),
      title = "Data saved",
      footer = NULL,
      easyClose = FALSE,
      size = "s"
    ))
    shinyjs::delay(300, shiny::stopApp(annotated_df))
  })

  session$onSessionEnded(function() {
    shiny::isolate({
      if (!close_state$intentional_close) {
        do_save(perform_autosave = TRUE)
      }
    })
  })
}

.setup_categorial_panels <- function(output, values, app_data) {
  # Panels are rendered per variable so category sets can differ while sharing one interaction pattern.
  output$classification_panels <- shiny::renderUI({
    classifications <- app_data$classifications
    if (length(classifications) == 0) return(NULL)
    panels <- lapply(names(classifications), function(var_name) {
      choices <- classifications[[var_name]]
      current_val <- .get_current_val(values, var_name, values$counter)
      missing_choices <- setNames(
        .format_NA(app_data$missing),
        paste0("(", app_data$missing, ")")
      )
      shiny::div(
        class = "classification-card",
        shiny::h5(var_name),
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
  # save_current captures the current row state before any navigation or session close event.
  save_current <- function() {
    for (var_name in names(app_data$classifications)) {
      input_id <- paste0("class_", var_name)
      if (!is.null(input[[input_id]])) {
        values$annotations[[var_name]][values$counter] <- input[[input_id]]
      }
    }
    if (app_data$add_notes && !is.null(input$note_text)) {
      values$notes[values$counter] <- input$note_text
    }
  }
  refresh_ui <- function() {
    # refresh_ui rehydrates controls when row index changes to keep UI aligned with stored values.
    for (var_name in names(app_data$classifications)) {
      input_id <- paste0("class_", var_name)
      current_val <- .get_current_val(values, var_name, values$counter)
      shiny::updateRadioButtons(session, input_id, selected = current_val)
    }
    if (app_data$add_notes) {
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

#' @keywords internal
#' @export
.gen_output <- function(original_data, current_ids, annotations, notes = NULL,
                        add_notes = FALSE, extra_cleanup_function = NULL) {
  # Merges annotation buffers back into the full original dataset by id, so subsetting from
  # start = "all_empty" or randomize = TRUE never drops rows the user did not see.
  # extra_cleanup_function lets mode-specific runtime columns (e.g. comparison context) be
  # stripped before the annotated frame is returned to the caller.
  annotated_df <- original_data
  idx_map <- match(current_ids, annotated_df$id)
  valid_idx <- !is.na(idx_map)
  if (any(!valid_idx)) warning(sum(!valid_idx), " row ID(s) in current session not found in original data and were skipped.")

  for (annotation_name in names(annotations)) {
    if (annotation_name %in% names(annotated_df)) {
      annotated_df[[annotation_name]][idx_map[valid_idx]] <- annotations[[annotation_name]][valid_idx]
    }
  }
  if (add_notes && !is.null(notes)) {
    notes_col <- if ("notes" %in% names(annotated_df)) as.character(annotated_df$notes) else rep("", nrow(annotated_df))
    notes_col[idx_map[valid_idx]] <- notes[valid_idx]
    annotated_df$notes <- notes_col
  }
  annotated_df$id <- NULL
  if ("before" %in% names(annotated_df)) annotated_df$before <- NULL
  if ("after" %in% names(annotated_df)) annotated_df$after <- NULL
  if (!is.null(extra_cleanup_function)) annotated_df <- extra_cleanup_function(annotated_df)
  annotated_df
}

#' @keywords internal
#' @export
.character_to_data <- function(data, arg_list, missing, prefix = "cat", comparison = NULL) {
  # Promotes a raw character vector to the annotation data-frame schema. Each classification
  # becomes a factor column with empty-string + missing sentinels + caller-supplied levels.
  # Auto-named variables (cat1, cat2, ... or bin1, bin2, ...) keep behaviour stable when the
  # caller passes unnamed ... arguments.
  df <- data.frame(texts = data, stringsAsFactors = FALSE)
  if (!is.null(comparison)) df$comparison <- comparison
  for (i in seq_along(arg_list)) {
    var_name <- names(arg_list)[i]
    if (is.null(var_name) || var_name == "") var_name <- paste0(prefix, i)
    df[[var_name]] <- factor("", levels = c("", .format_NA(missing), arg_list[[i]]))
  }
  df
}

# ============================================================================ #
# App Shell Builder                                                            #
# ---------------------------------------------------------------------------- #
# Shared fluidPage wrapper consumed by all three build_X_ui functions          #
# ============================================================================ #

.build_app_shell <- function(app_data, title, body_slot, panels_slot,
                             extra_styles = NULL, keyboard_script = NULL) {
  # Default script adds activeElement guard so Space/Enter don't fire inside notes/input fields.
  default_script <- paste0(
    "$(document).on('keyup', function(e) {",
    " if ($(document.activeElement).is('input, textarea')) return;",
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
        shiny::actionButton("quicksave", "Quicksave", class = "btn btn-warning"),
        shiny::actionButton("save_exit", "Save and Exit", class = "btn btn-success")
      ),
      if (app_data$add_notes) {
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
    shiny::tags$script(shiny::HTML(if (!is.null(keyboard_script)) keyboard_script else default_script))
  )
}

# ============================================================================ #
# Categorial & Comparison Mode                                                 #
# ---------------------------------------------------------------------------- #
# Entry point, UI, server, and runner for categorial and comparison annotation #
# ============================================================================ #

# Categorial annotation entry point for multi-class coding workflows.
handcode <- function(data, ..., start = "first_empty", randomize = FALSE,
                                context = FALSE, missing = c("Not applicable"),
                                pre = NULL, post = NULL,
                                comparison = NULL,
                                pre_comparison = NULL, post_comparison = NULL,
                                autosave = TRUE, add_notes = FALSE) {
  arg_list <- list(...)
  original_name <- deparse(substitute(data))
  .check_cat_session(.interactive(), arg_list, data)

  # Comparison mode is auto-detected: either a comparison vector is supplied alongside
  # character data, or a resumed data frame already carries the comparison column.
  comparison_mode <- !is.null(comparison) || (is.data.frame(data) && "comparison" %in% names(data))

  # Resume menu may swap the passed data for an autosave/quicksave snapshot before validation.
  data <- .resume_menu(data, original_name)

  # Char-vector path: validate ... category specs before promoting to a data frame.
  if (is.character(data)) {
    .check_cat_args(arg_list, missing)
    if (comparison_mode) .check_comparison_args(comparison, data)
  }

  # Promote raw character vector to the schema-bearing annotation data frame.
  if (is.character(data)) {
    data <- .character_to_data(data, arg_list, missing, prefix = "cat",
                               comparison = if (comparison_mode) comparison else NULL)
  }

  # Existing sessions can be extended: factor levels are additive — old levels are never removed,
  # new ones appended. This prevents data loss when a category list shrinks between calls.
  if (is.data.frame(data) && length(arg_list) > 0) {
    for (i in seq_along(arg_list)) {
      var_name <- names(arg_list)[i]
      if (is.null(var_name) || var_name == "") next
      # Update existing columns in place or create new schema columns when newly requested.
      if (var_name %in% names(data)) {
        # Existing levels remain valid; new levels are appended to avoid data loss.
        current_levels <- levels(data[[var_name]])
        new_levels <- c("", .format_NA(missing), arg_list[[i]])
        all_levels <- unique(c(current_levels, new_levels))
        data[[var_name]] <- factor(as.character(data[[var_name]]), levels = all_levels)
      } else {
        # New variables start uncoded across all rows to avoid implicit assumptions.
        all_levels <- c("", .format_NA(missing), arg_list[[i]])
        data[[var_name]] <- factor("", levels = all_levels)
      }
    }
  }

  # Re-validate missing sentinel consistency when resuming with new missing argument.
  if (is.data.frame(data) && length(arg_list) > 0) {
    for (col in names(data)) {
      if (is.factor(data[[col]]) && col %in% names(arg_list)) {
        existing_sentinel <- .format_NA(missing)
        missing_sentinels <- existing_sentinel[!existing_sentinel %in% levels(data[[col]])]
        if (length(missing_sentinels) > 0) {
          warning("Existing factor levels do not match new missing sentinel(s): ",
                  paste(missing_sentinels, collapse = ", "), " not found in ", col)
        }
      }
    }
  }

  if (comparison_mode) .check_comparison_col(data)

  .check_common_params(data, start, randomize, context, pre, post)

  # Comparison-side context columns mirror the primary text context: caller-supplied pre/post
  # take precedence, otherwise neighbouring rows fill before/after channels.
  if (comparison_mode) {
    n_rows <- if (is.data.frame(data)) nrow(data) else length(data)
    .check_comparison_context(pre_comparison, post_comparison, n_rows)
    if (!isFALSE(context)) {
      if (!is.null(pre_comparison) && !is.null(post_comparison)) {
        data$before_comparison <- pre_comparison
        data$after_comparison  <- post_comparison
      } else {
        data$before_comparison <- c("", data$comparison[-nrow(data)])
        data$after_comparison  <- c(data$comparison[-1], "")
      }
    }
  }

  # Comparison columns are excluded from class-column detection so they aren't treated as
  # annotation targets by start/randomize logic in .prepare_data().
  prepared_data <- if (comparison_mode) {
    .prepare_data(data, start, randomize, context, pre, post,
                  extra_exclude = c("comparison", "before_comparison", "after_comparison"))
  } else {
    .prepare_data(data, start, randomize, context, pre, post)
  }

  # Reconstruct the per-variable choice list from factor levels, stripping the empty-string
  # placeholder and the _missing_ sentinels so only real categories are surfaced to the UI.
  arg_list <- list()
  for (col in prepared_data$class_cols) {
    if (is.factor(prepared_data$data[[col]])) {
      lvls <- levels(prepared_data$data[[col]])
      lvls <- lvls[!grepl("^_.*_$|^$", lvls)]
      if (length(lvls) > 0) arg_list[[col]] <- lvls
    }
  }

  # app_data is the immutable runtime bundle passed into UI/server builders.
  app_data <- list(
    data            = prepared_data$data,
    original_data   = prepared_data$original_data,
    start_val       = prepared_data$start_val,
    context         = context,
    classifications = arg_list,
    missing         = missing,
    original_name   = original_name,
    add_notes       = add_notes
  )

  # UI execution is isolated in the app runner; this function only prepares and returns result data.
  result <- if (comparison_mode) {
    .run_comparison_app(app_data, autosave)
  } else {
    .run_categorial_app(app_data, autosave)
  }
  message("Please cite: Isermann, Lukas and Klingenspohr, Dennis. 2026. handcodeR: Text annotation app. R package version 0.2.0. https://github.com/liserman/handcodeR")
  result
}

.build_categorial_ui <- function(app_data) {
  body_slot <- shiny::div(
    class = "text-display",
    shiny::uiOutput("context_before"),
    shiny::div(class = "current-text", shiny::htmlOutput("current_text")),
    shiny::uiOutput("context_after")
  )
  .build_app_shell(app_data, "handcodeR - Categorial", body_slot,
                   shiny::uiOutput("classification_panels"))
}

.categorial_server <- function(app_data, autosave) {
  function(input, output, session) {
    values <- .init_server_values(app_data)
    .setup_common_outputs(input, output, session, values, app_data)
    .setup_categorial_panels(output, values, app_data)
    handler <- .make_categorial_handler(input, session, values, app_data)
    .setup_nav_handler(input, values, handler$save_current, handler$refresh_ui)
    .setup_save_handler(input, session, values, app_data, autosave, handler$save_current)
  }
}

.run_categorial_app <- function(app_data, autosave) {
  # Wraps UI + server into a Shiny app and blocks until the user closes the session.
  # Return value is the annotated data frame propagated up via stopApp() in .setup_save_handler.
  shiny::runApp(shiny::shinyApp(
    ui     = .build_categorial_ui(app_data),
    server = .categorial_server(app_data, autosave)
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
                   extra_styles = .comparison_styles())
}

.comparison_server <- function(app_data, autosave) {
  # Comparison server extends categorial with a second text channel and strips runtime context columns on save.
  function(input, output, session) {
    values <- .init_server_values(app_data)
    .setup_common_outputs(input, output, session, values, app_data)

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

    .setup_categorial_panels(output, values, app_data)
    handler <- .make_categorial_handler(input, session, values, app_data)
    .setup_nav_handler(input, values, handler$save_current, handler$refresh_ui)

    # Comparison context columns are runtime-only; extra_cleanup_function strips them before
    # the annotated data frame is returned to the caller.
    .setup_save_handler(input, session, values, app_data, autosave, handler$save_current,
      extra_cleanup_function = function(df) {
        if ("before_comparison" %in% names(df)) df$before_comparison <- NULL
        if ("after_comparison" %in% names(df)) df$after_comparison <- NULL
        df
      }
    )
  }
}

.run_comparison_app <- function(app_data, autosave) {
  # Comparison-mode equivalent of .run_categorial_app — same launch pattern, different UI/server.
  shiny::runApp(shiny::shinyApp(
    ui     = .build_comparison_ui(app_data),
    server = .comparison_server(app_data, autosave)
  ))
}

# ============================================================================ #
# Binary Mode                                                                  #
# ---------------------------------------------------------------------------- #
# Entry point, UI, server, and runner for two-choice binary annotation         #
# ============================================================================ #

# Binary annotation entry point for two-choice (left/right) coding workflows.
handcode_binary <- function(data, ..., start = "first_empty", randomize = FALSE,
                            context = FALSE, missing = c("Not applicable"),
                            pre = NULL, post = NULL, autosave = TRUE,
                            multifactorial = TRUE, enable_numeric = FALSE,
                            colors = list(), add_notes = FALSE) {
  arg_list <- list(...)
  original_name <- deparse(substitute(data))
  .check_bin_session(.interactive(), data)

  # Resume menu may swap the passed data for an autosave/quicksave snapshot before validation.
  data <- .resume_menu(data, original_name)

  # Char-vector path: validate that each ... entry is a length-2 character vector.
  if (is.character(data)) .check_binary_args(arg_list)

  .check_binary_params(missing, multifactorial, enable_numeric, arg_list)

  # Color inputs are validated early to avoid runtime UI inconsistencies.
  default_colors <- list(left = "#10b981", right = "#dc2626")
  colors <- utils::modifyList(default_colors, colors)
  .check_colors_bin(colors)

  # Promote raw character vector to the schema-bearing annotation data frame.
  if (is.character(data)) {
    data <- .character_to_data(data, arg_list, missing, prefix = "bin")
  }

  # Existing sessions can be extended: factor levels are additive — old levels preserved,
  # new ones appended to avoid data loss when labels change between calls.
  if (is.data.frame(data) && length(arg_list) > 0) {
    for (i in seq_along(arg_list)) {
      var_name <- names(arg_list)[i]
      if (is.null(var_name) || var_name == "") next
      if (var_name %in% names(data)) {
        # Existing columns are re-leveled rather than replaced to avoid data loss.
        current_levels <- levels(data[[var_name]])
        new_levels <- c("", .format_NA(missing), arg_list[[i]])
        all_levels <- unique(c(current_levels, new_levels))
        data[[var_name]] <- factor(as.character(data[[var_name]]), levels = all_levels)
      } else {
        # New variables are initialized as uncoded for every row.
        data[[var_name]] <- factor("", levels = c("", .format_NA(missing), arg_list[[i]]))
      }
    }
  }

  # Re-validate missing format consistency when resuming with new missing argument.
  if (is.data.frame(data) && length(arg_list) > 0) {
    for (col in names(data)) {
      if (is.factor(data[[col]]) && col %in% names(arg_list)) {
        existing_format <- .format_NA(missing)
        if (!existing_format %in% levels(data[[col]])) {
          warning("Existing factor levels do not match new missing format: ", existing_format, " not found in ", col)
        }
      }
    }
  }

  .check_common_params(data, start, randomize, context, pre, post)
  prepared_data <- .prepare_data(data, start, randomize, context, pre, post)

  # Reconstruct the per-variable choice list from factor levels, stripping the empty-string
  # placeholder and the _missing_ sentinels so only real categories are surfaced to the UI.
  arg_list <- list()
  for (col in prepared_data$class_cols) {
    if (is.factor(prepared_data$data[[col]])) {
      lvls <- levels(prepared_data$data[[col]])
      lvls <- lvls[!grepl("^_.*_$|^$", lvls)]
      if (length(lvls) > 0) arg_list[[col]] <- lvls
    }
  }

  # Binary-mode app_data carries extra UI flags: multifactorial, enable_numeric, colors.
  app_data <- list(
    data            = prepared_data$data,
    original_data   = prepared_data$original_data,
    start_val       = prepared_data$start_val,
    context         = context,
    classifications = arg_list,
    missing         = missing,
    original_name   = original_name,
    multifactorial  = multifactorial,
    enable_numeric  = enable_numeric,
    colors          = colors,
    add_notes       = add_notes
  )

  # Execution delegates to binary app runtime after input normalization is complete.
  result <- .run_binary_app(app_data, autosave)
  message("Please cite: Isermann, Lukas and Klingenspohr, Dennis. 2026. handcodeR: Text annotation app. R package version 0.2.0. https://github.com/liserman/handcodeR")
  result
}

.build_binary_ui <- function(app_data) {
  body_slot <- shiny::div(
    class = "text-display",
    shiny::uiOutput("context_before"),
    shiny::div(class = "current-text", shiny::htmlOutput("current_text")),
    shiny::uiOutput("context_after")
  )
  # Keys 1-9 trigger the left-button click for the variable at that position. Only fires
  # outside form fields. Cap of 9 matches the enable_numeric length limit in handcode_binary().
  keyboard_script <- paste0(
    "$(document).on('keyup', function(e) {",
    " if ($(document.activeElement).is('input, textarea')) return;",
    " if (e.key === ' ') { e.preventDefault(); $('#prev').click(); }",
    " if (e.key === 'Enter') $('#next').click();",
    "});",
    if (app_data$enable_numeric) {
      var_names_js <- paste0('["', paste(sapply(names(app_data$classifications), .sanitize_id), collapse = '","'), '"]')
      sprintf('
        var numericVars = %s;
        $(document).on("keydown", function(e) {
          if ($(document.activeElement).is("input, textarea, select")) return;
          var num = parseInt(e.key);
          if (!isNaN(num) && num >= 1 && num <= numericVars.length) {
            e.preventDefault();
            $("#btn_" + numericVars[num - 1] + "_1").click();
          }
        });
      ', var_names_js)
    } else ""
  )
  .build_app_shell(app_data, "handcodeR - Binary", body_slot,
                   shiny::uiOutput("binary_panels_container"),
                   extra_styles = .binary_styles(app_data$colors),
                   keyboard_script = keyboard_script)
}

.binary_server <- function(app_data, autosave) {
  # Binary server builds per-variable button panels, registers click observers, and wires shared
  # nav/save handlers. Visual selected-state updated via shinyjs to skip server round-trips.
  function(input, output, session) {
    values <- .init_server_values(app_data)

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
    classifications <- app_data$classifications
    for (i in seq_along(names(classifications))) {
      local({
        var_name <- names(classifications)[i]
        var_idx  <- i
        safe_id  <- .sanitize_id(var_name)
        output[[paste0("panels_", safe_id)]] <- shiny::renderUI({
          shiny::isolate({
            choices     <- classifications[[var_name]]
            current_val <- .get_current_val(values, var_name, values$counter)
            panel_label <- if (app_data$enable_numeric) paste0(var_idx, ". ", var_name) else var_name
            shiny::div(
              class = "classification-card",
              shiny::h5(panel_label),
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
              ),
              shiny::actionButton(
                inputId = paste0("btn_", safe_id, "_missing"),
                label = "(missing)",
                class = paste("missing-button", if (current_val == .format_NA(app_data$missing[1])) "selected" else "")
              )
            )
          })
        })
      })
    }

    .setup_common_outputs(input, output, session, values, app_data)

    save_current <- function() {
      # Binary choices are written immediately by button events; this hook persists notes if enabled.
      if (app_data$add_notes && !is.null(input$note_text)) {
        values$notes[values$counter] <- input$note_text
      }
    }

    refresh_ui <- function() {
      # Button classes are recomputed on navigation so visual state always matches stored annotations.
      missing_val <- .format_NA(app_data$missing[1])
      for (var_name in names(app_data$classifications)) {
        safe_id <- .sanitize_id(var_name)
        choices <- app_data$classifications[[var_name]]
        current_val <- .get_current_val(values, var_name, values$counter)
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
      if (app_data$add_notes) {
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

    .setup_nav_handler(input, values, save_current, refresh_ui)
    .setup_save_handler(input, session, values, app_data, autosave, save_current)
  }
}

.run_binary_app <- function(app_data, autosave) {
  # Binary-mode equivalent of .run_categorial_app — same launch pattern, different UI/server.
  shiny::runApp(shiny::shinyApp(
    ui     = .build_binary_ui(app_data),
    server = .binary_server(app_data, autosave)
  ))
}

# nolint end