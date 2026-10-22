# handcodeR text annotation app - shared foundation provider
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
  # .text-display carries a fixed default height so text length never resizes the box before the
  # user drags it; long texts scroll instead. The divider below it overrides it via an inline height.
  # touch-action: none stops touch browsers from scrolling the page instead of dragging the divider.
  shiny::tags$style(shiny::HTML("
    .app-container { padding: 20px 2.5%; }
    .text-display { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 12px; padding: 12px 24px 24px; height: calc(7.04rem + 5px); min-height: calc(7.04rem + 5px); display: flex; flex-direction: column; overflow: auto; }
    .text-divider { height: 14px; margin: 3px 0 13px; cursor: row-resize; touch-action: none; display: flex; align-items: center; justify-content: center; }
    .text-divider::before { content: ''; width: 48px; height: 4px; border-radius: 2px; background: #cbd5e1; }
    .text-divider:hover::before, .text-divider.dragging::before { background: #2563eb; }
    .current-text { font-size: 1.1rem; line-height: 1.6; color: #1e293b; }
    .context-text { color: #94a3b8; font-size: 0.95rem; }
    .classifications-container { display: flex; gap: 16px; margin-bottom: 20px; flex-wrap: wrap; }
    .classification-card { flex: 1 1 0px; min-width: 200px; background: white; border: 1px solid #e2e8f0; border-radius: 12px; padding: 16px; }
    .classification-card h5 { color: #1e293b; font-weight: 600; margin-bottom: 10px; border-bottom: 2px solid #e2e8f0; padding-bottom: 6px; }
    .save-button-container { display: flex; justify-content: center; gap: 12px; margin-top: 16px; }
    .missing-choice { opacity: 0.5; }
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
# Shared stop() checks used by both entry points                               #
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
  # The divider writes an inline height; storing it keeps the height persistent across cases and sessions.
  # It is injected here rather than in each .build_*_ui() so all four layouts get it from one place.
  shiny::tags$script(shiny::HTML(paste0(
    "$(function() {",
    "  var box = document.querySelector('.text-display');",
    "  if (!box) return;",
    "  var stored = localStorage.getItem('handcodeR_text_height');",
    "  if (stored) box.style.height = stored;",
    "  var bar = document.createElement('div');",
    "  bar.className = 'text-divider';",
    "  box.after(bar);",
    # Pointer capture keeps the drag alive when the cursor outruns the thin bar; the CSS
    # min-height clamps the inline value, so no lower bound is needed here.
    "  bar.addEventListener('pointerdown', function(e) {",
    "    e.preventDefault();",
    "    var startY = e.clientY, startH = box.getBoundingClientRect().height;",
    "    bar.setPointerCapture(e.pointerId);",
    "    bar.classList.add('dragging');",
    "    function move(ev) { box.style.height = (startH + ev.clientY - startY) + 'px'; }",
    "    function up() {",
    "      bar.removeEventListener('pointermove', move);",
    "      bar.removeEventListener('pointerup', up);",
    "      bar.classList.remove('dragging');",
    "      localStorage.setItem('handcodeR_text_height', box.style.height);",
    "    }",
    "    bar.addEventListener('pointermove', move);",
    "    bar.addEventListener('pointerup', up);",
    "  });",
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

# nolint end
