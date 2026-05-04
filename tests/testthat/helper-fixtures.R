# Pure data builders — no I/O, no Shiny. Loaded automatically by testthat before any test file.

make_text_vec <- function(n = 5) paste0("text", seq_len(n))

make_text_df <- function(n = 5, with_comparison = FALSE, with_context = FALSE) {
  df <- data.frame(texts = make_text_vec(n), stringsAsFactors = FALSE)
  if (with_comparison) df$comparison <- paste0("comp", seq_len(n))
  if (with_context) {
    df$before <- c("", df$texts[-n])
    df$after  <- c(df$texts[-1], "")
  }
  df
}

make_ann_df <- function(n = 3, vars = "cat1") {
  df <- data.frame(texts = make_text_vec(n), stringsAsFactors = FALSE)
  for (v in vars) {
    df[[v]] <- factor(rep("", n), levels = c("", "A", "B"))
  }
  df
}

# Builds a minimal valid app_data list as produced by the handcode() / handcode_binary() entry points.
# mode: "categorial" | "binary" | "comparison"
make_app_data <- function(
  mode           = "categorial",
  n              = 3,
  vars           = list(cat1 = c("A", "B")),
  context        = FALSE,
  missing        = "Not applicable",
  add_notes      = FALSE,
  save_loc       = NULL,
  start_val      = 1L
) {
  texts <- make_text_vec(n)
  df <- data.frame(texts = texts, stringsAsFactors = FALSE)
  missing_sentinel <- paste0("_", missing, "_")
  for (v in names(vars)) {
    df[[v]] <- factor("", levels = c("", missing_sentinel, vars[[v]]))
  }
  if (mode == "comparison") df$comparison <- paste0("comp", seq_len(n))
  if (!isFALSE(context)) {
    df$before <- c("", texts[-n])
    df$after  <- c(texts[-1], "")
    if (mode == "comparison") {
      df$before_comparison <- c("", df$comparison[-n])
      df$after_comparison  <- c(df$comparison[-1], "")
    }
  }
  df$id <- seq_len(n)

  app_data <- list(
    data            = df,
    original_data   = df,
    start_val       = start_val,
    context         = context,
    classifications = vars,
    missing         = missing,
    original_name   = "test_data",
    add_notes       = add_notes,
    save_loc        = save_loc
  )
  if (mode == "binary") {
    app_data$multifactorial <- TRUE
    app_data$enable_numeric <- FALSE
    app_data$colors         <- list(left = "#10b981", right = "#dc2626")
  }
  app_data
}
