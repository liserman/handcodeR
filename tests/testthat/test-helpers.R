# Pure-logic helpers: color, misc, styles, data prep, output. No Shiny dependency.

# ============================================================================ #
# Color: .darken_hex                                                           #
# ---------------------------------------------------------------------------- #
# Tests for RGB darkening by scalar factor.                                    #
# ============================================================================ #

test_that(".darken_hex darkens white correctly", {
  result <- handcodeR:::.darken_hex("#ffffff", 0.65)
  expect_equal(result, "#a5a5a5")
})

test_that(".darken_hex darkens a mixed color correctly", {
  result <- handcodeR:::.darken_hex("#10b981", 0.65)
  expect_equal(result, "#0a7853")
})

test_that(".darken_hex works without leading hash", {
  result <- handcodeR:::.darken_hex("ffffff", 0.65)
  expect_equal(result, "#a5a5a5")
})

test_that(".darken_hex factor=1 returns same color", {
  result <- handcodeR:::.darken_hex("#ffffff", 1.0)
  expect_equal(result, "#ffffff")
})

test_that(".darken_hex factor=0 returns black", {
  result <- handcodeR:::.darken_hex("#ff0000", 0)
  expect_equal(result, "#000000")
})

test_that(".darken_hex handles short lowercase hex", {
  expect_equal(handcodeR:::.darken_hex("#000000", 0.5), "#000000")
})

# ============================================================================ #
# Color: .lighten_hex                                                          #
# ---------------------------------------------------------------------------- #
# Tests for RGB lightening by scalar factor.                                   #
# ============================================================================ #

test_that(".lighten_hex lightens black correctly", {
  result <- handcodeR:::.lighten_hex("#000000", 0.88)
  expect_equal(result, "#e0e0e0")
})

test_that(".lighten_hex leaves white unchanged", {
  result <- handcodeR:::.lighten_hex("#ffffff", 0.88)
  expect_equal(result, "#ffffff")
})

test_that(".lighten_hex lightens a mixed color correctly", {
  result <- handcodeR:::.lighten_hex("#10b981", 0.88)
  expect_equal(result, "#e2f6ef")
})

test_that(".lighten_hex factor=0 returns original color", {
  result <- handcodeR:::.lighten_hex("#aabbcc", 0)
  expect_equal(result, "#aabbcc")
})

test_that(".lighten_hex factor=1 returns white", {
  result <- handcodeR:::.lighten_hex("#aabbcc", 1)
  expect_equal(result, "#ffffff")
})

# ============================================================================ #
# Misc: .format_NA                                                             #
# ---------------------------------------------------------------------------- #
# Wraps missing-value labels in underscore sentinels.                          #
# ============================================================================ #

test_that(".format_NA wraps single value in underscores", {
  expect_equal(handcodeR:::.format_NA("NA"), "_NA_")
})

test_that(".format_NA works with custom missing label", {
  expect_equal(handcodeR:::.format_NA("missing"), "_missing_")
})

# ============================================================================ #
# Misc: .sanitize_id                                                           #
# ---------------------------------------------------------------------------- #
# Replaces non-alphanumeric characters in Shiny input IDs.                     #
# ============================================================================ #

test_that(".sanitize_id replaces spaces with underscores", {
  expect_equal(handcodeR:::.sanitize_id("my var"), "my_var")
})

test_that(".sanitize_id replaces special characters", {
  expect_equal(handcodeR:::.sanitize_id("var.name-1"), "var_name_1")
})

test_that(".sanitize_id leaves valid identifiers unchanged", {
  expect_equal(handcodeR:::.sanitize_id("valid_ID123"), "valid_ID123")
})

# ============================================================================ #
# Misc: .get_current_value                                                     #
# ---------------------------------------------------------------------------- #
# Returns annotation value for the current row, or empty string.               #
# ============================================================================ #

test_that(".get_current_value returns value when set", {
  values <- list(annotations = list(cat1 = c("A", "B", "")))
  expect_equal(handcodeR:::.get_current_value(values, "cat1", 1), "A")
})

test_that(".get_current_value returns empty string for unset row", {
  values <- list(annotations = list(cat1 = c("A", "", "")))
  expect_equal(handcodeR:::.get_current_value(values, "cat1", 2), "")
})

test_that(".get_current_value returns empty string for NA", {
  values <- list(annotations = list(cat1 = c(NA_character_, "B")))
  expect_equal(handcodeR:::.get_current_value(values, "cat1", 1), "")
})

# ============================================================================ #
# Misc: .interactive                                                           #
# ---------------------------------------------------------------------------- #
# Verifies that .interactive() returns a logical scalar.                       #
# ============================================================================ #

test_that(".interactive returns a logical scalar", {
  result <- handcodeR:::.interactive()
  expect_true(is.logical(result) && length(result) == 1L)
})

# ============================================================================ #
# Styles: .common_styles, .binary_styles, .comparison_styles                   #
# ---------------------------------------------------------------------------- #
# CSS tag builders used by all three app modes.                                #
# ============================================================================ #

test_that(".common_styles returns a shiny tag", {
  result <- handcodeR:::.common_styles()
  expect_s3_class(result, "shiny.tag")
})

test_that(".common_styles output contains app-container CSS class", {
  result <- as.character(handcodeR:::.common_styles())
  expect_true(grepl("app-container", result, fixed = TRUE))
})

test_that(".binary_styles returns a shiny tag", {
  result <- handcodeR:::.binary_styles(list(left = "#10b981", right = "#dc2626"))
  expect_s3_class(result, "shiny.tag")
})

test_that(".binary_styles injects the supplied left color into CSS", {
  result <- as.character(handcodeR:::.binary_styles(list(left = "#aabbcc", right = "#112233")))
  expect_true(grepl("#aabbcc", result, fixed = TRUE))
})

test_that(".binary_styles injects the supplied right color into CSS", {
  result <- as.character(handcodeR:::.binary_styles(list(left = "#aabbcc", right = "#112233")))
  expect_true(grepl("#112233", result, fixed = TRUE))
})

test_that(".comparison_styles returns a shiny tag", {
  result <- handcodeR:::.comparison_styles()
  expect_s3_class(result, "shiny.tag")
})

test_that(".comparison_styles output contains comparison-col CSS class", {
  result <- as.character(handcodeR:::.comparison_styles())
  expect_true(grepl("comparison-col", result, fixed = TRUE))
})

# ============================================================================ #
# Data Prep: .character_to_data                                                #
# ---------------------------------------------------------------------------- #
# Promotes a raw character vector to the annotation data-frame schema.         #
# ============================================================================ #

test_that(".character_to_data creates texts column", {
  df <- handcodeR:::.character_to_data(
    c("hello", "world"),
    list(sentiment = c("pos", "neg")),
    missing = "NA"
  )
  expect_equal(df$texts, c("hello", "world"))
})

test_that(".character_to_data creates factor column with correct levels", {
  df <- handcodeR:::.character_to_data(
    c("t1"),
    list(cat1 = c("A", "B")),
    missing = "NA"
  )
  expect_true(is.factor(df$cat1))
  expect_equal(levels(df$cat1), c("", "_NA_", "A", "B"))
})

test_that(".character_to_data initialises all rows to empty string", {
  df <- handcodeR:::.character_to_data(
    c("t1", "t2"),
    list(cat1 = c("A", "B")),
    missing = "NA"
  )
  expect_true(all(as.character(df$cat1) == ""))
})

test_that(".character_to_data auto-names unnamed variables with prefix", {
  df <- handcodeR:::.character_to_data(
    c("t1"),
    list(c("A", "B")),
    missing = "NA",
    prefix = "cat"
  )
  expect_true("cat1" %in% names(df))
})

test_that(".character_to_data adds comparison column when provided", {
  df <- handcodeR:::.character_to_data(
    c("t1", "t2"),
    list(cat1 = c("A", "B")),
    missing = "NA",
    comparison = c("c1", "c2")
  )
  expect_equal(df$comparison, c("c1", "c2"))
})

test_that(".character_to_data handles comparison and multi-variable list together", {
  df <- handcodeR:::.character_to_data(
    c("t1", "t2"),
    list(cat1 = c("A", "B"), cat2 = c("X", "Y")),
    missing = "NA",
    comparison = c("c1", "c2")
  )
  expect_true("comparison" %in% names(df))
  expect_true("cat2" %in% names(df))
})

# ============================================================================ #
# Data Prep: .prepare_data                                                     #
# ---------------------------------------------------------------------------- #
# Normalizes input data, applies start and randomize rules.                    #
# ============================================================================ #

test_that(".prepare_data assigns sequential IDs", {
  df <- data.frame(texts = c("a", "b", "c"), stringsAsFactors = FALSE)
  result <- handcodeR:::.prepare_data(df, start = 1, randomize = FALSE, context = FALSE, pre = NULL, post = NULL)
  expect_equal(result$data$id, 1:3)
})

test_that(".prepare_data generates before/after context from neighbours", {
  df <- data.frame(texts = c("a", "b", "c"), stringsAsFactors = FALSE)
  result <- handcodeR:::.prepare_data(df, start = 1, randomize = FALSE, context = TRUE, pre = NULL, post = NULL)
  expect_equal(result$data$before, c("", "a", "b"))
  expect_equal(result$data$after, c("b", "c", ""))
})

test_that(".prepare_data uses caller-supplied pre/post over neighbour generation", {
  df <- data.frame(texts = c("a", "b"), stringsAsFactors = FALSE)
  result <- handcodeR:::.prepare_data(df,
    start = 1, randomize = FALSE, context = TRUE,
    pre = c("X", "Y"), post = c("P", "Q")
  )
  expect_equal(result$data$before, c("X", "Y"))
  expect_equal(result$data$after, c("P", "Q"))
})

test_that(".prepare_data skips context generation if columns already exist", {
  df <- data.frame(
    texts = c("a", "b"), before = c("old_b", "old_b2"),
    after = c("old_a", "old_a2"), stringsAsFactors = FALSE
  )
  result <- handcodeR:::.prepare_data(df, start = 1, randomize = FALSE, context = TRUE, pre = NULL, post = NULL)
  expect_equal(result$data$before, c("old_b", "old_b2"))
})

test_that(".prepare_data context=FALSE adds no before/after columns", {
  df <- data.frame(texts = c("a", "b"), stringsAsFactors = FALSE)
  result <- handcodeR:::.prepare_data(df, start = 1, randomize = FALSE, context = FALSE, pre = NULL, post = NULL)
  expect_false("before" %in% names(result$data))
  expect_false("after" %in% names(result$data))
})

test_that(".prepare_data numeric start sets start_val", {
  df <- data.frame(texts = c("a", "b", "c"), stringsAsFactors = FALSE)
  result <- handcodeR:::.prepare_data(df, start = 3, randomize = FALSE, context = FALSE, pre = NULL, post = NULL)
  expect_equal(result$start_val, 3L)
})

test_that(".prepare_data first_empty finds first uncoded row", {
  df <- data.frame(texts = c("a", "b", "c"), cat1 = c("X", "", ""), stringsAsFactors = FALSE)
  result <- handcodeR:::.prepare_data(df,
    start = "first_empty", randomize = FALSE,
    context = FALSE, pre = NULL, post = NULL
  )
  expect_equal(result$start_val, 2L)
})

test_that(".prepare_data all_empty filters to uncoded rows only", {
  df <- data.frame(texts = c("a", "b", "c"), cat1 = c("X", "", ""), stringsAsFactors = FALSE)
  result <- handcodeR:::.prepare_data(df,
    start = "all_empty", randomize = FALSE,
    context = FALSE, pre = NULL, post = NULL
  )
  expect_equal(nrow(result$data), 2L)
})

test_that(".prepare_data original_data preserves all rows after all_empty filter", {
  df <- data.frame(texts = c("a", "b", "c"), cat1 = c("X", "", ""), stringsAsFactors = FALSE)
  result <- handcodeR:::.prepare_data(df,
    start = "all_empty", randomize = FALSE,
    context = FALSE, pre = NULL, post = NULL
  )
  expect_equal(nrow(result$original_data), 3L)
})

test_that(".prepare_data detects class_cols correctly", {
  df <- data.frame(
    texts = "a", before = "x", after = "y", notes = "n",
    cat1 = "A", stringsAsFactors = FALSE
  )
  result <- handcodeR:::.prepare_data(df, start = 1, randomize = FALSE, context = FALSE, pre = NULL, post = NULL)
  expect_equal(result$class_cols, "cat1")
})

test_that(".prepare_data extra_exclude removes columns from class_cols", {
  df <- data.frame(texts = "a", cat1 = "A", comparison = "c", stringsAsFactors = FALSE)
  result <- handcodeR:::.prepare_data(df,
    start = 1, randomize = FALSE, context = FALSE,
    pre = NULL, post = NULL, extra_exclude = "comparison"
  )
  expect_false("comparison" %in% result$class_cols)
})

test_that(".prepare_data randomize=TRUE shuffles uncoded rows", {
  set.seed(42)
  df <- data.frame(texts = paste0("t", 1:10), cat1 = rep("", 10), stringsAsFactors = FALSE)
  result <- handcodeR:::.prepare_data(df, start = 1, randomize = TRUE, context = FALSE, pre = NULL, post = NULL)
  expect_equal(nrow(result$data), 10L)
  expect_false(identical(result$data$texts, df$texts))
})

test_that(".prepare_data randomize with first_empty: start_val is 1", {
  set.seed(1)
  df <- data.frame(texts = c("a", "b", "c"), cat1 = c("X", "", ""), stringsAsFactors = FALSE)
  result <- handcodeR:::.prepare_data(df, start = 1, randomize = TRUE, context = FALSE, pre = NULL, post = NULL)
  expect_equal(result$start_val, 1L)
})

test_that(".prepare_data extra_exclude with non-existent column is a no-op", {
  df <- data.frame(texts = "a", cat1 = "A", stringsAsFactors = FALSE)
  result <- handcodeR:::.prepare_data(df,
    start = 1, randomize = FALSE, context = FALSE,
    pre = NULL, post = NULL, extra_exclude = "nonexistent"
  )
  expect_true("cat1" %in% result$class_cols)
})

# ============================================================================ #
# Output: .init_annotations                                                    #
# ---------------------------------------------------------------------------- #
# Prefills annotation buffers to guarantee stable indexing.                    #
# ============================================================================ #

test_that(".init_annotations initialises empty strings for uncoded data", {
  app_data <- make_app_data(n = 3, vars = list(cat1 = c("A", "B")))
  result <- handcodeR:::.init_annotations(app_data)
  expect_equal(result$cat1, c("", "", ""))
})

test_that(".init_annotations preserves pre-existing annotations in data", {
  app_data <- make_app_data(n = 3, vars = list(cat1 = c("A", "B")))
  app_data$data$cat1[2] <- "A"
  result <- handcodeR:::.init_annotations(app_data)
  expect_equal(result$cat1[2], "A")
})

test_that(".init_annotations handles multiple variables", {
  app_data <- make_app_data(n = 2, vars = list(cat1 = c("A", "B"), cat2 = c("X", "Y")))
  result <- handcodeR:::.init_annotations(app_data)
  expect_true(all(c("cat1", "cat2") %in% names(result)))
})

test_that(".init_annotations returns empty vector for variable not in data", {
  app_data <- make_app_data(n = 2, vars = list(cat1 = c("A", "B")))
  app_data$classifications[["extra_var"]] <- c("P", "Q")
  result <- handcodeR:::.init_annotations(app_data)
  expect_equal(result$extra_var, c("", ""))
})

# ============================================================================ #
# Output: .gen_output                                                          #
# ---------------------------------------------------------------------------- #
# Merges annotation buffers back into the original data frame by id.           #
# ============================================================================ #

test_that(".gen_output merges annotations back by id", {
  original <- data.frame(
    texts = c("a", "b", "c"), cat1 = c("", "", ""), id = 1:3,
    stringsAsFactors = FALSE
  )
  result <- handcodeR:::.gen_output(
    original_data  = original,
    current_ids    = c(1L, 2L, 3L),
    annotations    = list(cat1 = c("X", "Y", "Z"))
  )
  expect_equal(result$cat1, c("X", "Y", "Z"))
})

test_that(".gen_output removes id column from output", {
  original <- data.frame(texts = "a", cat1 = "", id = 1L, stringsAsFactors = FALSE)
  result <- handcodeR:::.gen_output(original, 1L, list(cat1 = "X"))
  expect_false("id" %in% names(result))
})

test_that(".gen_output removes before/after columns from output", {
  original <- data.frame(
    texts = "a", cat1 = "", id = 1L, before = "x", after = "y",
    stringsAsFactors = FALSE
  )
  result <- handcodeR:::.gen_output(original, 1L, list(cat1 = "X"))
  expect_false("before" %in% names(result))
  expect_false("after" %in% names(result))
})

test_that(".gen_output preserves rows not in current_ids", {
  original <- data.frame(
    texts = c("a", "b"), cat1 = c("seen", ""), id = 1:2,
    stringsAsFactors = FALSE
  )
  result <- handcodeR:::.gen_output(original, current_ids = 2L, annotations = list(cat1 = "X"))
  expect_equal(result$cat1[1], "seen")
  expect_equal(result$cat1[2], "X")
})

test_that(".gen_output warns on unmatched row IDs", {
  original <- data.frame(texts = "a", cat1 = "", id = 1L, stringsAsFactors = FALSE)
  expect_warning(
    handcodeR:::.gen_output(original, current_ids = 99L, annotations = list(cat1 = "X")),
    "row ID"
  )
})

test_that(".gen_output writes notes when add_notes=TRUE", {
  original <- data.frame(
    texts = c("a", "b"), cat1 = c("", ""), id = 1:2,
    stringsAsFactors = FALSE
  )
  result <- handcodeR:::.gen_output(
    original,
    current_ids = 1:2,
    annotations = list(cat1 = c("X", "Y")),
    notes = c("note1", "note2"), add_notes = TRUE
  )
  expect_equal(result$notes, c("note1", "note2"))
})

test_that(".gen_output with add_notes=TRUE and empty notes column creates the column", {
  original <- data.frame(
    texts = c("a", "b"), cat1 = c("", ""), id = 1:2,
    stringsAsFactors = FALSE
  )
  result <- handcodeR:::.gen_output(
    original,
    current_ids = 1:2,
    annotations = list(cat1 = c("X", "Y")),
    notes = c("", ""), add_notes = TRUE
  )
  expect_true("notes" %in% names(result))
})

test_that(".gen_output applies extra_cleanup_function", {
  original <- data.frame(
    texts = "a", cat1 = "", extra_col = "drop", id = 1L,
    stringsAsFactors = FALSE
  )
  result <- handcodeR:::.gen_output(
    original, 1L, list(cat1 = "X"),
    extra_cleanup_function = function(df) {
      df$extra_col <- NULL
      df
    }
  )
  expect_false("extra_col" %in% names(result))
})

# ============================================================================ #
# Data Prep: .init_comparison_context                                          #
# ---------------------------------------------------------------------------- #
# Adds before_comparison/after_comparison columns for paired-text display.     #
# ============================================================================ #

test_that(".init_comparison_context context=FALSE adds no comparison context columns", {
  df <- make_text_df(3, with_comparison = TRUE)
  result <- handcodeR:::.init_comparison_context(df,
    context = FALSE,
    pre_comparison = NULL, post_comparison = NULL
  )
  expect_false("before_comparison" %in% names(result))
  expect_false("after_comparison" %in% names(result))
})

test_that(".init_comparison_context context=TRUE auto-fills before/after from neighbours", {
  df <- make_text_df(3, with_comparison = TRUE)
  result <- handcodeR:::.init_comparison_context(df,
    context = TRUE,
    pre_comparison = NULL, post_comparison = NULL
  )
  expect_equal(result$before_comparison, c("", df$comparison[1:2]))
  expect_equal(result$after_comparison, c(df$comparison[2:3], ""))
})

test_that(".init_comparison_context caller-supplied pre/post take precedence", {
  df <- make_text_df(3, with_comparison = TRUE)
  pre <- c("P1", "P2", "P3")
  pst <- c("Q1", "Q2", "Q3")
  result <- handcodeR:::.init_comparison_context(df,
    context = TRUE,
    pre_comparison = pre, post_comparison = pst
  )
  expect_equal(result$before_comparison, pre)
  expect_equal(result$after_comparison, pst)
})

test_that(".init_comparison_context single-row: boundary columns are empty strings", {
  df <- make_text_df(1, with_comparison = TRUE)
  result <- handcodeR:::.init_comparison_context(df,
    context = TRUE,
    pre_comparison = NULL, post_comparison = NULL
  )
  expect_equal(result$before_comparison, "")
  expect_equal(result$after_comparison, "")
})

test_that(".init_comparison_context context=FLEX triggers auto-fill", {
  df <- make_text_df(3, with_comparison = TRUE)
  result <- handcodeR:::.init_comparison_context(df,
    context = "FLEX",
    pre_comparison = NULL, post_comparison = NULL
  )
  expect_true("before_comparison" %in% names(result))
})

test_that(".init_comparison_context errors when pre_comparison has wrong length", {
  df <- make_text_df(3, with_comparison = TRUE)
  expect_error(
    handcodeR:::.init_comparison_context(df,
      context = TRUE,
      pre_comparison = c("a"), post_comparison = NULL
    ),
    "pre_comparison must have the same length as data"
  )
})

# ============================================================================ #
# Data Prep: .cleanup_comparison_columns                                       #
# ---------------------------------------------------------------------------- #
# Strips runtime-only comparison context columns before returning to caller.   #
# ============================================================================ #

test_that(".cleanup_comparison_columns removes both context columns", {
  df <- data.frame(
    texts = "a", before_comparison = "x", after_comparison = "y",
    stringsAsFactors = FALSE
  )
  result <- handcodeR:::.cleanup_comparison_columns(df)
  expect_false("before_comparison" %in% names(result))
  expect_false("after_comparison" %in% names(result))
})

test_that(".cleanup_comparison_columns removes only before_comparison when present alone", {
  df <- data.frame(texts = "a", before_comparison = "x", stringsAsFactors = FALSE)
  result <- handcodeR:::.cleanup_comparison_columns(df)
  expect_false("before_comparison" %in% names(result))
})

test_that(".cleanup_comparison_columns removes only after_comparison when present alone", {
  df <- data.frame(texts = "a", after_comparison = "y", stringsAsFactors = FALSE)
  result <- handcodeR:::.cleanup_comparison_columns(df)
  expect_false("after_comparison" %in% names(result))
})

test_that(".cleanup_comparison_columns returns df unchanged when neither column present", {
  df <- data.frame(texts = "a", cat1 = "X", stringsAsFactors = FALSE)
  expect_identical(handcodeR:::.cleanup_comparison_columns(df), df)
})

test_that(".cleanup_comparison_columns leaves standard before/after columns intact", {
  df <- data.frame(
    texts = "a", before = "x", after = "y",
    before_comparison = "p", after_comparison = "q",
    stringsAsFactors = FALSE
  )
  result <- handcodeR:::.cleanup_comparison_columns(df)
  expect_true("before" %in% names(result))
  expect_true("after" %in% names(result))
})
