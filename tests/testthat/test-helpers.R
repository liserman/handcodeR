# Tests for exported pure-logic helpers. No Shiny dependency.

# ── .darken_hex ──────────────────────────────────────────────────────────────

test_that(".darken_hex darkens white correctly", {
  result <- handcodeR::.darken_hex("#ffffff", 0.65)
  # 255 * 0.65 = 165.75 -> 165 = 0xa5
  expect_equal(result, "#a5a5a5")
})

test_that(".darken_hex darkens a mixed color correctly", {
  result <- handcodeR::.darken_hex("#10b981", 0.65)
  # r=16*0.65=10, g=185*0.65=120, b=129*0.65=83
  expect_equal(result, "#0a7853")
})

test_that(".darken_hex works without leading hash", {
  result <- handcodeR::.darken_hex("ffffff", 0.65)
  expect_equal(result, "#a5a5a5")
})

test_that(".darken_hex factor=1 returns same color", {
  result <- handcodeR::.darken_hex("#ffffff", 1.0)
  expect_equal(result, "#ffffff")
})

test_that(".darken_hex factor=0 returns black", {
  result <- handcodeR::.darken_hex("#ff0000", 0)
  expect_equal(result, "#000000")
})

# ── .lighten_hex ─────────────────────────────────────────────────────────────

test_that(".lighten_hex lightens black correctly", {
  result <- handcodeR::.lighten_hex("#000000", 0.88)
  # 0 + (255 - 0) * 0.88 = 224.4 -> 224 = 0xe0
  expect_equal(result, "#e0e0e0")
})

test_that(".lighten_hex leaves white unchanged", {
  result <- handcodeR::.lighten_hex("#ffffff", 0.88)
  expect_equal(result, "#ffffff")
})

test_that(".lighten_hex lightens a mixed color correctly", {
  result <- handcodeR::.lighten_hex("#10b981", 0.88)
  # r=16+(255-16)*0.88=226, g=185+(255-185)*0.88=246, b=129+(255-129)*0.88=239
  expect_equal(result, "#e2f6ef")
})

# ── .format_NA ───────────────────────────────────────────────────────────────

test_that(".format_NA wraps single value in underscores", {
  expect_equal(handcodeR::.format_NA("NA"), "_NA_")
})

test_that(".format_NA works with custom missing label", {
  expect_equal(handcodeR::.format_NA("missing"), "_missing_")
})

# ── .sanitize_id ─────────────────────────────────────────────────────────────

test_that(".sanitize_id replaces spaces with underscores", {
  expect_equal(handcodeR::.sanitize_id("my var"), "my_var")
})

test_that(".sanitize_id replaces special characters", {
  expect_equal(handcodeR::.sanitize_id("var.name-1"), "var_name_1")
})

test_that(".sanitize_id leaves valid identifiers unchanged", {
  expect_equal(handcodeR::.sanitize_id("valid_ID123"), "valid_ID123")
})

# ── .count_annotations ───────────────────────────────────────────────────────

test_that(".count_annotations returns 0 for fully empty df", {
  df <- data.frame(texts = c("a", "b"), cat1 = c("", ""), stringsAsFactors = FALSE)
  expect_equal(handcodeR::.count_annotations(df), 0L)
})

test_that(".count_annotations counts rows with at least one annotation", {
  df <- data.frame(texts = c("a", "b", "c"), cat1 = c("X", "", "Y"), stringsAsFactors = FALSE)
  expect_equal(handcodeR::.count_annotations(df), 2L)
})

test_that(".count_annotations ignores technical columns", {
  df <- data.frame(
    texts = "a", id = 1L, before = "x", after = "y", notes = "note",
    stringsAsFactors = FALSE
  )
  expect_equal(handcodeR::.count_annotations(df), 0L)
})

test_that(".count_annotations returns 0L for df with no annotation columns", {
  df <- data.frame(texts = c("a", "b"), stringsAsFactors = FALSE)
  expect_equal(handcodeR::.count_annotations(df), 0L)
})

test_that(".count_annotations treats NA as unannotated", {
  df <- data.frame(texts = "a", cat1 = NA_character_, stringsAsFactors = FALSE)
  expect_equal(handcodeR::.count_annotations(df), 0L)
})

# ── .get_current_val ─────────────────────────────────────────────────────────

test_that(".get_current_val returns value when set", {
  values <- list(annotations = list(cat1 = c("A", "B", "")))
  expect_equal(handcodeR::.get_current_val(values, "cat1", 1), "A")
})

test_that(".get_current_val returns empty string for unset row", {
  values <- list(annotations = list(cat1 = c("A", "", "")))
  expect_equal(handcodeR::.get_current_val(values, "cat1", 2), "")
})

test_that(".get_current_val returns empty string for NA", {
  values <- list(annotations = list(cat1 = c(NA_character_, "B")))
  expect_equal(handcodeR::.get_current_val(values, "cat1", 1), "")
})

# ── .character_to_data ───────────────────────────────────────────────────────

test_that(".character_to_data creates texts column", {
  df <- handcodeR::.character_to_data(
    c("hello", "world"),
    list(sentiment = c("pos", "neg")),
    missing = "NA"
  )
  expect_equal(df$texts, c("hello", "world"))
})

test_that(".character_to_data creates factor column with correct levels", {
  df <- handcodeR::.character_to_data(
    c("t1"),
    list(cat1 = c("A", "B")),
    missing = "NA"
  )
  expect_true(is.factor(df$cat1))
  expect_equal(levels(df$cat1), c("", "_NA_", "A", "B"))
})

test_that(".character_to_data initialises all rows to empty string", {
  df <- handcodeR::.character_to_data(
    c("t1", "t2"),
    list(cat1 = c("A", "B")),
    missing = "NA"
  )
  expect_true(all(as.character(df$cat1) == ""))
})

test_that(".character_to_data auto-names unnamed variables with prefix", {
  df <- handcodeR::.character_to_data(
    c("t1"),
    list(c("A", "B")),
    missing = "NA",
    prefix = "cat"
  )
  expect_true("cat1" %in% names(df))
})

test_that(".character_to_data adds comparison column when provided", {
  df <- handcodeR::.character_to_data(
    c("t1", "t2"),
    list(cat1 = c("A", "B")),
    missing = "NA",
    comparison = c("c1", "c2")
  )
  expect_equal(df$comparison, c("c1", "c2"))
})

# ── .prepare_data ────────────────────────────────────────────────────────────

test_that(".prepare_data assigns sequential IDs", {
  df <- data.frame(texts = c("a", "b", "c"), stringsAsFactors = FALSE)
  result <- handcodeR::.prepare_data(df, start = 1, randomize = FALSE, context = FALSE, pre = NULL, post = NULL)
  expect_equal(result$data$id, 1:3)
})

test_that(".prepare_data generates before/after context from neighbours", {
  df <- data.frame(texts = c("a", "b", "c"), stringsAsFactors = FALSE)
  result <- handcodeR::.prepare_data(df, start = 1, randomize = FALSE, context = TRUE, pre = NULL, post = NULL)
  expect_equal(result$data$before, c("", "a", "b"))
  expect_equal(result$data$after,  c("b", "c", ""))
})

test_that(".prepare_data uses caller-supplied pre/post over neighbour generation", {
  df <- data.frame(texts = c("a", "b"), stringsAsFactors = FALSE)
  result <- handcodeR::.prepare_data(df, start = 1, randomize = FALSE, context = TRUE,
                                      pre = c("X", "Y"), post = c("P", "Q"))
  expect_equal(result$data$before, c("X", "Y"))
  expect_equal(result$data$after,  c("P", "Q"))
})

test_that(".prepare_data skips context generation if columns already exist", {
  df <- data.frame(texts = c("a", "b"), before = c("old_b", "old_b2"),
                   after = c("old_a", "old_a2"), stringsAsFactors = FALSE)
  result <- handcodeR::.prepare_data(df, start = 1, randomize = FALSE, context = TRUE, pre = NULL, post = NULL)
  expect_equal(result$data$before, c("old_b", "old_b2"))
})

test_that(".prepare_data context=FALSE adds no before/after columns", {
  df <- data.frame(texts = c("a", "b"), stringsAsFactors = FALSE)
  result <- handcodeR::.prepare_data(df, start = 1, randomize = FALSE, context = FALSE, pre = NULL, post = NULL)
  expect_false("before" %in% names(result$data))
  expect_false("after" %in% names(result$data))
})

test_that(".prepare_data numeric start sets start_val", {
  df <- data.frame(texts = c("a", "b", "c"), stringsAsFactors = FALSE)
  result <- handcodeR::.prepare_data(df, start = 3, randomize = FALSE, context = FALSE, pre = NULL, post = NULL)
  expect_equal(result$start_val, 3L)
})

test_that(".prepare_data first_empty finds first uncoded row", {
  df <- data.frame(texts = c("a", "b", "c"), cat1 = c("X", "", ""), stringsAsFactors = FALSE)
  result <- handcodeR::.prepare_data(df, start = "first_empty", randomize = FALSE,
                                      context = FALSE, pre = NULL, post = NULL)
  expect_equal(result$start_val, 2L)
})

test_that(".prepare_data all_empty filters to uncoded rows only", {
  df <- data.frame(texts = c("a", "b", "c"), cat1 = c("X", "", ""), stringsAsFactors = FALSE)
  result <- handcodeR::.prepare_data(df, start = "all_empty", randomize = FALSE,
                                      context = FALSE, pre = NULL, post = NULL)
  expect_equal(nrow(result$data), 2L)
})

test_that(".prepare_data original_data preserves all rows after all_empty filter", {
  df <- data.frame(texts = c("a", "b", "c"), cat1 = c("X", "", ""), stringsAsFactors = FALSE)
  result <- handcodeR::.prepare_data(df, start = "all_empty", randomize = FALSE,
                                      context = FALSE, pre = NULL, post = NULL)
  expect_equal(nrow(result$original_data), 3L)
})

test_that(".prepare_data detects class_cols correctly", {
  df <- data.frame(texts = "a", before = "x", after = "y", notes = "n",
                   cat1 = "A", stringsAsFactors = FALSE)
  result <- handcodeR::.prepare_data(df, start = 1, randomize = FALSE, context = FALSE, pre = NULL, post = NULL)
  expect_equal(result$class_cols, "cat1")
})

test_that(".prepare_data extra_exclude removes columns from class_cols", {
  df <- data.frame(texts = "a", cat1 = "A", comparison = "c", stringsAsFactors = FALSE)
  result <- handcodeR::.prepare_data(df, start = 1, randomize = FALSE, context = FALSE,
                                      pre = NULL, post = NULL, extra_exclude = "comparison")
  expect_false("comparison" %in% result$class_cols)
})

# ── .gen_output ──────────────────────────────────────────────────────────────

test_that(".gen_output merges annotations back by id", {
  original <- data.frame(texts = c("a", "b", "c"), cat1 = c("", "", ""), id = 1:3,
                          stringsAsFactors = FALSE)
  result <- handcodeR::.gen_output(
    original_data  = original,
    current_ids    = c(1L, 2L, 3L),
    annotations    = list(cat1 = c("X", "Y", "Z"))
  )
  expect_equal(result$cat1, c("X", "Y", "Z"))
})

test_that(".gen_output removes id column from output", {
  original <- data.frame(texts = "a", cat1 = "", id = 1L, stringsAsFactors = FALSE)
  result <- handcodeR::.gen_output(original, 1L, list(cat1 = "X"))
  expect_false("id" %in% names(result))
})

test_that(".gen_output removes before/after columns from output", {
  original <- data.frame(texts = "a", cat1 = "", id = 1L, before = "x", after = "y",
                          stringsAsFactors = FALSE)
  result <- handcodeR::.gen_output(original, 1L, list(cat1 = "X"))
  expect_false("before" %in% names(result))
  expect_false("after" %in% names(result))
})

test_that(".gen_output preserves rows not in current_ids", {
  original <- data.frame(texts = c("a", "b"), cat1 = c("seen", ""), id = 1:2,
                          stringsAsFactors = FALSE)
  result <- handcodeR::.gen_output(original, current_ids = 2L, annotations = list(cat1 = "X"))
  expect_equal(result$cat1[1], "seen")
  expect_equal(result$cat1[2], "X")
})

test_that(".gen_output warns on unmatched row IDs", {
  original <- data.frame(texts = "a", cat1 = "", id = 1L, stringsAsFactors = FALSE)
  expect_warning(
    handcodeR::.gen_output(original, current_ids = 99L, annotations = list(cat1 = "X")),
    "row ID"
  )
})

test_that(".gen_output writes notes when add_notes=TRUE", {
  original <- data.frame(texts = c("a", "b"), cat1 = c("", ""), id = 1:2,
                          stringsAsFactors = FALSE)
  result <- handcodeR::.gen_output(
    original, current_ids = 1:2,
    annotations = list(cat1 = c("X", "Y")),
    notes = c("note1", "note2"), add_notes = TRUE
  )
  expect_equal(result$notes, c("note1", "note2"))
})

test_that(".gen_output applies extra_cleanup_function", {
  original <- data.frame(texts = "a", cat1 = "", extra_col = "drop", id = 1L,
                          stringsAsFactors = FALSE)
  result <- handcodeR::.gen_output(
    original, 1L, list(cat1 = "X"),
    extra_cleanup_function = function(df) { df$extra_col <- NULL; df }
  )
  expect_false("extra_col" %in% names(result))
})