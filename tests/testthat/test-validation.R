# All input-validation tests. Autosave cancel/abort flows live in test-autosave.R.
# Tests mock .interactive to bypass the session guard; validation fires before any Shiny call.

# ---- common: .check_common_params ----

test_that(".check_common_params rejects start vector longer than 1", {
  df <- data.frame(texts = "a", stringsAsFactors = FALSE)
  expect_error(
    handcodeR:::.check_common_params(df, start = c(1, 2), randomize = FALSE, context = FALSE, pre = NULL, post = NULL),
    "start must be a single value"
  )
})

test_that(".check_common_params rejects invalid start string", {
  df <- data.frame(texts = "a", stringsAsFactors = FALSE)
  expect_error(
    handcodeR:::.check_common_params(df, start = "banana", randomize = FALSE, context = FALSE, pre = NULL, post = NULL),
    "start must be numeric, first_empty, or all_empty"
  )
})

test_that(".check_common_params accepts first_empty and all_empty", {
  df <- data.frame(texts = "a", stringsAsFactors = FALSE)
  expect_no_error(handcodeR:::.check_common_params(df, "first_empty", FALSE, FALSE, NULL, NULL))
  expect_no_error(handcodeR:::.check_common_params(df, "all_empty",   FALSE, FALSE, NULL, NULL))
})

test_that(".check_common_params rejects non-logical randomize", {
  df <- data.frame(texts = "a", stringsAsFactors = FALSE)
  expect_error(
    handcodeR:::.check_common_params(df, 1, "yes", FALSE, NULL, NULL),
    "randomize must be a single logical value"
  )
})

test_that(".check_common_params rejects invalid context value", {
  df <- data.frame(texts = "a", stringsAsFactors = FALSE)
  expect_error(
    handcodeR:::.check_common_params(df, 1, FALSE, "maybe", NULL, NULL),
    "context must be TRUE, FALSE"
  )
})

test_that(".check_common_params accepts context FLEX", {
  df <- data.frame(texts = "a", stringsAsFactors = FALSE)
  expect_no_error(handcodeR:::.check_common_params(df, 1, FALSE, "FLEX", NULL, NULL))
})

test_that(".check_common_params rejects pre with wrong length", {
  df <- data.frame(texts = c("a", "b", "c"), stringsAsFactors = FALSE)
  expect_error(
    handcodeR:::.check_common_params(df, 1, FALSE, FALSE, pre = c("x", "y"), post = NULL),
    "pre must have the same length as data"
  )
})

test_that(".check_common_params rejects post with wrong length", {
  df <- data.frame(texts = c("a", "b", "c"), stringsAsFactors = FALSE)
  expect_error(
    handcodeR:::.check_common_params(df, 1, FALSE, FALSE, pre = NULL, post = c("x", "y")),
    "post must have the same length as data"
  )
})

# ---- common: .check_data_first_col ----

test_that(".check_data_first_col accepts valid texts column", {
  df <- data.frame(texts = c("a", "b"), stringsAsFactors = FALSE)
  expect_no_error(handcodeR:::.check_data_first_col(df))
})

test_that(".check_data_first_col rejects wrong column name", {
  df <- data.frame(content = "a", stringsAsFactors = FALSE)
  expect_error(
    handcodeR:::.check_data_first_col(df),
    "First column must be texts"
  )
})

test_that(".check_data_first_col rejects non-character texts column", {
  df <- data.frame(texts = 1:3)
  expect_error(
    handcodeR:::.check_data_first_col(df),
    "First column must be texts"
  )
})

# ---- categorial: handcode() validation ----

test_that("non-interactive session throws error", {
  local_mocked_bindings(.interactive = function() FALSE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(c("text"), c("A", "B")),
    "can only be used in an interactive R session"
  )
})

test_that("non-character/non-dataframe data throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(data = 123, c("A", "B")),
    "data must be a character vector or data frame"
  )
})

test_that("no classification variables throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(data = c("text1")),
    "At least one classification variable must be provided"
  )
})

test_that("non-character classification variable throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(data = c("text1"), 1:3),
    "All classification arguments must be character vectors"
  )
})

test_that("empty string in category values throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(data = c("text1"), c("A", "")),
    "Empty strings are not allowed as category values"
  )
})

test_that("duplicate category values throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(data = c("text1"), c("A", "A")),
    "Duplicate categories are not allowed"
  )
})

test_that("missing value overlapping category throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(
      data = c("text1"),
      c("foo", "bar"),
      missing = c("foo")
    ),
    "Missing values cannot overlap with category values"
  )
})

test_that("start vector longer than 1 throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(data = c("text1"), c("A", "B"), start = c(1, 2)),
    "start must be a single value"
  )
})

test_that("invalid start string throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(data = c("text1"), c("A", "B"), start = "banana"),
    "start must be numeric, first_empty, or all_empty"
  )
})

test_that("non-logical randomize throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(data = c("text1"), c("A", "B"), randomize = "yes"),
    "randomize must be a single logical value"
  )
})

test_that("non-logical context throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(data = c("text1"), c("A", "B"), context = 1),
    "context must be TRUE, FALSE"
  )
})

test_that("pre with wrong length throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(
      data = c("t1", "t2", "t3"),
      c("A", "B"),
      pre = c("x", "y")
    ),
    "pre must have the same length as data"
  )
})

test_that("post with wrong length throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(
      data = c("t1", "t2", "t3"),
      c("A", "B"),
      post = c("x", "y")
    ),
    "post must have the same length as data"
  )
})

test_that("colors argument throws error in categorial mode", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(
      data = c("text1"),
      c("A", "B"),
      colors = list(left = "#10b981", right = "#dc2626")
    ),
    "colors is not supported in categorial annotation"
  )
})

test_that("context FLEX is accepted without error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  local_mocked_bindings(runApp = function(...) invisible(NULL), .package = "shiny")
  expect_no_error(
    handcodeR:::handcode(data = c("text1"), c("A", "B"), context = "FLEX")
  )
})

test_that("missing overlapping across multiple category vectors throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(
      data = c("text1"),
      c("A", "B"),
      c("C", "D"),
      missing = c("C")
    ),
    "Missing values cannot overlap with category values"
  )
})

# ---- binary: handcode_binary() validation ----

test_that("non-interactive session throws error (binary)", {
  local_mocked_bindings(.interactive = function() FALSE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(c("text"), c("Yes", "No")),
    "can only be used in an interactive R session"
  )
})

test_that("non-character/non-dataframe data throws error (binary)", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(data = 123, c("Yes", "No")),
    "data must be a character vector or data frame"
  )
})

test_that("no binary variables throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(data = c("text1")),
    "At least one binary classification variable must be provided"
  )
})

test_that("binary variable with more than 2 values throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(data = c("text1"), c("Yes", "No", "Maybe")),
    "All binary classification arguments must be character vectors with exactly two values"
  )
})

test_that("binary variable with only 1 value throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(data = c("text1"), c("Yes")),
    "All binary classification arguments must be character vectors with exactly two values"
  )
})

test_that("empty string in binary values throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(data = c("text1"), c("Yes", "")),
    "Empty strings are not allowed as binary values"
  )
})

test_that("start vector longer than 1 throws error (binary)", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(data = c("text1"), c("Yes", "No"), start = c(1, 2)),
    "start must be a single value"
  )
})

test_that("invalid start string throws error (binary)", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(data = c("text1"), c("Yes", "No"), start = "banana"),
    "start must be numeric, first_empty, or all_empty"
  )
})

test_that("non-logical randomize throws error (binary)", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(data = c("text1"), c("Yes", "No"), randomize = "yes"),
    "randomize must be a single logical value"
  )
})

test_that("non-logical context throws error (binary)", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(data = c("text1"), c("Yes", "No"), context = 1),
    "context must be TRUE, FALSE"
  )
})

test_that("non-logical multifactorial throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(data = c("text1"), c("Yes", "No"), multifactorial = "yes"),
    "multifactorial must be a single logical value"
  )
})

test_that("non-logical enable_numeric throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(data = c("text1"), c("Yes", "No"), enable_numeric = 1),
    "enable_numeric must be a single logical value"
  )
})

test_that("enable_numeric with more than 9 variables throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(
      data = c("text1"),
      c("a", "b"), c("a", "b"), c("a", "b"), c("a", "b"), c("a", "b"),
      c("a", "b"), c("a", "b"), c("a", "b"), c("a", "b"), c("a", "b"),
      enable_numeric = TRUE
    ),
    "enable_numeric = TRUE supports at most 9 classification variables"
  )
})

test_that("invalid left hex color throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(
      data = c("text1"), c("Yes", "No"),
      colors = list(left = "notahex")
    ),
    "valid 6-digit hex color"
  )
})

test_that("invalid right hex color throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(
      data = c("text1"), c("Yes", "No"),
      colors = list(right = "#GGGGGG")
    ),
    "valid 6-digit hex color"
  )
})

test_that("pre with wrong length throws error (binary)", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(
      data = c("t1", "t2", "t3"),
      c("Yes", "No"),
      pre = c("x", "y")
    ),
    "pre must have the same length as data"
  )
})

test_that("post with wrong length throws error (binary)", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(
      data = c("t1", "t2", "t3"),
      c("Yes", "No"),
      post = c("x", "y")
    ),
    "post must have the same length as data"
  )
})

test_that("missing with length > 1 throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(
      data = c("text1"),
      c("Yes", "No"),
      missing = c("NA", "Other")
    ),
    "Missing argument must be a single value"
  )
})

test_that("context FLEX is accepted without error (binary)", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  local_mocked_bindings(runApp = function(...) invisible(NULL), .package = "shiny")
  expect_no_error(
    handcodeR:::handcode_binary(data = c("text1"), c("Yes", "No"), context = "FLEX")
  )
})

# ---- comparison: .check_comparison_* ----

test_that(".check_comparison_args errors when comparison is NULL", {
  expect_error(
    handcodeR:::.check_comparison_args(NULL, c("t1", "t2")),
    "comparison must be provided"
  )
})

test_that(".check_comparison_args errors when comparison is not character", {
  expect_error(
    handcodeR:::.check_comparison_args(1:2, c("t1", "t2")),
    "comparison must be a character vector"
  )
})

test_that(".check_comparison_args errors when comparison length mismatches data", {
  expect_error(
    handcodeR:::.check_comparison_args(c("c1"), c("t1", "t2")),
    "comparison must have the same length as data"
  )
})

test_that(".check_comparison_args accepts valid comparison vector", {
  expect_no_error(
    handcodeR:::.check_comparison_args(c("c1", "c2"), c("t1", "t2"))
  )
})

test_that(".check_comparison_col errors when comparison column absent", {
  df <- data.frame(texts = c("a", "b"), cat1 = c("", ""), stringsAsFactors = FALSE)
  expect_error(
    handcodeR:::.check_comparison_col(df),
    "comparison column"
  )
})

test_that(".check_comparison_col accepts df with comparison column", {
  df <- data.frame(texts = "a", comparison = "c", stringsAsFactors = FALSE)
  expect_no_error(handcodeR:::.check_comparison_col(df))
})

test_that(".check_comparison_context errors when pre_comparison wrong length", {
  expect_error(
    handcodeR:::.check_comparison_context(pre_comparison = c("a"), post_comparison = NULL, n_rows = 3),
    "pre_comparison must have the same length as data"
  )
})

test_that(".check_comparison_context errors when post_comparison wrong length", {
  expect_error(
    handcodeR:::.check_comparison_context(pre_comparison = NULL, post_comparison = c("a"), n_rows = 3),
    "post_comparison must have the same length as data"
  )
})

test_that(".check_comparison_context accepts NULL pre/post (no context columns)", {
  expect_no_error(
    handcodeR:::.check_comparison_context(NULL, NULL, 3)
  )
})

test_that(".check_comparison_context accepts correctly sized pre/post", {
  expect_no_error(
    handcodeR:::.check_comparison_context(c("a", "b", "c"), c("x", "y", "z"), 3)
  )
})
