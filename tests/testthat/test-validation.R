# handcode() is intentionally unexported (Shiny entry point).
# Tests mock handcodeR:::.interactive to bypass the session guard.
# Validation errors fire before any Shiny call, so no browser is needed.

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
  expect_error(
    handcodeR:::handcode(data = c("text1"), c("A", "B"), context = "FLEX"),
    NA
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