# handcode_binary() is intentionally unexported (Shiny entry point).
# Tests mock handcodeR:::.interactive to bypass the session guard.
# Validation errors fire before any Shiny call, so no browser is needed.

test_that("non-interactive session throws error", {
  local_mocked_bindings(.interactive = function() FALSE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(c("text"), c("Yes", "No")),
    "can only be used in an interactive R session"
  )
})

test_that("non-character/non-dataframe data throws error", {
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

test_that("start vector longer than 1 throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(data = c("text1"), c("Yes", "No"), start = c(1, 2)),
    "start must be a single value"
  )
})

test_that("invalid start string throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(data = c("text1"), c("Yes", "No"), start = "banana"),
    "start must be numeric, first_empty, or all_empty"
  )
})

test_that("non-logical randomize throws error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(data = c("text1"), c("Yes", "No"), randomize = "yes"),
    "randomize must be a single logical value"
  )
})

test_that("non-logical context throws error", {
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

test_that("pre with wrong length throws error", {
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

test_that("post with wrong length throws error", {
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

test_that("context FLEX is accepted without error", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  local_mocked_bindings(runApp = function(...) invisible(NULL), .package = "shiny")
  expect_error(
    handcodeR:::handcode_binary(data = c("text1"), c("Yes", "No"), context = "FLEX"),
    NA
  )
})