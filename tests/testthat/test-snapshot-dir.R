# Quicksave setup + argument-handling tests. Quicksave file-writing is covered in test-server.R.

# ============================================================================ #
# Setup: .quicksave_setup                                                      #
# ---------------------------------------------------------------------------- #
# Resolves the quicksave argument: FALSE disables, a path enables + locates.   #
# ============================================================================ #

test_that(".quicksave_setup returns NULL when quicksave is FALSE", {
  expect_null(handcodeR:::.quicksave_setup(FALSE, "mydata"))
})

test_that(".quicksave_setup returns NULL when quicksave is NULL", {
  expect_null(handcodeR:::.quicksave_setup(NULL, "mydata"))
})

test_that(".quicksave_setup errors when quicksave is a bare TRUE", {
  expect_error(handcodeR:::.quicksave_setup(TRUE, "mydata"), "FALSE or a path")
})

test_that(".quicksave_setup errors when quicksave is an empty/blank string", {
  expect_error(handcodeR:::.quicksave_setup("   ", "mydata"), "FALSE or a path")
})

test_that(".quicksave_setup errors when directory does not exist", {
  missing_dir <- file.path(tempfile(), "nope")
  expect_error(handcodeR:::.quicksave_setup(missing_dir, "mydata"), "does not exist")
})

test_that(".quicksave_setup does not create a missing directory", {
  missing_dir <- file.path(tempfile(), "nope")
  try(handcodeR:::.quicksave_setup(missing_dir, "mydata"), silent = TRUE)
  expect_false(dir.exists(missing_dir))
})

test_that(".quicksave_setup returns dir and sanitized prefix for an existing directory", {
  tmp_dir <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  result  <- handcodeR:::.quicksave_setup(tmp_dir, "my data$1")
  expect_equal(result$dir, normalizePath(tmp_dir))
  expect_equal(result$prefix, "my_data_1")
})

# ============================================================================ #
# Integration: quicksave argument on the entry points                          #
# ---------------------------------------------------------------------------- #
# Default and invalid-path handling across both entry points.                  #
# ============================================================================ #

test_that("handcode quicksave default is FALSE", {
  expect_identical(formals(handcodeR:::handcode)[["quicksave"]], FALSE)
})

test_that("handcode_binary quicksave default is FALSE", {
  expect_identical(formals(handcodeR:::handcode_binary)[["quicksave"]], FALSE)
})

test_that("handcode errors when quicksave = TRUE carries no path", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(c("text1", "text2"), cat = c("A", "B"), quicksave = TRUE),
    "FALSE or a path"
  )
})

test_that("handcode errors when quicksave points to a non-existent directory", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode(c("text1", "text2"), cat = c("A", "B"),
                         quicksave = file.path(tempfile(), "nope")),
    "does not exist"
  )
})

test_that("handcode_binary errors when quicksave = TRUE carries no path", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(c("t1", "t2"), lr = c("L", "R"), quicksave = TRUE),
    "FALSE or a path"
  )
})

test_that("handcode_binary errors when quicksave points to a non-existent directory", {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_error(
    handcodeR:::handcode_binary(c("t1", "t2"), lr = c("L", "R"),
                                quicksave = file.path(tempfile(), "nope")),
    "does not exist"
  )
})
