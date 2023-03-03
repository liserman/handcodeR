context("check-init_data")
library(testthat)
library(handcodeR)

# Check stop if texts is wrong input
test_that("init_data() requires text to be character vector", {
  expect_error(
    init_data(texts = c(1,2,3), categories = c("eins", "zwei")),
    "texts must be character vector"
  )
})

test_that("init_data() requires text to be vector", {
  expect_error(
    init_data(texts = list("eins", "zwei", "drei"), categories = c("eins", "zwei")),
    "texts must be character vector"
  )
})


# Check stop if categories is wrong input
test_that("init_data() requires categories to be character vector", {
  expect_error(
    init_data(texts = c("eins", "zwei", "drei"), categories = c(1, 2)),
    "categories must be character vector"
  )
})

test_that("init_data() requires categories to be vector", {
  expect_error(
    init_data(texts = c("eins", "zwei", "drei"), categories = list("eins", "zwei")),
    "categories must be character vector"
  )
})

# Check if output is dataframe
test_that("init_data() returns a data frame", {
  output <- init_data(texts = c("eins", "zwei"), categories = c("eins", "zwei"))
  expect_equal(class(output), "data.frame")
})

# Check if output has correct dimensions
test_that("init_data() returns a data frame with dimensions length(texts), length(categories) + 2", {
  output <- init_data(texts = c("eins", "zwei"), categories = c("eins", "zwei"))
  expect_equal(dim(output), c(2, 4))
})

# Check if categories are translated to lower case
test_that("init_data() transfers categories to lowercase", {
  output <- init_data(texts = c("eins", "zwei"), categories = c("Eins", "Zwei"))
  expect_equal(names(output)[2:3], c("eins", "zwei"))
})

# Check if spaces in categories are replaced by _
test_that("init_data() replaces spaces in categories with _", {
  output <- init_data(texts = c("eins", "zwei"), categories = c("e ins", "z wei"))
  expect_equal(names(output)[2:3], c("e_ins", "z_wei"))
})






