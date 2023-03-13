library(testthat)
library(handcodeR)


# Input must be dataframe
test_that("handcode function throws an error when data is not a dataframe", {
  # Arrange
  data <- list(texts = c("Text 1", "Text 2", "Text 3"), cat1 = c("cat1a", "cat1b"))

  # Act and Assert
  expect_error(handcode(data), "data must be a dataframe initialized via")
})


# First column must be texts
test_that("handcode function throws an error when first row of data is not texts", {
  # Arrange
  data <- data.frame(words = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("", "Not applicable", "cat1a", "cat1b")))

  # Act and Assert
  expect_error(handcode(data), "First column of data must be character vector named texts")
})


# All columns but first one must be factor
test_that("handcode function throws an error when classification variables are not factor", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("", "Not applicable", "cat1a", "cat1b")),
                     cat2 = c("", "cat2a", "cat2b"))

  # Act and Assert
  expect_error(handcode(data), "All columns except the first one in the dataframe must be factors")
})


# Minimum of one classification variable
test_that("handcode function throws an error when less than 1 classification variables are specified", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"))

  # Act and Assert
  expect_error(handcode(data), "The dataframe must contain at least one classification")
})

# Maximum of 3 classification variables
test_that("handcode function throws an error when more than 3 classification variables are specified", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")),
                     cat3 = factor("", levels = c("cat3a", "cat3b", "", "Not applicable")),
                     cat4 = factor("", levels = c("cat4a", "cat4b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data), "The dataframe can have at most three classification")
})

# Numeric input to start
test_that("handcode function throws an error when start is not numeric", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data, start = "a"), "start must be numeric")
})

# Single input to start
test_that("handcode function throws an error when more than one start value is given", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data, start = c(1, 2)), "start must be a single")
})

# Check error if no uncoded data
test_that("check handcode() thows error if no uncoded data", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("cat1a", levels = c("cat1a", "cat1b", "", "Not applicable")))
  expect_error(handcode(data), "All your data is already classified")
})

# check error message if interactive = FALSE
if(!interactive()){
  test_that("check handcode() throws error if session is not interactive", {
    data <- data.frame(texts = c("Text 1", "Text 2"),
                       cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")))
    expect_error(handcode(data), "can only be used in an interactive R session")
  })
}

# check if output is dataframe
if(interactive()){
test_that("check handcode() output is dataframe with dimensions and variable names identical to input", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")))
  out <- handcode(data)

  expect_s3_class(out, "data.frame")
  expect_equal(names(out), names(data))
  expect_equal(dim(out), dim(data))
})
}
