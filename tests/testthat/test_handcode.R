library(testthat)
library(handcodeR)


# data is data.frame or character vector
test_that("handcode() throws error if data is not vector or data.frame",{
  # Arrange
  data <- list(texts = c("Text 1", "Text 2", "Text 3"), cat1 = c("cat1a", "cat1b"))

  # Act and Assert
  expect_error(handcode(data), "data must be a character vector of texts")
})


test_that("handcode() throws error if data is not vector or data.frame",{
  # Arrange
  data <- c(1, 2, 3)

  # Act and Assert
  expect_error(handcode(data, cat1 = c("cat1a", "cat1b")), "data must be a character vector of texts")
})


test_that("handcode() throws an error when empty vector as data is given", {
  # Arrange
  data <- c()
  categories <- c("cat1a", "cat1b")

  # Act and Assert
  expect_error(handcode(data, cat = categories), "data must be a character vector of texts you want to annotate or a data")
})



# start is numeric or one of "first_empty" and "all_empty"
test_that("handcode() throws error when start is not numeric", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data, start = "a"), "start must be numeric")
})

# start is a single value
test_that("handcode() throws error when more than one start value is given", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data, start = c(1, 2)), "start must be a single")
})


# context is logical
test_that("handcode() throws error if context is not logical", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data, context = 3), "context must be either TRUE")
})


# context is a single value
test_that("handcode() throws error if more than one context value is given", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data, context = c(TRUE, FALSE)), "context must be a single")
})


# randomize is logical
test_that("handcode() throws error if context is not logical", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data, randomize = 3), "randomize must be either TRUE")
})


# randomize is a single value
test_that("handcode() throws error if more than one context value is given", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data, randomize = c(TRUE, FALSE)), "randomize must be a single")
})


# All arguments in ... are named character vectors
test_that("handcode() throws error when arg_list is not a named character vector", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories1 <- c("cat1a", "cat1b")
  categories2 <- list("cat2a", "cat2b", "cat2c")

  # Act and Assert
  expect_error(handcode(data, categories1, categories2), "All arguments in ... must be named character vectors.")
})


# Between 1 and 3 arguments are given in ...
test_that("handcode() throws error when there are more than 3 named character vectors", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories1 <- c("cat1a", "cat1b")
  categories2 <- c("cat2a", "cat2b", "cat2c")
  categories3 <- c("cat3a", "cat3b", "cat3c")
  categories4 <- c("cat4a", "cat4b", "cat4c")

  # Act and Assert
  expect_error(handcode(data, cat1 = categories1, cat2 = categories2, cat3 = categories3, cat4 = categories4), "If data is a character vector of texts to annotate, you must provide between 1 and 3 named")
})

test_that("handcode() throws error when there are fewer than 1 named character vectors", {
  # Arrange
  data <- c("text1", "text2", "text3")

  # Act and Assert
  expect_error(handcode(data), "If data is a character vector of texts to annotate, you must provide between 1 and 3 named")
})


# No empty vectors in categories
test_that("handcode() throws error when empty texts vector is given", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories <- c()

  # Act and Assert
  expect_error(handcode(data, cat = categories), "All arguments in ... must be named character vectors")
})


# "" and "not applicable" are not in list of categories
test_that("handcode() throws error when \"\" or \"Not applicable\" are given as categories", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories1 <- c("", "cat1", "cat2")
  categories2 <- c("Not applicable", "cat1", "cat2")

  # Act and Assert
  expect_error(handcode(data, categories1), "The default categories")
  expect_error(handcode(data, categories2), "The default categories")
})


# no duplicates in list of categories
test_that("handcode() throws error when duplicate categories are given", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories <- c("cat1", "cat1", "cat2")

  # Act and Assert
  expect_error(handcode(data, categories), "You cannot set duplicate categories for a variable. Please provide unique categories for classification")
})

# if data is data.frame, first column is texts
test_that("handcode() throws error when first row of data.frame is not texts", {
  # Arrange
  data <- data.frame(words = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("", "Not applicable", "cat1a", "cat1b")))

  # Act and Assert
  expect_error(handcode(data), "data must be a character vector of texts you want to annotate or a data.frame")
})

# if data is data.frame, texts column is character
test_that("handcode() throws error when first row of data.frame is not texts", {
  # Arrange
  data <- data.frame(texts = c(1,2,3,4),
                     cat1 = factor("", levels = c("", "Not applicable", "cat1a", "cat1b")))

  # Act and Assert
  expect_error(handcode(data), "data must be a character vector of texts you want to annotate or a data.frame")
})

# if data is data.frame, annotation vectors are factors
test_that("handcode() throws error when first row of data.frame is not texts", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = "")

  # Act and Assert
  expect_error(handcode(data), "data must be a character vector of texts you want to annotate or a data.frame")
})

# if data is data.frame, between 1 and 3 annotation vectors
test_that("handcode() throws error when less than 1 classification variables are specified", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"))

  # Act and Assert
  expect_error(handcode(data), "data must be a character vector of texts you want to annotate or a data.frame")
})


test_that("handcode() throws an error when more than 3 classification variables are specified", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")),
                     cat3 = factor("", levels = c("cat3a", "cat3b", "", "Not applicable")),
                     cat4 = factor("", levels = c("cat4a", "cat4b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data), "data must be a character vector of texts you want to annotate or a data.frame")
})


# no uncoded data
test_that("handcode() thows error if no uncoded data", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("cat1a", levels = c("cat1a", "cat1b", "", "Not applicable")))
  expect_error(handcode(data), "All your data is already classified")
})



# check error message if interactive = FALSE
if(!interactive()){
  test_that("handcode() throws error if session is not interactive", {
    data <- data.frame(texts = c("Text 1", "Text 2"),
                       cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")))
    expect_error(handcode(data), "can only be used in an interactive R session")
  })
}



# check if output is dataframe
if(interactive()){
test_that("handcode() output is dataframe with dimensions and variable names identical to input", {
  skip_on_cran()
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")))
  out <- handcode(data)

  expect_s3_class(out, "data.frame")
  expect_equal(names(out), names(data))
  expect_equal(dim(out), dim(data))
})
}
