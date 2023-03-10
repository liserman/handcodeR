library(testthat)
library(handcodeR)


# Text if function returns dataframe
test_that("init_data function returns a dataframe", {
  # Arrange
  texts <- c("text1", "text2", "text3")
  categories1 <- c("cat1a", "cat1b")
  categories2 <- c("cat2a", "cat2b", "cat2c")

  # Act
  output <- init_data(texts, categories1, categories2)

  # Assert
  expect_s3_class(output, "data.frame")
})

# Check if error when text is not character vector
test_that("init_data function throws an error when texts is not a character vector", {
  # Arrange
  texts <- list("text1", "text2", "text3")
  categories1 <- c("cat1a", "cat1b")

  # Act and Assert
  expect_error(init_data(texts, categories1), "texts must be character vector")
})

# Check if error when arg_list not named character vector
test_that("init_data function throws an error when arg_list is not a named character vector", {
  # Arrange
  texts <- c("text1", "text2", "text3")
  categories1 <- c("cat1a", "cat1b")
  categories2 <- list("cat2a", "cat2b", "cat2c")

  # Act and Assert
  expect_error(init_data(texts, categories1, categories2), "All arguments in ... must be named character vectors.")
})

# Check if error when too many variables
test_that("init_data function throws an error when there are more than 3 named character vectors", {
  # Arrange
  texts <- c("text1", "text2", "text3")
  categories1 <- c("cat1a", "cat1b")
  categories2 <- c("cat2a", "cat2b", "cat2c")
  categories3 <- c("cat3a", "cat3b", "cat3c")
  categories4 <- c("cat4a", "cat4b", "cat4c")

  # Act and Assert
  expect_error(init_data(texts, categories1, categories2, categories3, categories4), "There must be between 1 and 3 named character vectors given.")
})

# Check if error when too few variables
test_that("init_data function throws an error when there are fewer than 1 named character vectors", {
  # Arrange
  texts <- c("text1", "text2", "text3")

  # Act and Assert
  expect_error(init_data(texts), "There must be between 1 and 3 named character vectors given.")
})

# Check if output is as expected - 1 categorization
test_that("init_data function correctly initializes dataframe with one categorization", {
  # Arrange
  texts <- c("text1", "text2", "text3")
  categories1 <- c("cat1a", "cat1b")

  # Act
  output <- init_data(texts, categories1 = categories1)

  # Assert
  expect_equal(ncol(output), 2)
  expect_identical(colnames(output), c("texts", "categories1"))
  expect_identical(levels(output$categories1), c("", "Not applicable", "cat1a", "cat1b"))
})

# Check if output is as expected - 2 categorizations
test_that("init_data function correctly initializes dataframe with two categorizations", {
  # Arrange
  texts <- c("text1", "text2", "text3")
  categories1 <- c("cat1a", "cat1b")
  categories2 <- c("cat2a", "cat2b", "cat2c")

  # Act
  output <- init_data(texts, categories1 = categories1, categories2 = categories2)

  # Assert
  expect_equal(ncol(output), 3)
  expect_identical(colnames(output), c("texts", "categories1", "categories2"))
  expect_identical(levels(output$categories1), c("", "Not applicable", "cat1a", "cat1b"))
  expect_identical(levels(output$categories2), c("", "Not applicable", "cat2a", "cat2b", "cat2c"))
})

# Check if output is as expected - 3 categorizations
test_that("init_data function correctly initializes dataframe with two categorizations", {
  # Arrange
  texts <- c("text1", "text2", "text3")
  categories1 <- c("cat1a", "cat1b")
  categories2 <- c("cat2a", "cat2b", "cat2c")
  categories3 <- c("cat3a", "cat3b", "cat3c", "cat3d")

  # Act
  output <- init_data(texts, categories1 = categories1, categories2 = categories2, categories3 = categories3)

  # Assert
  expect_equal(ncol(output), 4)
  expect_identical(colnames(output), c("texts", "categories1", "categories2", "categories3"))
  expect_identical(levels(output$categories1), c("", "Not applicable", "cat1a", "cat1b"))
  expect_identical(levels(output$categories2), c("", "Not applicable", "cat2a", "cat2b", "cat2c"))
  expect_identical(levels(output$categories3), c("", "Not applicable", "cat3a", "cat3b", "cat3c", "cat3d"))
})



# Check with empty vector for variables
test_that("init_data function throws an error when empty vector as variable is given", {
  # Arrange
  texts <- c()
  categories <- c("cat1a", "cat1b")

  # Act and Assert
  expect_error(init_data(texts, categories), "texts must be character vector indicating the texts you whish to classify")
})



# Check with empty vector for texts
test_that("init_data function throws an error when empty texts vector is given", {
  # Arrange
  texts <- c("text1", "text2", "text3")
  categories <- c()

  # Act and Assert
  expect_error(init_data(texts, categories), "All arguments in ... must be named character vectors")
})


# Check with duplicate names for variables
test_that("init_data function gives dataframe if duplicate names for variables are given", {
  # Arrange
  texts <- c("text1", "text2", "text3")
  categories1 <- c("cat1a", "cat1b")
  categories2 <- c("cat2a", "cat2b", "cat2c")

  # Act
  output <- init_data(texts = texts, categories = categories1, categories = categories2)

  # Assert
  expect_s3_class(output, "data.frame")
  expect_equal(ncol(output), 3)

})



# Check with duplicate categories
test_that("init_data function throws an error when duplicate categories are given", {
  # Arrange
  texts <- c("text1", "text2", "text3")
  categories <- c("cat1", "cat1", "cat2")

  # Act and Assert
  expect_error(init_data(texts, categories), "You cannot set duplicate categories for a variable. Please provide unique categories for classification")
})



# Check with "" or "Not applicable" as categories
test_that("init_data function throws an error when \"\" or \"Not applicable\" are given as categories", {
  # Arrange
  texts <- c("text1", "text2", "text3")
  categories1 <- c("", "cat1", "cat2")
  categories2 <- c("Not applicable", "cat1", "cat2")

  # Act and Assert
  expect_error(init_data(texts, categories1), "The default categories")
  expect_error(init_data(texts, categories2), "The default categories")
})



# Check with longer vector of texts
test_that("init_data function gives dataframe when longer vector of texts is given", {
  # Arrange
  texts <- rep(c("text1", "text2", "text3"), 10000)
  categories1 <- c("cat1a", "cat1b")
  categories2 <- c("cat2a", "cat2b", "cat2c")

  # Act
  output <- init_data(texts = texts, categories1, categories2)

  # Assert
  expect_s3_class(output, "data.frame")
  expect_equal(dim(output), c(30000,3))

})


# Check with empty strings in texts
test_that("init_data function gives dataframe when texts contains empty strings", {
  # Arrange
  texts <- c("", "text2", "text3")
  categories1 <- c("cat1a", "cat1b")
  categories2 <- c("cat2a", "cat2b", "cat2c")

  # Act
  output <- init_data(texts = texts, categories1, categories2)

  # Assert
  expect_s3_class(output, "data.frame")
  expect_equal(dim(output), c(3,3))

})

