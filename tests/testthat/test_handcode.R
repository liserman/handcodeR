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


# Between 1 and 6 arguments are given in ...
test_that("handcode() throws error when there are more than 5 named character vectors", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories1 <- c("cat1a", "cat1b")
  categories2 <- c("cat2a", "cat2b", "cat2c")
  categories3 <- c("cat3a", "cat3b", "cat3c")
  categories4 <- c("cat4a", "cat4b", "cat4c")
  categories5 <- c("cat5a", "cat5b", "cat5c")
  categories6 <- c("cat6a", "cat6b", "cat6c")
  categories7 <- c("cat7a", "cat7b", "cat7c")

  # Act and Assert
  expect_error(handcode(data, cat1 = categories1, cat2 = categories2, cat3 = categories3, cat4 = categories4, cat5 = categories5, cat6 = categories6, cat7 = categories7), "If data is a character vector of texts to annotate, you must provide between 1 and 6 named")
})

test_that("handcode() throws error when there are fewer than 1 named character vectors", {
  # Arrange
  data <- c("text1", "text2", "text3")

  # Act and Assert
  expect_error(handcode(data), "If data is a character vector of texts to annotate, you must provide between 1 and 6 named")
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
test_that("handcode() throws error when \"\" is given as category", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories <- c("", "cat1", "cat2")

  # Act and Assert
  expect_error(handcode(data, categories), "The default missing value")
})

# Missing must be character vector
test_that("handcode() throws error when missing is not character vector", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories <- c("banana", "apple")
  missing <- 4

  # Act and Assert
  expect_error(handcode(data, categories = categories, missing = missing), "missing must be a character vector")
})


# No duplicates between categories and missing values
test_that("handcode() throws error when duplicate between missing and categories", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories <- c("banana", "apple", "pear")
  missing <- c("NA", "banana")

  # Act and Assert
  expect_error(handcode(data, categories = categories, missing = missing), "cannot be similar to values")
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
  expect_error(handcode(data), "is currently only able to handle between")
})


test_that("handcode() throws an error when more than 6 classification variables are specified", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "_Not applicable_")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "_Not applicable_")),
                     cat3 = factor("", levels = c("cat3a", "cat3b", "", "_Not applicable_")),
                     cat4 = factor("", levels = c("cat4a", "cat4b", "", "_Not applicable_")),
                     cat5 = factor("", levels = c("cat5a", "cat5b", "", "_Not applicable_")),
                     cat6 = factor("", levels = c("cat6a", "cat6b", "", "_Not applicable_")),
                     cat7 = factor("", levels = c("cat7a", "cat7b", "", "_Not applicable_")))

  # Act and Assert
  expect_error(handcode(data), "is currently only able to handle between")
})


# no uncoded data
test_that("handcode() throws error if no uncoded data", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("cat1a", levels = c("cat1a", "cat1b", "", "_Not applicable_")))
  expect_error(handcode(data), "All your data is already classified")
})


# Pre is not null or character
test_that("handcode() throws error if pre is wrong class", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("", "_Not applicable_", "cat1a", "cat1b")))
  expect_error(
    handcode(data, pre = list("Text 0", "Text 1")),
    "pre and post must be character"
  )

})

# Post is not null or character
test_that("handcode() throws error if pre is wrong class", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("", "_Not applicable_", "cat1a", "cat1b")))
  expect_error(
    handcode(data, post = list("Text 2", "Text 3")),
    "pre and post must be character"
  )

})


# Pre has wrong length
test_that("handcode() throws error if pre is wrong class", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("", "_Not applicable_", "cat1a", "cat1b")))
  expect_error(
    handcode(data, pre = c("Text 0", "Text 1", "Text 2")),
    "pre and post must be of the same length"
  )

})

# Post has wrong length
test_that("handcode() throws error if pre is wrong class", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("", "_Not applicable_", "cat1a", "cat1b")))
  expect_error(
    handcode(data, post = c("Text 2")),
    "pre and post must be of the same length"
  )

})


# check error message if interactive = FALSE
if(!interactive()){
  test_that("handcode() throws error if session is not interactive", {
    data <- data.frame(texts = c("Text 1", "Text 2"),
                       cat1 = factor("", levels = c("cat1a", "cat1b", "", "_Not applicable_")))
    expect_error(handcode(data), "can only be used in an interactive R session")
  })
}


# check if output is dataframe
if(interactive()){
test_that("handcode() output is dataframe with dimensions and variable names identical to input", {
  skip_on_cran()
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("", "_Not applicable_", "cat1a", "cat1b")))
  out <- handcode(data)

  expect_s3_class(out, "data.frame")
  expect_equal(names(out), names(data))
  expect_equal(dim(out), dim(data))
})
}


# check if output dataframe matches input dataframe for continued coding
if(interactive()){
  test_that("handcode() output matches already coded to input", {
    skip_on_cran()
    data <- data.frame(texts = c("Text 1", "Text 2", "Text 3", "Text 4", "Text 5", "Text 6", "Text 7"),
                       cat1 = factor(c("cat1a", "cat1a", "cat1b", "cat1a", "cat1b", "", ""),
                                     levels = c("", "_Not applicable_", "cat1a", "cat1b")))
    out <- handcode(data)

    expect_s3_class(out, "data.frame")
    expect_equal(names(out), names(data))
    expect_equal(dim(out), dim(data))
    expect_equal(out[1:5,], data[1:5,])
  })
}


# check if output dataframe matches input dataframe for continued coding, randomize true
if(interactive()){
  test_that("handcode() output matches already coded to input when randomize TRUE", {
    skip_on_cran()
    data <- data.frame(texts = c("Text 1", "Text 2", "Text 3", "Text 4", "Text 5", "Text 6", "Text 7"),
                       cat1 = factor(c("cat1a", "cat1a", "cat1b", "cat1a", "cat1b", "", ""),
                                     levels = c("", "_Not applicable_", "cat1a", "cat1b")))
    out <- handcode(data, randomize = T, start = "all_empty")

    expect_s3_class(out, "data.frame")
    expect_equal(names(out), names(data))
    expect_equal(dim(out), dim(data))
    expect_equal(out[1:5,], data[1:5,])
  })
}


# check if output dataframe matches input dataframe for continued coding, randomize true, empty row in between
if(interactive()){
  test_that("handcode() output matches already coded to input when randomize TRUE", {
    skip_on_cran()
    data <- data.frame(texts = c("Text 1", "Text 2", "Text 3", "Text 4", "Text 5", "Text 6", "Text 7"),
                       cat1 = factor(c("cat1a", "", "cat1b", "cat1a", "cat1b", "cat1a", "cat1b"),
                                     levels = c("", "_Not applicable_", "cat1a", "cat1b")))
    out <- handcode(data, randomize = T, start = "all_empty")

    expect_s3_class(out, "data.frame")
    expect_equal(names(out), names(data))
    expect_equal(dim(out), dim(data))
    expect_equal(out[c(1,3,4,5,6,7),], data[c(1,3,4,5,6,7),])
  })
}

