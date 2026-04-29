library(testthat)
library(handcodeR)


# data is data.frame or character vector
test_that("handcode() throws error if data is not vector or data.frame",{
  # Arrange
  data <- list(texts = c("Text 1", "Text 2", "Text 3"), cat1 = c("cat1a", "cat1b"))

  # Act and Assert
  expect_error(handcode(data), "'data' must be either a character vector of texts or")
})


test_that("handcode() throws error if data is not vector or data.frame",{
  # Arrange
  data <- c(1, 2, 3)

  # Act and Assert
  expect_error(handcode(data, cat1 = c("cat1a", "cat1b")), "'data' must be either a character vector of texts or")
})


test_that("handcode() throws an error when empty vector as data is given", {
  # Arrange
  data <- c()
  categories <- c("cat1a", "cat1b")

  # Act and Assert
  expect_error(handcode(data, cat = categories), "'data' must be either a character vector of texts or")
})



# start is numeric or one of "first_empty" and "all_empty"
test_that("handcode() throws error when start is not numeric", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data, start = "a"), "Invalid 'start' value: provide a single")
})

# start is a single value
test_that("handcode() throws error when more than one start value is given", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data, start = c(1, 2)), "Invalid 'start' value: provide a single")
})


# context is logical
test_that("handcode() throws error if context is not logical", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data, context = 3), "Invalid 'context' argument: provide")
})


# context is a single value
test_that("handcode() throws error if more than one context value is given", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data, context = c(TRUE, FALSE)), "Invalid 'context' argument: provide")
})


# randomize is logical
test_that("handcode() throws error if context is not logical", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data, randomize = 3), "Invalid 'randomize' argument: provide")
})


# randomize is a single value
test_that("handcode() throws error if more than one context value is given", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("cat1a", "cat1b", "", "Not applicable")),
                     cat2 = factor("", levels = c("cat2a", "cat2b", "", "Not applicable")))

  # Act and Assert
  expect_error(handcode(data, randomize = c(TRUE, FALSE)), "Invalid 'randomize' argument: provide")
})


# All arguments in ... are named character vectors
test_that("handcode() throws error when arg_list is not a named character vector", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories1 <- c("cat1a", "cat1b")
  categories2 <- list("cat2a", "cat2b", "cat2c")

  # Act and Assert
  expect_error(handcode(data, categories1, categories2), "Arguments passed in '...' must be named character")
})


# Between 1 and 6 arguments are given in ...
test_that("handcode() throws error when there are more than 6 named character vectors", {
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
  expect_error(handcode(data, cat1 = categories1, cat2 = categories2, cat3 = categories3, cat4 = categories4, cat5 = categories5, cat6 = categories6, cat7 = categories7), "You must supply between 1 and 6 named character vectors of annotation")
})

test_that("handcode() throws error when there are fewer than 1 named character vectors", {
  # Arrange
  data <- c("text1", "text2", "text3")

  # Act and Assert
  expect_error(handcode(data), "You must supply between 1 and 6 named character vectors of annotation")
})


# No empty vectors in categories
test_that("handcode() throws error when empty texts vector is given", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories <- c()

  # Act and Assert
  expect_error(handcode(data, cat = categories), "Arguments passed in '...' must be named character ")
})


# "" and "not applicable" are not in list of categories
test_that("handcode() throws error when \"\" is given as category", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories <- c("", "cat1", "cat2")

  # Act and Assert
  expect_error(handcode(data, categories), "The empty string \"\" cannot be used as a category ")
})

# Missing must be character vector
test_that("handcode() throws error when missing is not character vector", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories <- c("banana", "apple")
  missing <- 4

  # Act and Assert
  expect_error(handcode(data, categories = categories, missing = missing), "Invalid 'missing' argument. Provide a ")
})


# No duplicates between categories and missing values
test_that("handcode() throws error when duplicate between missing and categories", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories <- c("banana", "apple", "pear")
  missing <- c("NA", "banana")

  # Act and Assert
  expect_error(handcode(data, categories = categories, missing = missing), "Invalid input: some category values")
})



# no duplicates in list of categories
test_that("handcode() throws error when duplicate categories are given", {
  # Arrange
  data <- c("text1", "text2", "text3")
  categories <- c("cat1", "cat1", "cat2")

  # Act and Assert
  expect_error(handcode(data, categories), "Duplicate categories detected. Ensure each")
})

# if data is data.frame, first column is texts
test_that("handcode() throws error when first row of data.frame is not texts", {
  # Arrange
  data <- data.frame(words = c("Text 1", "Text 2", "Text 3"),
                     cat1 = factor("", levels = c("", "Not applicable", "cat1a", "cat1b")))

  # Act and Assert
  expect_error(handcode(data), "Invalid 'data'. Only a data frame returned")
})

# if data is data.frame, texts column is character
test_that("handcode() throws error when first row of data.frame is not texts", {
  # Arrange
  data <- data.frame(texts = c(1,2,3,4),
                     cat1 = factor("", levels = c("", "Not applicable", "cat1a", "cat1b")))

  # Act and Assert
  expect_error(handcode(data), "Invalid 'data'. Only a data frame returned")
})

# if data is data.frame, annotation vectors are factors
test_that("handcode() throws error when first row of data.frame is not texts", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     cat1 = "")

  # Act and Assert
  expect_error(handcode(data), "Invalid data: all annotation")
})

# if data is data.frame, between 1 and 6 annotation vectors
test_that("handcode() throws error when less than 1 classification variables are specified", {
  # Arrange
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"))

  # Act and Assert
  expect_error(handcode(data), "Invalid input: provide between 1 and 6 ")
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
  expect_error(handcode(data), "Invalid input: provide between 1 and 6 ")
})


# no uncoded data
test_that("handcode() throws error if no uncoded data", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("cat1a", levels = c("cat1a", "cat1b", "", "_Not applicable_")))
  expect_error(handcode(data), "All classification columns are filled.")
})


# Pre is not null or character
test_that("handcode() throws error if pre is wrong class", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("", "_Not applicable_", "cat1a", "cat1b")))
  expect_error(
    handcode(data, pre = list("Text 0", "Text 1")),
    "Invalid 'pre' argument: must"
  )

})

# Post is not null or character
test_that("handcode() throws error if pre is wrong class", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("", "_Not applicable_", "cat1a", "cat1b")))
  expect_error(
    handcode(data, post = list("Text 2", "Text 3")),
    "Invalid 'post' argument: must"
  )

})


# Pre has wrong length
test_that("handcode() throws error if pre is wrong class", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("", "_Not applicable_", "cat1a", "cat1b")))
  expect_error(
    handcode(data, pre = c("Text 0", "Text 1", "Text 2")),
    "'pre' must have the same length as"
  )

})

# Post has wrong length
test_that("handcode() throws error if pre is wrong class", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("", "_Not applicable_", "cat1a", "cat1b")))
  expect_error(
    handcode(data, post = c("Text 2")),
    "'post' must have the same length as"
  )

})


# Comparison is not character
test_that("handcode() throws error if comparison is not character", {
  data <- c("Text 1", "Text 2")
  comparison <- list("Comparison 1", "Comparison 2")

  expect_error(handcode(data = data, comparison = comparison, cat1 = c("cat1a", "cat1b")),
               "Invalid 'comparison' argument: 'comparison'")
})


# Comparison is wrong length
test_that("handcode() throws error if comparison is of wrong length", {
  data <- c("Text 1", "Text 2")
  comparison <- list("Comparison 1", "Comparison 2", "Comparison 3")

  expect_error(handcode(data = data, comparison = comparison, cat1 = c("cat1a", "cat1b")),
               "Invalid 'comparison' argument: 'comparison'")
})

# comparison_pre is not null or character
test_that("handcode() throws error if comparison_pre is wrong class", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("", "_Not applicable_", "cat1a", "cat1b")))
  expect_error(
    handcode(data, comparison_pre = list("Text 0", "Text 1")),
    "Invalid 'comparison_pre' argument: must"
  )
})

# comparison_post is not null or character
test_that("handcode() throws error if comparison_post is wrong class", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("", "_Not applicable_", "cat1a", "cat1b")))
  expect_error(
    handcode(data, comparison_post = list("Text 2", "Text 3")),
    "Invalid 'comparison_post' argument: must"
  )
})


# comparison_pre has wrong length
test_that("handcode() throws error if comparison_pre is wrong class", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("", "_Not applicable_", "cat1a", "cat1b")))
  expect_error(
    handcode(data, comparison_pre = c("Text 0", "Text 1", "Text 2")),
    "'comparison_pre' must have the same length as"
  )
})

# comparison_post has wrong length
test_that("handcode() throws error if comparison_post is wrong class", {
  data <- data.frame(texts = c("Text 1", "Text 2"),
                     cat1 = factor("", levels = c("", "_Not applicable_", "cat1a", "cat1b")))
  expect_error(
    handcode(data, comparison_post = c("Text 2")),
    "'comparison_post' must have the same length as"
  )
})


# Comparison input with data.frame
if(interactive()){
  test_that("comparison is ignored when data is data.frame",{
    skip_on_cran()
    data <- data.frame(texts = c("Text 1", "Text 2", "Text 3", "Text 4", "Text 5", "Text 6", "Text 7"),
                       cat1 = factor(c("cat1a", "cat1a", "cat1b", "cat1a", "cat1b", "", ""),
                                     levels = c("", "_Not applicable_", "cat1a", "cat1b")))

    expect_warning(handcode(data, comparison = c("Comp 1", "Comp 2", "Comp 3", "Comp 4", "Comp 5", "Comp 6", "Comp 7")),
                   "'comparison' was ignored ")
})
}

# Comparison in data and given as additional argument
if(interactive()){
  test_that("comparison is ignored when data is data.frame",{
    skip_on_cran()
    data <- data.frame(texts = c("Text 1", "Text 2", "Text 3", "Text 4", "Text 5", "Text 6", "Text 7"),
                       comparison = c("Comp 1", "Comp 2", "Comp 3", "Comp 4", "Comp 5", "Comp 6", "Comp 7"),
                       cat1 = factor(c("cat1a", "cat1a", "cat1b", "cat1a", "cat1b", "", ""),
                                     levels = c("", "_Not applicable_", "cat1a", "cat1b")))

    expect_warning(handcode(data, comparison = c("Comp 1", "Comp 2", "Comp 3", "Comp 4", "Comp 5", "Comp 6", "Comp 7")),
                   "'comparison' was ignored ")
  })
}

# add_notes not logical
test_that("handcode() throws error if add_notes is wrong class", {
  data <- c("Text 1", "Text 2")

  expect_error(
    handcode(data, cat1 = c("cat1a", "cat1b"), add_notes = "drei"),
    "Invalid 'add_notes' argument"
  )
})


# add_notes wrong length
test_that("handcode() throws error if add_notes is wrong class", {
  data <- c("Text 1", "Text 2")

  expect_error(
    handcode(data, cat1 = c("cat1a", "cat1b"), add_notes = c(TRUE, FALSE)),
    "Invalid 'add_notes' argument"
  )
})



# check error message if interactive = FALSE
if(!interactive()){
  test_that("handcode() throws error if session is not interactive", {
    data <- data.frame(texts = c("Text 1", "Text 2"),
                       cat1 = factor("", levels = c("cat1a", "cat1b", "", "_Not applicable_")))
    expect_error(handcode(data), "only be run in an interactive R session")
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


# check if output dataframe matches input dataframe for continued coding, randomize true, notes
if(interactive()){
  test_that("handcode() output matches already coded to input when randomize TRUE", {
    skip_on_cran()
    data <- data.frame(texts = c("Text 1", "Text 2", "Text 3", "Text 4", "Text 5", "Text 6", "Text 7"),
                       cat1 = factor(c("cat1a", "cat1a", "cat1b", "cat1a", "cat1b", "", ""),
                                     levels = c("", "_Not applicable_", "cat1a", "cat1b")),
                       notes = c("", "", "", "", "review", "", ""))
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


# Check output with comparison given as additional argument
if(interactive()){
  test_that("comparison given as argument",{
    skip_on_cran()
    texts <- c("Text 1", "Text 2", "Text 3", "Text 4", "Text 5", "Text 6", "Text 7")
    comparison <- c("Comp 1", "Comp 2", "Comp 3", "Comp 4", "Comp 5", "Comp 6", "Comp 7")
    out <- handcode(data = texts,
                    cat1 = c("cat1a", "cat2a"),
                    comparison = comparison)

    expect_s3_class(out, "data.frame")
    expect_equal(names(out), c("texts", "comparison", "cat1"))
    expect_equal(nrow(out), 7)
    expect_equal(out[,1], texts)
    expect_equal(out[,2], comparison)
  })
}

# Check output with comparison given in data.frame
if(interactive()){
  test_that("comparison given in dataframe",{
    skip_on_cran()
    data <- data.frame(texts = c("Text 1", "Text 2", "Text 3", "Text 4", "Text 5", "Text 6", "Text 7"),
                       comparison = c("Comp 1", "Comp 2", "Comp 3", "Comp 4", "Comp 5", "Comp 6", "Comp 7"),
                       cat1 = factor(c("cat1a", "cat1a", "cat1b", "cat1a", "cat1b", "", ""),
                                     levels = c("", "_Not applicable_", "cat1a", "cat1b")))

    out <- handcode(data)

    expect_s3_class(out, "data.frame")
    expect_equal(names(out), names(data))
    expect_equal(nrow(out), nrow(data))
    expect_equal(out[,1], data[,1])
    expect_equal(out[,2], data[,2])
  })
}


# Check output with pre, post, comparison_pre and comparison_post
if(interactive()){
  test_that("comparison given as argument",{
    skip_on_cran()
    texts <- c("Text 1", "Text 2", "Text 3", "Text 4", "Text 5", "Text 6", "Text 7")
    comparison <- c("Comp 1", "Comp 2", "Comp 3", "Comp 4", "Comp 5", "Comp 6", "Comp 7")
    pre <- c("pre 1", "pre 2", "pre 3", "pre 4", "pre 5", "pre 6", "pre 7")
    post <- c("post 1", "post 2", "post 3", "post 4", "post 5", "post 6", "post 7")
    comparison_pre <- c("comp pre 1", "comp pre 2", "comp pre 3", "comp pre 4", "comp pre 5", "comp pre 6", "comp pre 7")
    comparison_post <- c("comp post 1", "comp post 2", "comp post 3", "comp post 4", "comp post 5", "comp post 6", "comp post 7")

    out <- handcode(data = texts,
                    cat1 = c("cat1a", "cat2a"),
                    comparison = comparison,
                    pre = pre,
                    post = post,
                    comparison_pre = comparison_pre,
                    comparison_post = comparison_post,
                    context = TRUE,
                    randomize = TRUE)

    expect_s3_class(out, "data.frame")
    expect_equal(names(out), c("texts", "comparison", "cat1", "pre", "post", "comparison_pre", "comparison_post"))
    expect_equal(nrow(out), 7)
    expect_equal(out[,1], texts)
    expect_equal(out[,2], comparison)
    expect_equal(out[,4], pre)
    expect_equal(out[,5], post)
    expect_equal(out[,6], comparison_pre)
    expect_equal(out[,7], comparison_post)
  })
}

