library(testthat)
library(handcodeR)



# button_output -----------------------------------------------------------

test_that("Test button_output", {

  classification <- list(
    kat1 = c("", "_Not applicable_", "category 1", "category 2")
  )

  res_label <- button_output(classification, 1, names = FALSE)
  res_names <- button_output(classification, 1, names = TRUE)
  res_out <- button_output(classification, 2, names = TRUE)

  # Label output is list
  expect_equal(class(res_label), "list")

  # Names output is character vector
  expect_equal(class(res_names), "character")

  # Names output is equal to first element of classification
  expect_equal(res_names, classification[[1]])

  # Second item of label output is html
  expect_equal(class(res_label[[2]]), c("html", "character"))

  # Out of boundary return is ""
  expect_equal(res_out, "")
})


# gen_output --------------------------------------------------------------

test_that("Test gen_output", {

  data <- data.frame(id = c(2,1,3),
                     before = c("Text 1", "", "Text 2"),
                     after = c("Text 3", "Text 2", ""),
                     texts = c("Text 2", "Text 1", "Text 3"),
                     fruits = factor("", levels = c("", "Not applicable", "apple", "banana", "pear")),
                     colors = factor("", levels = c("", "Not applicable", "green", "yellow")))

  values <- list(code1 = factor(c("apple", "apple", "banana"),
                                levels = c("", "Not applicable", "apple", "banana", "pear")),
                 code2 = factor(c("green", "green", "yellow"),
                                levels = c("", "Not applicable", "green", "yellow")),
                 code3 = c("", "", ""),
                 code4 = c("", "", ""),
                 code5 = c("", "", ""),
                 code6 = c("", "", ""))

  output <- gen_output(data, values)

  # Output is data.frame of correct dimensions
  expect_equal(class(output), "data.frame")
  expect_equal(dim(output), c(nrow(data), ncol(data)-3))

  # Output is in order
  expect_true(all(output$texts == data[order(data$id),"texts"]))

  # Correct names
  expect_equal(names(output), names(data)[-c(1,2,3)])

})


# character_to_data -------------------------------------------------------

test_that("Test character_to_data", {

  data <- c("Text 1", "Text 2", "Text 3")
  arg_list <- list(fruits = c("apple", "banana", "pear"),
                   colors = c("green", "yellow"))
  missing <- c("Not applicable")

  output <- character_to_data(data, arg_list, missing)

  # Output is dataframe
  expect_equal(class(output), "data.frame")

  # Expect dimensions 3x3
  expect_equal(dim(output), c(length(data),length(arg_list)+1))

  # Row 1 is character
  expect_equal(class(output[,1]), "character")

  # Row 2 and 3 are factor
  expect_equal(class(output[,2]), "factor")
  expect_equal(class(output[,3]), "factor")

  # Row 1 is equal data input
  expect_equal(output[,1], data)

  # Names
  expect_equal(names(output), c("texts", names(arg_list)))

  # Factor levels
  expect_contains(levels(output[,2]), "_Not applicable_")
  expect_contains(levels(output[,2]), "")
  expect_contains(levels(output[,2]), arg_list[[1]])

})


# data_for_app ------------------------------------------------------------


test_that("Test 1 data_for_app", {

  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     fruits = factor("", levels = c("", "Not applicable", "apple", "banana", "pear")),
                     colors = factor("", levels = c("", "Not applicable ", "green", "yellow")))
  start <- "first_empty"
  randomize <- FALSE
  context <- FALSE

  a <- data_for_app(data, start, randomize, context)

  # Output is list
  expect_equal(class(a), "list")

  # Names
  expect_equal(names(a), c("container", "data_app", "start_app", "classifications", "context_app"))

  # Container
  expect_equal(dim(a$container), c(nrow(data), 6))

  # Data
  expect_equal(names(a$data_app), c("id", "before", "after", names(data)))

  # Start
  expect_equal(a$start_app, 1)

  # Classifications
  expect_equal(names(a$classifications), names(data)[-1])
  expect_equal(a$classifications[[1]], levels(data[,2]))

  # Context
  expect_equal(a$context_app, context)

  # Ranomize
  expect_true(all(a$data_app$id == seq_len(nrow(a$data_app))))
})



test_that("Test 2 data_for_app", {

  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3", "Text 4"),
                     fruits = factor(c("", "apple", "", ""), levels = c("", "Not applicable", "apple", "banana", "pear")),
                     colors = factor(c("", "green", "", ""), levels = c("", "Not applicable ", "green", "yellow")),
                     taste = factor(c("", "sour", "", ""), levels = c("", "Not applicable", "sweet", "sour")))
  start <- "all_empty"
  randomize <- TRUE
  context <- TRUE

  a <- data_for_app(data, start, randomize, context)

  # Output is list
  expect_equal(class(a), "list")

  # Names
  expect_equal(names(a), c("container", "data_app", "start_app", "classifications", "context_app"))

  # Container
  expect_equal(dim(a$container), c(nrow(data), 6))

  # Data
  expect_equal(names(a$data_app), c("id", "before", "after", names(data)))

  # Start
  expect_equal(a$start_app, 2)
  expect_equal(a$data_app$id[1], 2)

  # Classifications
  expect_equal(names(a$classifications), names(data)[-1])
  expect_equal(a$classifications[[3]], levels(data[,4]))

  # Context
  expect_equal(a$context_app, context)

  # Randomize
  expect_false(all(a$data_app$id == seq_len(nrow(a$data_app))))
})



test_that("Test 3 data_for_app", {
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3", "Text 4"),
                     fruits = factor(c("", "apple", "", ""), levels = c("", "Not applicable", "apple", "banana", "pear")))
  start <- 2
  randomize <- FALSE
  context <- TRUE
  pre <- c("pre 1", "pre 2", "pre 3", "pre 4")
  post <- c("post 1", "post 2", "post 3", "post 4")

  a <- data_for_app(data, start, randomize, context, pre, post)

  # pre and post in data_app as before and after
  expect_equal(a$data_app$before, pre)
  expect_equal(a$data_app$after, post)

  # Start is 2
  expect_equal(a$start_app, 2)
})









