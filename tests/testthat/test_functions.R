library(testthat)
library(handcodeR)



# button_output -----------------------------------------------------------

test_that("Test button_output", {

  classification <- list(
    kat1 = c("", "_Not applicable_", "_Undecided_", "category 1", "category 2")
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

  # Third item of label output is html
  expect_equal(class(res_label[[3]]), c("html", "character"))

  # Out of boundary return is ""
  expect_equal(res_out, "")
})


# gen_output --------------------------------------------------------------

test_that("Test 1 gen_output", {

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
                 code6 = c("", "", ""),
                 notes = c("", "", ""))

  output <- gen_output(data, values)

  # Output is data.frame of correct dimensions
  expect_equal(class(output), "data.frame")
  expect_equal(dim(output), c(nrow(data), ncol(data)-3))

  # Output is in order
  expect_true(all(output$texts == data[order(data$id),"texts"]))

  # Correct names
  expect_equal(names(output), names(data)[-c(1,2,3)])

})

test_that("Test 2 gen_output", {

  data <- data.frame(id = c(2,1,3),
                     before_comparison = c("Comparison 1", "", "Comparison 2"),
                     after_comparison = c("Comparison 3", "Comparison 2", ""),
                     before = c("Text 1", "", "Text 2"),
                     after = c("Text 3", "Text 2", ""),
                     texts = c("Text 2", "Text 1", "Text 3"),
                     comparison = c("Comparison 2", "Comparison 1", "Comparison 3"),
                     fruits = factor("", levels = c("", "Not applicable", "apple", "banana", "pear")),
                     colors = factor("", levels = c("", "Not applicable", "green", "yellow")))

  values <- list(code1 = factor(c("apple", "apple", "banana"),
                                levels = c("", "Not applicable", "apple", "banana", "pear")),
                 code2 = factor(c("green", "green", "yellow"),
                                levels = c("", "Not applicable", "green", "yellow")),
                 code3 = c("", "", ""),
                 code4 = c("", "", ""),
                 code5 = c("", "", ""),
                 code6 = c("", "", ""),
                 notes = c("", "", ""))

  output <- gen_output(data, values)

  # Output is data.frame of correct dimensions
  expect_equal(class(output), "data.frame")
  expect_equal(dim(output), c(nrow(data), ncol(data)-5))

  # Output is in order
  expect_true(all(output$texts == data[order(data$id),"texts"]))

  # Correct names
  expect_equal(names(output), names(data)[-c(1,2,3,4,5)])

  # Comparison is attached
  expect_true("comparison" %in% names(output))

})


test_that("Test 3 gen_output", {

  data <- data.frame(id = c(2,1,3),
                     before_comparison = c("Comparison 1", "", "Comparison 2"),
                     after_comparison = c("Comparison 3", "Comparison 2", ""),
                     before = c("Text 1", "", "Text 2"),
                     after = c("Text 3", "Text 2", ""),
                     texts = c("Text 2", "Text 1", "Text 3"),
                     comparison = c("Comparison 2", "Comparison 1", "Comparison 3"),
                     fruits = factor("", levels = c("", "Not applicable", "apple", "banana", "pear")),
                     colors = factor("", levels = c("", "Not applicable", "green", "yellow")),
                     notes = "")

  values <- list(code1 = factor(c("apple", "apple", "banana"),
                                levels = c("", "Not applicable", "apple", "banana", "pear")),
                 code2 = factor(c("green", "green", "yellow"),
                                levels = c("", "Not applicable", "green", "yellow")),
                 code3 = c("", "", ""),
                 code4 = c("", "", ""),
                 code5 = c("", "", ""),
                 code6 = c("", "", ""),
                 notes = c("", "", ""))

  output <- gen_output(data, values)

  # Output is data.frame of correct dimensions
  expect_equal(class(output), "data.frame")
  expect_equal(dim(output), c(nrow(data), ncol(data)-5))

  # Output is in order
  expect_true(all(output$texts == data[order(data$id),"texts"]))

  # Correct names
  expect_equal(names(output), names(data)[-c(1,2,3,4,5)])

  # Comparison is attached
  expect_true("comparison" %in% names(output))

  # Notes in output
  expect_true("notes" %in% names(output))

  # Notes is character in output
  expect_equal(class(output$notes), "character")
})


# character_to_data -------------------------------------------------------

test_that("Test 1 character_to_data", {

  data <- c("Text 1", "Text 2", "Text 3")
  arg_list <- list(fruits = c("apple", "banana", "pear"),
                   colors = c("green", "yellow"))
  missing <- c("Not applicable")

  output <- character_to_data(data = data, arg_list = arg_list, missing = missing, add_notes = FALSE)

  # Output is dataframe
  expect_equal(class(output), "data.frame")

  # Expect dimensions 3x3
  expect_equal(dim(output), c(length(data),length(arg_list)+1))

  # Column 1 is character
  expect_equal(class(output[,1]), "character")

  # Column 2 and 3 are factor
  expect_equal(class(output[,2]), "factor")
  expect_equal(class(output[,3]), "factor")

  # Column 1 is equal data input
  expect_equal(output[,1], data)

  # Names
  expect_equal(names(output), c("texts", names(arg_list)))

  # Factor levels
  expect_contains(levels(output[,2]), "_Not applicable_")
  expect_contains(levels(output[,2]), "")
  expect_contains(levels(output[,2]), arg_list[[1]])

})


test_that("Test 2 character_to_data", {

  data <- c("Text 1", "Text 2", "Text 3")
  comparison <- c("Comparison 1", "Comparison 2", "Comparison 3")

  arg_list <- list(fruits = c("apple", "banana", "pear"),
                   colors = c("green", "yellow"))
  missing <- c("Not applicable")

  output <- character_to_data(data = data, comparison = comparison, arg_list = arg_list, missing = missing, add_notes = FALSE)

  # Output is dataframe
  expect_equal(class(output), "data.frame")

  # Expect dimensions 3x4
  expect_equal(dim(output), c(length(data),length(arg_list)+2))

  # Column 1 is character
  expect_equal(class(output[,1]), "character")

  # Column 2 is character
  expect_equal(class(output[,2]), "character")

  # Column 3 and 4 are factor
  expect_equal(class(output[,3]), "factor")
  expect_equal(class(output[,4]), "factor")

  # Column 1 is equal data input
  expect_equal(output[,1], data)

  # Column 2 is equal comparison input
  expect_equal(output[,2], comparison)

  # Names
  expect_equal(names(output), c("texts", "comparison", names(arg_list)))

  # Factor levels
  expect_contains(levels(output[,3]), "_Not applicable_")
  expect_contains(levels(output[,3]), "")
  expect_contains(levels(output[,3]), arg_list[[1]])

})

test_that("Test 3 character_to_data", {

  data <- c("Text 1", "Text 2", "Text 3")
  comparison <- c("Comparison 1", "Comparison 2", "Comparison 3")

  arg_list <- list(fruits = c("apple", "banana", "pear"),
                   colors = c("green", "yellow"))
  missing <- c("Not applicable")

  output <- character_to_data(data = data, comparison = comparison, arg_list = arg_list, missing = missing, add_notes = TRUE)

  # Output is dataframe
  expect_equal(class(output), "data.frame")

  # Expect dimensions 3x4
  expect_equal(dim(output), c(length(data),length(arg_list)+3))

  # Column 1 is character
  expect_equal(class(output[,1]), "character")

  # Column 2 is character
  expect_equal(class(output[,2]), "character")

  # Column 3 and 4 are factor
  expect_equal(class(output[,3]), "factor")
  expect_equal(class(output[,4]), "factor")

  # Column 5 is character
  expect_equal(class(output[,5]), "character")

  # Column 1 is equal data input
  expect_equal(output[,1], data)

  # Column 2 is equal comparison input
  expect_equal(output[,2], comparison)

  # Names
  expect_equal(names(output), c("texts", "comparison", names(arg_list), "notes"))

  # Factor levels
  expect_contains(levels(output[,3]), "_Not applicable_")
  expect_contains(levels(output[,3]), "")
  expect_contains(levels(output[,3]), arg_list[[1]])

  # Notes is empty
  expect_equal(output[,5], c("", "", ""))
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
  expect_equal(names(a), c("container", "data_app", "start_app", "classifications", "context_app", "compare", "add_notes"))

  # Container
  expect_equal(dim(a$container), c(nrow(data), 7))

  # Data
  expect_equal(names(a$data_app), c("id", "before", "after", names(data)))

  # Start
  expect_equal(a$start_app, 1)

  # Classifications
  expect_equal(names(a$classifications), names(data)[-1])
  expect_equal(a$classifications[[1]], levels(data[,2]))

  # Context
  expect_equal(a$context_app, context)

  # Randomize
  expect_true(all(a$data_app$id == seq_len(nrow(a$data_app))))

  # Compare boolean
  expect_false(a$compare)
  expect_false(a$add_notes)

})



test_that("Test 2 data_for_app", {

  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3", "Text 4"),
                     fruits = factor(c("", "apple", "", ""), levels = c("", "Not applicable", "apple", "banana", "pear")),
                     colors = factor(c("", "green", "", ""), levels = c("", "Not applicable ", "green", "yellow")),
                     taste = factor(c("", "sour", "", ""), levels = c("", "Not applicable", "sweet", "sour")),
                     notes = c("", "", "", ""))
  start <- "all_empty"
  randomize <- TRUE
  context <- TRUE

  a <- data_for_app(data, start, randomize, context)

  # Output is list
  expect_equal(class(a), "list")

  # Names
  expect_equal(names(a), c("container", "data_app", "start_app", "classifications", "context_app", "compare", "add_notes"))

  # Container
  expect_equal(dim(a$container), c(nrow(data), 7))

  # Data
  expect_equal(names(a$data_app), c("id", "before", "after", names(data)))

  # Start
  expect_equal(a$start_app, 2)
  expect_equal(a$data_app$id[1], 2)

  # Classifications
  expect_equal(names(a$classifications), names(data)[-c(1,5)])
  expect_equal(a$classifications[[3]], levels(data[,4]))

  # Context
  expect_equal(a$context_app, context)

  # Randomize
  expect_false(all(a$data_app$id == seq_len(nrow(a$data_app))))

  # Compare boolean
  expect_false(a$compare)
  expect_true(a$add_notes)
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

  # Compare boolean
  expect_false(a$compare)
})


test_that("Test 4 data_for_app", {
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3", "Text 4"),
                     fruits = factor(c("banana", "", "apple", "pear"), levels = c("", "Not applicable", "apple", "banana", "pear")),
                     notes = c("", "", "may be a pear", ""))
  start <- "all_empty"
  randomize <- TRUE
  context <- TRUE
  pre <- c("pre 1", "pre 2", "pre 3", "pre 4")
  post <- c("post 1", "post 2", "post 3", "post 4")

  a <- data_for_app(data, start, randomize, context, pre, post)

  # Start is 4
  expect_equal(a$start_app, 4)

  # Last row of data is Text 2
  expect_equal(a$data_app$id[a$start], 2)

  # Number of rows of data is 4
  expect_equal(nrow(a$data_app), 4)

  # Compare boolean
  expect_false(a$compare)
  expect_true(a$add_notes)

  # Notes in data_app
  expect_true("notes" %in% names(a$data_app))
})


test_that("Test 5 data_for_app", {

  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3"),
                     comparison = c("Comparison 1", "Comparison 2", "Comparison 3"),
                     fruits = factor("", levels = c("", "Not applicable", "apple", "banana", "pear")),
                     colors = factor("", levels = c("", "Not applicable ", "green", "yellow")))
  start <- "first_empty"
  randomize <- FALSE
  context <- FALSE

  a <- data_for_app(data, start, randomize, context)

  # Output is list
  expect_equal(class(a), "list")

  # Names
  expect_equal(names(a), c("container", "data_app", "start_app", "classifications", "context_app", "compare", "add_notes"))

  # Container
  expect_equal(dim(a$container), c(nrow(data), 7))

  # Data
  expect_equal(names(a$data_app), c("id", "before_comparison", "after_comparison", "before", "after", names(data)))

  # Start
  expect_equal(a$start_app, 1)

  # Classifications
  expect_equal(names(a$classifications), names(data)[-c(1,2)])
  expect_equal(a$classifications[[1]], levels(data[,3]))

  # Context
  expect_equal(a$context_app, context)

  # Randomize
  expect_true(all(a$data_app$id == seq_len(nrow(a$data_app))))

  # Compare boolean
  expect_true(a$compare)
})



test_that("Test 6 data_for_app", {

  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3", "Text 4"),
                     comparison = c("Comparison 1", "Comparison 2", "Comparison 3", "Comparison 4"),
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
  expect_equal(names(a), c("container", "data_app", "start_app", "classifications", "context_app", "compare", "add_notes"))

  # Container
  expect_equal(dim(a$container), c(nrow(data), 7))

  # Data
  expect_equal(names(a$data_app), c("id", "before_comparison", "after_comparison", "before", "after", names(data)))

  # Start
  expect_equal(a$start_app, 2)
  expect_equal(a$data_app$id[1], 2)

  # Classifications
  expect_equal(names(a$classifications), names(data)[-c(1,2)])
  expect_equal(a$classifications[[3]], levels(data[,5]))

  # Context
  expect_equal(a$context_app, context)

  # Randomize
  expect_false(all(a$data_app$id == seq_len(nrow(a$data_app))))

  # Compare boolean
  expect_true(a$compare)
})



test_that("Test 7 data_for_app", {
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3", "Text 4"),
                     comparison = c("Comparison 1", "Comparison 2", "Comparison 3", "Comparison 4"),
                     fruits = factor(c("", "apple", "", ""), levels = c("", "Not applicable", "apple", "banana", "pear")))
  start <- 2
  randomize <- FALSE
  context <- TRUE
  pre <- c("pre 1", "pre 2", "pre 3", "pre 4")
  post <- c("post 1", "post 2", "post 3", "post 4")
  comparison_pre <- c("comp pre 1", "comp pre 2", "comp pre 3", "comp pre 4")
  comparison_post <- c("comp post 1", "comp post 2", "comp post 3", "comp post 4")


  a <- data_for_app(data, start, randomize, context, pre, post, comparison_pre, comparison_post)

  # pre and post in data_app as before and after
  expect_equal(a$data_app$before, pre)
  expect_equal(a$data_app$after, post)
  expect_equal(a$data_app$before_comparison, comparison_pre)
  expect_equal(a$data_app$after_comparison, comparison_post)

  # Start is 2
  expect_equal(a$start_app, 2)

  # Compare boolean
  expect_true(a$compare)
})


test_that("Test 8 data_for_app", {
  data <- data.frame(texts = c("Text 1", "Text 2", "Text 3", "Text 4"),
                     comparison = c("Comparison 1", "Comparison 2", "Comparison 3", "Comparison 4"),
                     fruits = factor(c("banana", "", "apple", "pear"), levels = c("", "Not applicable", "apple", "banana", "pear")))
  start <- "all_empty"
  randomize <- TRUE
  context <- TRUE
  pre <- c("pre 1", "pre 2", "pre 3", "pre 4")
  post <- c("post 1", "post 2", "post 3", "post 4")
  comparison_pre <- c("comp pre 1", "comp pre 2", "comp pre 3", "comp pre 4")
  comparison_post <- c("comp post 1", "comp post 2", "comp post 3", "comp post 4")

  a <- data_for_app(data, start, randomize, context, pre, post, comparison_pre, comparison_post)

  # Start is 4
  expect_equal(a$start_app, 4)

  # Last Column of data is Text 2
  expect_equal(a$data_app$id[a$start], 2)

  # Number of rows of data is 4
  expect_equal(nrow(a$data_app), 4)

  # Compare boolean
  expect_true(a$compare)
})



