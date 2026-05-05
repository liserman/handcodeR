# Shiny server reactive tests via shiny::testServer(). All blocks skip_on_cran()
# because reactive timing can be flaky on minimal CRAN check environments.

# ---- categorial server ----

test_that("categorial server: initial counter equals start_val", {
  skip_on_cran()
  app_data   <- make_app_data(mode = "categorial", n = 3, vars = list(cat1 = c("A", "B")))
  server_fn  <- handcodeR:::.categorial_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    expect_equal(values$counter, 1L)
  })
})

test_that("categorial server: annotations initialised to empty strings", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "categorial", n = 3, vars = list(cat1 = c("A", "B")))
  server_fn <- handcodeR:::.categorial_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    expect_true(all(values$annotations$cat1 == ""))
  })
})

test_that("categorial server: next advances counter", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "categorial", n = 3, vars = list(cat1 = c("A", "B")))
  server_fn <- handcodeR:::.categorial_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    session$setInputs(`next` = 1)
    expect_equal(values$counter, 2L)
  })
})

test_that("categorial server: prev at row 1 stays at 1", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "categorial", n = 3, vars = list(cat1 = c("A", "B")))
  server_fn <- handcodeR:::.categorial_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    session$setInputs(prev = 1)
    expect_equal(values$counter, 1L)
  })
})

test_that("categorial server: next at last row stays at N", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "categorial", n = 3, vars = list(cat1 = c("A", "B")),
                             start_val = 3L)
  server_fn <- handcodeR:::.categorial_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    session$setInputs(`next` = 1)
    expect_equal(values$counter, 3L)
  })
})

test_that("categorial server: prev after next returns to row 1", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "categorial", n = 3, vars = list(cat1 = c("A", "B")))
  server_fn <- handcodeR:::.categorial_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    session$setInputs(`next` = 1)
    session$setInputs(prev = 1)
    expect_equal(values$counter, 1L)
  })
})

test_that("categorial server: radio input writes to annotations", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "categorial", n = 3, vars = list(cat1 = c("A", "B")))
  server_fn <- handcodeR:::.categorial_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    # Simulate user clicking "A" for cat1 on row 1
    session$setInputs(class_cat1 = "A")
    # Navigate to flush the save_current handler
    session$setInputs(`next` = 1)
    expect_equal(values$annotations$cat1[1], "A")
  })
})

test_that("categorial server: progress_text renders row info", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "categorial", n = 3, vars = list(cat1 = c("A", "B")))
  server_fn <- handcodeR:::.categorial_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    expect_true(grepl("Row 1 of 3", output$progress_text))
  })
})

test_that("categorial server: quicksave without save_loc shows warning notification", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "categorial", n = 3, vars = list(cat1 = c("A", "B")),
                             save_loc = NULL)
  server_fn <- handcodeR:::.categorial_server(app_data, autosave = FALSE)
  notification_shown <- FALSE
  shiny::testServer(server_fn, {
    local_mocked_bindings(
      showNotification = function(...) { notification_shown <<- TRUE; invisible(NULL) },
      .package = "shiny"
    )
    session$setInputs(quicksave = 1)
    expect_true(notification_shown)
  })
})

# ---- binary server ----

test_that("binary server: initial counter equals start_val", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "binary", n = 3, vars = list(lr = c("L", "R")))
  server_fn <- handcodeR:::.binary_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    expect_equal(values$counter, 1L)
  })
})

test_that("binary server: left button click writes left value to annotations", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "binary", n = 3, vars = list(lr = c("L", "R")))
  server_fn <- handcodeR:::.binary_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    session$setInputs(btn_lr_1 = 1)
    expect_equal(values$annotations$lr[1], "L")
  })
})

test_that("binary server: right button click writes right value to annotations", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "binary", n = 3, vars = list(lr = c("L", "R")))
  server_fn <- handcodeR:::.binary_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    session$setInputs(btn_lr_2 = 1)
    expect_equal(values$annotations$lr[1], "R")
  })
})

test_that("binary server: missing button click writes missing sentinel to annotations", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "binary", n = 3, vars = list(lr = c("L", "R")))
  server_fn <- handcodeR:::.binary_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    session$setInputs(btn_lr_missing = 1)
    expect_equal(values$annotations$lr[1], "_Not applicable_")
  })
})

test_that("binary server: next advances counter", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "binary", n = 3, vars = list(lr = c("L", "R")))
  server_fn <- handcodeR:::.binary_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    session$setInputs(`next` = 1)
    expect_equal(values$counter, 2L)
  })
})

test_that("binary server multifactorial=FALSE forces other vars to right when one is set left", {
  skip_on_cran()
  app_data <- make_app_data(mode = "binary", n = 3,
                            vars = list(lr1 = c("L1", "R1"), lr2 = c("L2", "R2")))
  app_data$multifactorial <- FALSE
  server_fn <- handcodeR:::.binary_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    session$setInputs(btn_lr1_1 = 1)
    expect_equal(values$annotations$lr2[1], "R2")
  })
})

# ---- comparison server ----

test_that("comparison server: initial counter equals start_val", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "comparison", n = 3, vars = list(cat1 = c("A", "B")))
  server_fn <- handcodeR:::.comparison_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    expect_equal(values$counter, 1L)
  })
})

test_that("comparison server: comparison_text output renders the comparison column", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "comparison", n = 3, vars = list(cat1 = c("A", "B")))
  server_fn <- handcodeR:::.comparison_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    rendered <- output$comparison_text
    expect_true(any(grepl("comp1", as.character(rendered))))
  })
})

test_that("comparison server: FLEX context toggle updates show_context", {
  skip_on_cran()
  app_data        <- make_app_data(mode = "comparison", n = 3, vars = list(cat1 = c("A", "B")),
                                   context = "FLEX")
  server_fn       <- handcodeR:::.comparison_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    session$setInputs(show_context_check = TRUE)
    expect_true(values$show_context)
    session$setInputs(show_context_check = FALSE)
    expect_false(values$show_context)
  })
})

# ---- shared save handlers ----

test_that("save handler: autosave=FALSE writes no file on session end", {
  skip_on_cran()
  tmp_dir    <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  save_loc   <- list(dir = tmp_dir, prefix = "test_save")
  app_data   <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")),
                               save_loc = save_loc)
  server_fn  <- handcodeR:::.categorial_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    session$close()
  })
  expect_equal(length(list.files(tmp_dir, pattern = "_autosave\\.RData$")), 0L)
})

test_that("save handler: autosave=TRUE writes autosave file on unexpected close", {
  skip_on_cran()
  tmp_dir    <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  save_loc   <- list(dir = tmp_dir, prefix = "test_save")
  app_data   <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")),
                               save_loc = save_loc)
  server_fn  <- handcodeR:::.categorial_server(app_data, autosave = TRUE)
  shiny::testServer(server_fn, {
    session$close()
  })
  expect_equal(length(list.files(tmp_dir, pattern = "_autosave\\.RData$")), 1L)
})

test_that("save handler: quicksave writes timestamped file when save_loc is set", {
  skip_on_cran()
  tmp_dir    <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  save_loc   <- list(dir = tmp_dir, prefix = "qs_test")
  app_data   <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")),
                               save_loc = save_loc)
  server_fn  <- handcodeR:::.categorial_server(app_data, autosave = FALSE)
  shiny::testServer(server_fn, {
    session$setInputs(quicksave = 1)
  })
  qs_files <- list.files(tmp_dir, pattern = "_quicksave_[0-9]+\\.RData$")
  expect_equal(length(qs_files), 1L)
})
