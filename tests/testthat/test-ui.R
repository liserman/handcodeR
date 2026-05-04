# UI builder and app launcher smoke tests.
# UI builders return htmltools tag structures; launchers are exercised with mocked runApp.
# Launcher tests guarded with skip_on_cran() because they drive shiny::shinyApp().

# ---- .build_app_shell ----

test_that(".build_app_shell returns a shiny tag or tag list", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  body     <- shiny::div("body content")
  panels   <- shiny::div("panels content")
  result   <- handcodeR:::.build_app_shell(app_data, "Test Title", body, panels)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that(".build_app_shell HTML contains prev and next buttons", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  html     <- paste(as.character(handcodeR:::.build_app_shell(app_data, "T", shiny::div(), shiny::div())), collapse = "")
  expect_true(grepl("id=\"prev\"",   html, fixed = TRUE))
  expect_true(grepl("id=\"next\"",   html, fixed = TRUE))
})

test_that(".build_app_shell HTML contains save_exit button", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  html     <- paste(as.character(handcodeR:::.build_app_shell(app_data, "T", shiny::div(), shiny::div())), collapse = "")
  expect_true(grepl("id=\"save_exit\"", html, fixed = TRUE))
})

test_that(".build_app_shell HTML contains quicksave button when save_loc is set", {
  save_loc <- list(dir = tempdir(), prefix = "p")
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")),
                             save_loc = save_loc)
  html     <- paste(as.character(handcodeR:::.build_app_shell(app_data, "T", shiny::div(), shiny::div())), collapse = "")
  expect_true(grepl("id=\"quicksave\"", html, fixed = TRUE))
})

test_that(".build_app_shell HTML omits quicksave button when save_loc is NULL", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")),
                             save_loc = NULL)
  html     <- paste(as.character(handcodeR:::.build_app_shell(app_data, "T", shiny::div(), shiny::div())), collapse = "")
  expect_false(grepl("id=\"quicksave\"", html, fixed = TRUE))
})

test_that(".build_app_shell embeds the supplied title", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  html     <- as.character(handcodeR:::.build_app_shell(app_data, "My Custom Title", shiny::div(), shiny::div()))
  expect_true(grepl("My Custom Title", html, fixed = TRUE))
})

# ---- .build_categorial_ui ----

test_that(".build_categorial_ui returns a shiny tag or tag list", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  result   <- handcodeR:::.build_categorial_ui(app_data)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that(".build_categorial_ui HTML contains handcodeR - Categorial title", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  html     <- paste(as.character(handcodeR:::.build_categorial_ui(app_data)), collapse = "")
  expect_true(grepl("Categorial", html, fixed = TRUE))
})

# ---- .build_binary_ui ----

test_that(".build_binary_ui returns a shiny tag or tag list", {
  app_data <- make_app_data(mode = "binary", n = 2, vars = list(lr = c("L", "R")))
  result   <- handcodeR:::.build_binary_ui(app_data)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that(".build_binary_ui HTML contains Binary title", {
  app_data <- make_app_data(mode = "binary", n = 2, vars = list(lr = c("L", "R")))
  html     <- paste(as.character(handcodeR:::.build_binary_ui(app_data)), collapse = "")
  expect_true(grepl("Binary", html, fixed = TRUE))
})

test_that(".binary_styles injects colors used by .build_binary_ui", {
  # Test the style generator directly — the full fluidPage render doesn't expand nested CSS text
  app_data <- make_app_data(mode = "binary", n = 2, vars = list(lr = c("L", "R")))
  styles   <- paste(as.character(handcodeR:::.binary_styles(app_data$colors)), collapse = "")
  expect_true(grepl("#10b981", styles, fixed = TRUE))
  expect_true(grepl("#dc2626", styles, fixed = TRUE))
})

# ---- .build_comparison_ui ----

test_that(".build_comparison_ui returns a shiny tag or tag list", {
  app_data <- make_app_data(mode = "comparison", n = 2, vars = list(cat1 = c("A", "B")))
  result   <- handcodeR:::.build_comparison_ui(app_data)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that(".build_comparison_ui HTML contains Comparison title", {
  app_data <- make_app_data(mode = "comparison", n = 2, vars = list(cat1 = c("A", "B")))
  html     <- paste(as.character(handcodeR:::.build_comparison_ui(app_data)), collapse = "")
  expect_true(grepl("Comparison", html, fixed = TRUE))
})

test_that(".build_comparison_ui HTML contains comparison-col CSS class", {
  app_data <- make_app_data(mode = "comparison", n = 2, vars = list(cat1 = c("A", "B")))
  html     <- paste(as.character(handcodeR:::.build_comparison_ui(app_data)), collapse = "")
  expect_true(grepl("comparison-col", html, fixed = TRUE))
})

# ---- app launcher dispatch ----

test_that(".run_categorial_app calls shiny::runApp once", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  called    <- 0L
  local_mocked_bindings(runApp = function(...) { called <<- called + 1L; invisible(NULL) }, .package = "shiny")
  handcodeR:::.run_categorial_app(app_data, autosave = FALSE)
  expect_equal(called, 1L)
})

test_that(".run_binary_app calls shiny::runApp once", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "binary", n = 2, vars = list(lr = c("L", "R")))
  called    <- 0L
  local_mocked_bindings(runApp = function(...) { called <<- called + 1L; invisible(NULL) }, .package = "shiny")
  handcodeR:::.run_binary_app(app_data, autosave = FALSE)
  expect_equal(called, 1L)
})

test_that(".run_comparison_app calls shiny::runApp once", {
  skip_on_cran()
  app_data  <- make_app_data(mode = "comparison", n = 2, vars = list(cat1 = c("A", "B")))
  called    <- 0L
  local_mocked_bindings(runApp = function(...) { called <<- called + 1L; invisible(NULL) }, .package = "shiny")
  handcodeR:::.run_comparison_app(app_data, autosave = FALSE)
  expect_equal(called, 1L)
})
