# UI builder smoke tests and app launcher dispatch via mocked shiny::runApp().
# Launcher tests skip_on_cran() because they drive shiny::shinyApp().

# ============================================================================ #
# .build_app_shell                                                             #
# ---------------------------------------------------------------------------- #
# Structural tests for the shared fluidPage wrapper.                           #
# ============================================================================ #

test_that(".build_app_shell returns a shiny tag or tag list", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  body <- shiny::div("body content")
  panels <- shiny::div("panels content")
  result <- handcodeR:::.build_app_shell(app_data, "Test Title", body, panels)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that(".build_app_shell HTML contains prev and next buttons", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  html <- paste(as.character(handcodeR:::.build_app_shell(app_data, "T", shiny::div(), shiny::div())), collapse = "")
  expect_true(grepl("id=\"prev\"", html, fixed = TRUE))
  expect_true(grepl("id=\"next\"", html, fixed = TRUE))
})

test_that(".build_app_shell HTML contains save_exit button", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  html <- paste(as.character(handcodeR:::.build_app_shell(app_data, "T", shiny::div(), shiny::div())), collapse = "")
  expect_true(grepl("id=\"save_exit\"", html, fixed = TRUE))
})

test_that(".build_app_shell HTML contains quicksave button when save_loc is set", {
  save_loc <- list(dir = tempdir(), prefix = "p")
  app_data <- make_app_data(
    mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")),
    save_loc = save_loc
  )
  html <- paste(as.character(handcodeR:::.build_app_shell(app_data, "T", shiny::div(), shiny::div())), collapse = "")
  expect_true(grepl("id=\"quicksave\"", html, fixed = TRUE))
})

test_that(".build_app_shell HTML omits quicksave button when save_loc is NULL", {
  app_data <- make_app_data(
    mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")),
    save_loc = NULL
  )
  html <- paste(as.character(handcodeR:::.build_app_shell(app_data, "T", shiny::div(), shiny::div())), collapse = "")
  expect_false(grepl("id=\"quicksave\"", html, fixed = TRUE))
})

test_that(".build_app_shell embeds the supplied title", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  html <- as.character(handcodeR:::.build_app_shell(app_data, "My Custom Title", shiny::div(), shiny::div()))
  expect_true(grepl("My Custom Title", html, fixed = TRUE))
})

# ============================================================================ #
# .build_categorial_ui                                                         #
# ---------------------------------------------------------------------------- #
# Smoke tests for categorial annotation UI output.                             #
# ============================================================================ #

test_that(".build_categorial_ui returns a shiny tag or tag list", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  result <- handcodeR:::.build_categorial_ui(app_data)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that(".build_categorial_ui HTML contains handcodeR - Categorial title", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  html <- paste(as.character(handcodeR:::.build_categorial_ui(app_data)), collapse = "")
  expect_true(grepl("Categorial", html, fixed = TRUE))
})

# ============================================================================ #
# .build_binary_ui                                                             #
# ---------------------------------------------------------------------------- #
# Smoke tests for binary annotation UI output.                                 #
# ============================================================================ #

test_that(".build_binary_ui returns a shiny tag or tag list", {
  app_data <- make_app_data(mode = "binary", n = 2, vars = list(lr = c("L", "R")))
  result <- handcodeR:::.build_binary_ui(app_data)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that(".build_binary_ui HTML contains Binary title", {
  app_data <- make_app_data(mode = "binary", n = 2, vars = list(lr = c("L", "R")))
  html <- paste(as.character(handcodeR:::.build_binary_ui(app_data)), collapse = "")
  expect_true(grepl("Binary", html, fixed = TRUE))
})

test_that(".binary_styles injects colors used by .build_binary_ui", {
  # Test the style generator directly — the full fluidPage render doesn't expand nested CSS text
  app_data <- make_app_data(mode = "binary", n = 2, vars = list(lr = c("L", "R")))
  styles <- paste(as.character(handcodeR:::.binary_styles(app_data$colors)), collapse = "")
  expect_true(grepl("#10b981", styles, fixed = TRUE))
  expect_true(grepl("#dc2626", styles, fixed = TRUE))
})

# ============================================================================ #
# .build_comparison_ui                                                         #
# ---------------------------------------------------------------------------- #
# Smoke tests for comparison annotation UI output.                             #
# ============================================================================ #

test_that(".build_comparison_ui returns a shiny tag or tag list", {
  app_data <- make_app_data(mode = "comparison", n = 2, vars = list(cat1 = c("A", "B")))
  result <- handcodeR:::.build_comparison_ui(app_data)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that(".build_comparison_ui HTML contains Comparison title", {
  app_data <- make_app_data(mode = "comparison", n = 2, vars = list(cat1 = c("A", "B")))
  html <- paste(as.character(handcodeR:::.build_comparison_ui(app_data)), collapse = "")
  expect_true(grepl("Comparison", html, fixed = TRUE))
})

test_that(".build_comparison_ui HTML contains comparison-col CSS class", {
  app_data <- make_app_data(mode = "comparison", n = 2, vars = list(cat1 = c("A", "B")))
  html <- paste(as.character(handcodeR:::.build_comparison_ui(app_data)), collapse = "")
  expect_true(grepl("comparison-col", html, fixed = TRUE))
})

# ============================================================================ #
# App Launcher Dispatch                                                        #
# ---------------------------------------------------------------------------- #
# Verifies each .run_*_app() calls shiny::runApp() exactly once.               #
# ============================================================================ #

test_that(".run_categorial_app calls shiny::runApp once", {
  skip_on_cran()
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  called <- 0L
  local_mocked_bindings(runApp = function(...) {
    called <<- called + 1L
    invisible(NULL)
  }, .package = "shiny")
  handcodeR:::.run_categorial_app(app_data, autosave = FALSE)
  expect_equal(called, 1L)
})

test_that(".run_binary_app calls shiny::runApp once", {
  skip_on_cran()
  app_data <- make_app_data(mode = "binary", n = 2, vars = list(lr = c("L", "R")))
  called <- 0L
  local_mocked_bindings(runApp = function(...) {
    called <<- called + 1L
    invisible(NULL)
  }, .package = "shiny")
  handcodeR:::.run_binary_app(app_data, autosave = FALSE)
  expect_equal(called, 1L)
})

test_that(".run_comparison_app calls shiny::runApp once", {
  skip_on_cran()
  app_data <- make_app_data(mode = "comparison", n = 2, vars = list(cat1 = c("A", "B")))
  called <- 0L
  local_mocked_bindings(runApp = function(...) {
    called <<- called + 1L
    invisible(NULL)
  }, .package = "shiny")
  handcodeR:::.run_comparison_app(app_data, autosave = FALSE)
  expect_equal(called, 1L)
})

# ============================================================================ #
# .build_cat_keyboard_script                                                   #
# ---------------------------------------------------------------------------- #
# Generates JavaScript for numeric keyboard shortcuts in categorial mode.      #
# ============================================================================ #

test_that(".build_cat_keyboard_script returns a character string", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  app_data$enable_numeric <- FALSE
  result <- handcodeR:::.build_cat_keyboard_script(app_data)
  expect_true(is.character(result) && length(result) == 1L)
})

test_that(".build_cat_keyboard_script always includes Space/Enter handlers", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  app_data$enable_numeric <- FALSE
  result <- handcodeR:::.build_cat_keyboard_script(app_data)
  expect_true(grepl("prev", result, fixed = TRUE))
  expect_true(grepl("next", result, fixed = TRUE))
})

test_that(".build_cat_keyboard_script includes keydown numeric handler when enable_numeric=TRUE", {
  app_data <- make_app_data(mode = "categorial", n = 2, vars = list(cat1 = c("A", "B")))
  app_data$enable_numeric <- TRUE
  result <- handcodeR:::.build_cat_keyboard_script(app_data)
  expect_true(grepl("keydown", result, fixed = TRUE))
  expect_true(grepl("numericVars", result, fixed = TRUE))
})

test_that(".build_cat_keyboard_script embeds variable names in JS array", {
  app_data <- make_app_data(
    mode = "categorial", n = 2,
    vars = list(cat1 = c("A", "B"), cat2 = c("X", "Y"))
  )
  app_data$enable_numeric <- TRUE
  result <- handcodeR:::.build_cat_keyboard_script(app_data)
  expect_true(grepl("cat1", result, fixed = TRUE))
  expect_true(grepl("cat2", result, fixed = TRUE))
})

test_that(".build_cat_keyboard_script embeds raw variable name in CSS selector string", {
  app_data <- make_app_data(
    mode = "categorial", n = 2,
    vars = list(`var name` = c("A", "B"))
  )
  app_data$enable_numeric <- TRUE
  result <- handcodeR:::.build_cat_keyboard_script(app_data)
  expect_true(grepl("var name", result, fixed = TRUE))
})

# ============================================================================ #
# .build_binary_keyboard_script                                                #
# ---------------------------------------------------------------------------- #
# Generates JS with three modes: quickcode, enable_numeric,                    #
# or Space/Enter only.                                                         #
# ============================================================================ #

test_that(".build_binary_keyboard_script returns a character string", {
  app_data <- make_app_data(mode = "binary", n = 2, vars = list(lr = c("L", "R")))
  result <- handcodeR:::.build_binary_keyboard_script(app_data)
  expect_true(is.character(result) && length(result) == 1L)
})

test_that(".build_binary_keyboard_script with quickcode=TRUE includes 700ms auto-advance", {
  app_data <- make_app_data(mode = "binary", n = 2, vars = list(lr = c("L", "R")))
  app_data$quickcode <- TRUE
  result <- handcodeR:::.build_binary_keyboard_script(app_data)
  expect_true(grepl("700", result, fixed = TRUE))
  expect_true(grepl("setTimeout", result, fixed = TRUE))
})

test_that(".build_binary_keyboard_script with quickcode=TRUE includes btn_1, btn_2, btn_missing", {
  app_data <- make_app_data(mode = "binary", n = 2, vars = list(lr = c("L", "R")))
  app_data$quickcode <- TRUE
  result <- handcodeR:::.build_binary_keyboard_script(app_data)
  expect_true(grepl("btn_lr_1", result, fixed = TRUE))
  expect_true(grepl("btn_lr_2", result, fixed = TRUE))
  expect_true(grepl("btn_lr_missing", result, fixed = TRUE))
})

test_that(".build_binary_keyboard_script enable_numeric=TRUE: keydown without timeout", {
  app_data <- make_app_data(mode = "binary", n = 2, vars = list(lr = c("L", "R")))
  app_data$enable_numeric <- TRUE
  result <- handcodeR:::.build_binary_keyboard_script(app_data)
  expect_true(grepl("keydown", result, fixed = TRUE))
  expect_false(grepl("setTimeout", result, fixed = TRUE))
})

test_that(".build_binary_keyboard_script neither flag: only Space/Enter handlers", {
  app_data <- make_app_data(mode = "binary", n = 2, vars = list(lr = c("L", "R")))
  result <- handcodeR:::.build_binary_keyboard_script(app_data)
  expect_true(grepl("prev", result, fixed = TRUE))
  expect_false(grepl("keydown", result, fixed = TRUE))
})

test_that(".build_binary_keyboard_script sanitizes special chars in button IDs", {
  app_data <- make_app_data(
    mode = "binary", n = 2,
    vars = list(`var name` = c("L", "R"))
  )
  app_data$quickcode <- TRUE
  result <- handcodeR:::.build_binary_keyboard_script(app_data)
  expect_true(grepl("btn_var_name_1", result, fixed = TRUE))
})

# ============================================================================ #
# .build_binary_comparison_ui                                                  #
# ---------------------------------------------------------------------------- #
# Smoke tests for binary-comparison annotation UI output.                      #
# ============================================================================ #

test_that(".build_binary_comparison_ui returns a shiny tag or tag list", {
  app_data <- make_app_data(mode = "binary_comparison", n = 2, vars = list(lr = c("L", "R")))
  result <- handcodeR:::.build_binary_comparison_ui(app_data)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that(".build_binary_comparison_ui HTML contains Binary (Comparison) title", {
  app_data <- make_app_data(mode = "binary_comparison", n = 2, vars = list(lr = c("L", "R")))
  html <- paste(as.character(handcodeR:::.build_binary_comparison_ui(app_data)), collapse = "")
  expect_true(grepl("Binary (Comparison)", html, fixed = TRUE))
})

test_that(".build_binary_comparison_ui HTML contains comparison-col CSS class", {
  app_data <- make_app_data(mode = "binary_comparison", n = 2, vars = list(lr = c("L", "R")))
  html <- paste(as.character(handcodeR:::.build_binary_comparison_ui(app_data)), collapse = "")
  expect_true(grepl("comparison-col", html, fixed = TRUE))
})

test_that(".run_binary_comparison_app calls shiny::runApp once", {
  skip_on_cran()
  app_data <- make_app_data(mode = "binary_comparison", n = 2, vars = list(lr = c("L", "R")))
  called <- 0L
  local_mocked_bindings(
    runApp = function(...) {
      called <<- called + 1L
      invisible(NULL)
    },
    .package = "shiny"
  )
  handcodeR:::.run_binary_comparison_app(app_data, autosave = FALSE)
  expect_equal(called, 1L)
})
