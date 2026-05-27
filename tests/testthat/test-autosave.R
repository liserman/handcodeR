# All autosave / recovery tests. Helper fixtures live in helper-mocks.R and helper-fixtures.R.

# ============================================================================ #
# Setup: .autosave_setup                                                       #
# ---------------------------------------------------------------------------- #
# Resolves the autosave argument: FALSE disables, a path enables + locates.    #
# ============================================================================ #

test_that(".autosave_setup returns NULL when autosave is FALSE", {
  expect_null(handcodeR:::.autosave_setup(FALSE, "mydata"))
})

test_that(".autosave_setup returns NULL when autosave is NULL", {
  expect_null(handcodeR:::.autosave_setup(NULL, "mydata"))
})

test_that(".autosave_setup errors when autosave is a bare TRUE", {
  expect_error(handcodeR:::.autosave_setup(TRUE, "mydata"), "FALSE or a path")
})

test_that(".autosave_setup errors when autosave is an empty/blank string", {
  expect_error(handcodeR:::.autosave_setup("   ", "mydata"), "FALSE or a path")
})

test_that(".autosave_setup errors when directory does not exist", {
  missing_dir <- file.path(tempfile(), "nope")
  expect_error(handcodeR:::.autosave_setup(missing_dir, "mydata"), "does not exist")
})

test_that(".autosave_setup returns dir and sanitized prefix for an existing directory", {
  tmp_dir <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  result  <- handcodeR:::.autosave_setup(tmp_dir, "my data$1")
  expect_equal(result$dir, normalizePath(tmp_dir))
  expect_equal(result$prefix, "my_data_1")
})

# ============================================================================ #
# Resume: .resume_menu                                                         #
# ---------------------------------------------------------------------------- #
# Recovery selection menu presented to the user at session start.              #
# ============================================================================ #

test_that(".resume_menu returns data unchanged when save_loc is NULL", {
  df <- make_ann_df()
  expect_identical(handcodeR:::.resume_menu(df, "df", NULL), df)
})

test_that(".resume_menu returns non-data-frame input unchanged", {
  v <- c("a", "b")
  expect_identical(handcodeR:::.resume_menu(v, "v", list(dir = tempdir(), prefix = "v")), v)
})

test_that(".resume_menu returns data unchanged when df has no texts column", {
  df <- data.frame(x = 1:3)
  expect_identical(handcodeR:::.resume_menu(df, "x", list(dir = tempdir(), prefix = "x")), df)
})

test_that(".resume_menu returns data unchanged when no recovery files exist", {
  df      <- make_ann_df()
  tmp_dir <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  expect_identical(
    handcodeR:::.resume_menu(df, "norecover", list(dir = tmp_dir, prefix = "norecover")),
    df
  )
})

test_that(".resume_menu returns data unchanged in a non-interactive session", {
  df      <- make_ann_df(3)
  tmp_dir <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  write_fake_autosave(tmp_dir, "mydata", n = 5)
  local_mocked_bindings(.interactive = function() FALSE, .package = "handcodeR")
  expect_identical(handcodeR:::.resume_menu(df, "mydata", list(dir = tmp_dir, prefix = "mydata")), df)
})

test_that(".resume_menu choice 1 returns original passed data", {
  df      <- make_ann_df(3)
  tmp_dir <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  write_fake_autosave(tmp_dir, "mydata", n = 5)
  local_mocked_bindings(.interactive = function() TRUE, .menu_wrapper = function(...) 1L, .package = "handcodeR")
  expect_identical(handcodeR:::.resume_menu(df, "mydata", list(dir = tmp_dir, prefix = "mydata")), df)
})

test_that(".resume_menu choice 2 returns autosave data", {
  df      <- make_ann_df(3)
  tmp_dir <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  write_fake_autosave(tmp_dir, "mydata", n = 5)
  local_mocked_bindings(.interactive = function() TRUE, .menu_wrapper = function(...) 2L, .package = "handcodeR")
  result <- handcodeR:::.resume_menu(df, "mydata", list(dir = tmp_dir, prefix = "mydata"))
  expect_equal(nrow(result), 5L)
})

test_that(".resume_menu quicksave can be selected", {
  df      <- make_ann_df(3)
  tmp_dir <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  write_fake_quicksave(tmp_dir, "mydata", n = 6)
  local_mocked_bindings(.interactive = function() TRUE, .menu_wrapper = function(...) 2L, .package = "handcodeR")
  result <- handcodeR:::.resume_menu(df, "mydata", list(dir = tmp_dir, prefix = "mydata"))
  expect_equal(nrow(result), 6L)
})

test_that(".resume_menu Abort (last choice) stops execution", {
  df      <- make_ann_df()
  tmp_dir <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  write_fake_autosave(tmp_dir, "mydata")
  local_mocked_bindings(.interactive = function() TRUE, .menu_wrapper = function(choices, ...) length(choices), .package = "handcodeR")
  expect_error(
    handcodeR:::.resume_menu(df, "mydata", list(dir = tmp_dir, prefix = "mydata")),
    "aborted"
  )
})

test_that(".resume_menu Escape (choice 0) stops execution", {
  df      <- make_ann_df()
  tmp_dir <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  write_fake_autosave(tmp_dir, "mydata")
  local_mocked_bindings(.interactive = function() TRUE, .menu_wrapper = function(...) 0L, .package = "handcodeR")
  expect_error(
    handcodeR:::.resume_menu(df, "mydata", list(dir = tmp_dir, prefix = "mydata")),
    "aborted"
  )
})

# ============================================================================ #
# Resume: .load_rdata                                                          #
# ---------------------------------------------------------------------------- #
# Fail-safe RData loader used by resume and autosave recovery.                 #
# ============================================================================ #

test_that(".load_rdata round-trips an RData file correctly", {
  tmp <- tempfile(fileext = ".RData")
  on.exit(unlink(tmp))
  test_df_load <- data.frame(x = 1:3)
  save(test_df_load, file = tmp)
  result <- handcodeR:::.load_rdata(tmp, "test_df_load")
  expect_equal(result, test_df_load)
})

test_that(".load_rdata returns NULL for missing variable name in file", {
  tmp <- tempfile(fileext = ".RData")
  on.exit(unlink(tmp))
  save_var <- data.frame(x = 1)
  save(save_var, file = tmp)
  result <- handcodeR:::.load_rdata(tmp, "nonexistent_var")
  expect_null(result)
})

test_that(".load_rdata returns NULL for a nonexistent file path", {
  result <- handcodeR:::.load_rdata("/no/such/file.RData", "anything")
  expect_null(result)
})

# ============================================================================ #
# Integration: Autosave Default and Cancel / Abort Flows                       #
# ---------------------------------------------------------------------------- #
# End-to-end cancel and abort handling across both entry points.               #
# ============================================================================ #

test_that("handcode autosave default is FALSE", {
  expect_identical(formals(handcodeR:::handcode)[["autosave"]], FALSE)
})

test_that("handcode_binary autosave default is FALSE", {
  expect_identical(formals(handcodeR:::handcode_binary)[["autosave"]], FALSE)
})

test_that("handcode returns NULL when autosave = TRUE carries no path", {
  expect_autosave_true_returns_null(
    handcodeR:::handcode,
    c("text1", "text2"), cat = c("A", "B")
  )
})

test_that("handcode returns NULL when resume menu is aborted", {
  expect_resume_abort_returns_null(
    handcodeR:::handcode,
    c("text1", "text2"), cat = c("A", "B")
  )
})

test_that("handcode_binary returns NULL when autosave = TRUE carries no path", {
  expect_autosave_true_returns_null(
    handcodeR:::handcode_binary,
    c("t1", "t2"), lr = c("L", "R")
  )
})

test_that("handcode_binary returns NULL when resume menu is aborted", {
  expect_resume_abort_returns_null(
    handcodeR:::handcode_binary,
    c("t1", "t2"), lr = c("L", "R")
  )
})
