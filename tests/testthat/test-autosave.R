# All autosave / recovery tests. Helper fixtures live in helper-mocks.R and helper-fixtures.R.

# ---- config: .autosave_config_path, .read_last_save_dir, .write_last_save_dir ----

test_that(".autosave_config_path returns path ending with last_save_dir.txt", {
  expect_true(endsWith(handcodeR:::.autosave_config_path(), "last_save_dir.txt"))
})

test_that(".read_last_save_dir returns NULL when config file absent", {
  tmp <- tempfile()
  local_mocked_bindings(.autosave_config_path = function() tmp, .package = "handcodeR")
  expect_null(handcodeR:::.read_last_save_dir())
})

test_that(".read_last_save_dir returns NULL when stored path does not exist on disk", {
  tmp <- tempfile()
  writeLines("/nonexistent/path/xyz_abc_123", tmp)
  local_mocked_bindings(.autosave_config_path = function() tmp, .package = "handcodeR")
  expect_null(handcodeR:::.read_last_save_dir())
})

test_that(".read_last_save_dir returns NULL for empty config file", {
  tmp <- tempfile()
  writeLines("", tmp)
  local_mocked_bindings(.autosave_config_path = function() tmp, .package = "handcodeR")
  expect_null(handcodeR:::.read_last_save_dir())
})

test_that(".write_last_save_dir and .read_last_save_dir round-trip correctly", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  local_mocked_bindings(.autosave_config_path = function() tmp, .package = "handcodeR")
  handcodeR:::.write_last_save_dir(getwd())
  expect_equal(handcodeR:::.read_last_save_dir(), getwd())
})

# ---- resume: .resume_menu ----

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
  expect_identical(
    handcodeR:::.resume_menu(df, "norecover", list(dir = tmp_dir, prefix = "norecover")),
    df
  )
})

test_that(".resume_menu choice 1 returns original passed data", {
  df      <- make_ann_df(3)
  tmp_dir <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  write_fake_autosave(tmp_dir, "mydata", n = 5)
  local_mocked_bindings(.menu_wrapper = function(...) 1L, .package = "handcodeR")
  expect_identical(handcodeR:::.resume_menu(df, "mydata", list(dir = tmp_dir, prefix = "mydata")), df)
})

test_that(".resume_menu choice 2 returns autosave data", {
  df      <- make_ann_df(3)
  tmp_dir <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  write_fake_autosave(tmp_dir, "mydata", n = 5)
  local_mocked_bindings(.menu_wrapper = function(...) 2L, .package = "handcodeR")
  result <- handcodeR:::.resume_menu(df, "mydata", list(dir = tmp_dir, prefix = "mydata"))
  expect_equal(nrow(result), 5L)
})

test_that(".resume_menu quicksave can be selected", {
  df      <- make_ann_df(3)
  tmp_dir <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  write_fake_quicksave(tmp_dir, "mydata", n = 6)
  local_mocked_bindings(.menu_wrapper = function(...) 2L, .package = "handcodeR")
  result <- handcodeR:::.resume_menu(df, "mydata", list(dir = tmp_dir, prefix = "mydata"))
  expect_equal(nrow(result), 6L)
})

test_that(".resume_menu Abort (last choice) stops execution", {
  df      <- make_ann_df()
  tmp_dir <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  write_fake_autosave(tmp_dir, "mydata")
  local_mocked_bindings(.menu_wrapper = function(choices, ...) length(choices), .package = "handcodeR")
  expect_error(
    handcodeR:::.resume_menu(df, "mydata", list(dir = tmp_dir, prefix = "mydata")),
    "aborted"
  )
})

test_that(".resume_menu Escape (choice 0) stops execution", {
  df      <- make_ann_df()
  tmp_dir <- tempfile(); dir.create(tmp_dir); on.exit(unlink(tmp_dir, recursive = TRUE))
  write_fake_autosave(tmp_dir, "mydata")
  local_mocked_bindings(.menu_wrapper = function(...) 0L, .package = "handcodeR")
  expect_error(
    handcodeR:::.resume_menu(df, "mydata", list(dir = tmp_dir, prefix = "mydata")),
    "aborted"
  )
})

# ---- resume: .load_rdata ----

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

# ---- menu: .autosave_menu ----

test_that(".autosave_menu Cancel (last choice) stops execution", {
  tmp <- tempfile(); on.exit(unlink(tmp))
  local_mocked_bindings(.autosave_config_path = function() tmp, .package = "handcodeR")
  local_mocked_bindings(
    .menu_wrapper     = function(choices, ...) length(choices),
    .readline_wrapper = function(...) "",
    .package          = "handcodeR"
  )
  expect_error(handcodeR:::.autosave_menu("mydata"), "cancelled")
})

test_that(".autosave_menu Escape (choice 0) stops execution", {
  tmp <- tempfile(); on.exit(unlink(tmp))
  local_mocked_bindings(.autosave_config_path = function() tmp, .package = "handcodeR")
  local_mocked_bindings(
    .menu_wrapper     = function(...) 0L,
    .readline_wrapper = function(...) "",
    .package          = "handcodeR"
  )
  expect_error(handcodeR:::.autosave_menu("mydata"), "cancelled")
})

test_that(".autosave_menu cwd choice returns correct dir and default prefix", {
  tmp <- tempfile(); on.exit(unlink(tmp))
  local_mocked_bindings(.autosave_config_path = function() tmp, .package = "handcodeR")
  local_mocked_bindings(
    .menu_wrapper     = function(choices, ...) 1L,
    .readline_wrapper = function(...) "",
    .package          = "handcodeR"
  )
  result <- handcodeR:::.autosave_menu("mydata")
  expect_equal(result$dir, getwd())
  expect_equal(result$prefix, "mydata")
})

test_that(".autosave_menu custom prefix overrides default", {
  tmp <- tempfile(); on.exit(unlink(tmp))
  local_mocked_bindings(.autosave_config_path = function() tmp, .package = "handcodeR")
  local_mocked_bindings(
    .menu_wrapper     = function(choices, ...) 1L,
    .readline_wrapper = function(...) "custom_prefix",
    .package          = "handcodeR"
  )
  expect_equal(handcodeR:::.autosave_menu("mydata")$prefix, "custom_prefix")
})

test_that(".autosave_menu writes chosen dir to config", {
  tmp <- tempfile(); on.exit(unlink(tmp))
  local_mocked_bindings(.autosave_config_path = function() tmp, .package = "handcodeR")
  local_mocked_bindings(
    .menu_wrapper     = function(choices, ...) 1L,
    .readline_wrapper = function(...) "",
    .package          = "handcodeR"
  )
  handcodeR:::.autosave_menu("mydata")
  expect_equal(trimws(readLines(tmp, n = 1, warn = FALSE)), getwd())
})

test_that(".autosave_menu subdir choice creates directory and returns its path", {
  tmp_cfg    <- tempfile()
  tmp_parent <- tempfile(); dir.create(tmp_parent)
  on.exit({ unlink(tmp_cfg); unlink(tmp_parent, recursive = TRUE) })
  old_wd <- setwd(tmp_parent); on.exit(setwd(old_wd), add = TRUE)
  local_mocked_bindings(.autosave_config_path = function() tmp_cfg, .package = "handcodeR")
  call_n <- 0L
  local_mocked_bindings(
    .menu_wrapper     = function(choices, ...) 2L,
    .readline_wrapper = function(prompt = "") { call_n <<- call_n + 1L; if (call_n == 1L) "saves" else "" },
    .package          = "handcodeR"
  )
  result <- handcodeR:::.autosave_menu("mydata")
  expect_true(dir.exists(result$dir))
  expect_equal(basename(result$dir), "saves")
})

test_that(".autosave_menu empty subdir name stops execution", {
  tmp_cfg    <- tempfile()
  tmp_parent <- tempfile(); dir.create(tmp_parent)
  on.exit({ unlink(tmp_cfg); unlink(tmp_parent, recursive = TRUE) })
  old_wd <- setwd(tmp_parent); on.exit(setwd(old_wd), add = TRUE)
  local_mocked_bindings(.autosave_config_path = function() tmp_cfg, .package = "handcodeR")
  local_mocked_bindings(
    .menu_wrapper     = function(choices, ...) 2L,
    .readline_wrapper = function(...) "",
    .package          = "handcodeR"
  )
  expect_error(handcodeR:::.autosave_menu("mydata"), "cancelled")
})

test_that(".autosave_menu shows last location when saved dir exists and differs from cwd", {
  tmp_cfg <- tempfile()
  tmp_dir <- tempfile(); dir.create(tmp_dir)
  on.exit({ unlink(tmp_cfg); unlink(tmp_dir, recursive = TRUE) })
  writeLines(tmp_dir, tmp_cfg)
  local_mocked_bindings(.autosave_config_path = function() tmp_cfg, .package = "handcodeR")
  captured <- NULL
  local_mocked_bindings(
    .menu_wrapper     = function(choices, ...) { captured <<- choices; length(choices) },
    .readline_wrapper = function(...) "",
    .package          = "handcodeR"
  )
  expect_error(handcodeR:::.autosave_menu("mydata"), "cancelled")
  expect_true(any(grepl("Use last location", captured)))
})

test_that(".autosave_menu prefix prompt shows 'enter to resume' when recovery file exists", {
  tmp_cfg <- tempfile()
  tmp_dir <- tempfile(); dir.create(tmp_dir)
  on.exit({ unlink(tmp_cfg); unlink(tmp_dir, recursive = TRUE) })
  write_fake_autosave(tmp_dir, "mydata")
  local_mocked_bindings(.autosave_config_path = function() tmp_cfg, .package = "handcodeR")
  call_n <- 0L; captured_prompt <- NULL
  local_mocked_bindings(
    .menu_wrapper     = function(choices, ...) 3L,
    # call 1 = "Path: " prompt → return tmp_dir; call 2 = prefix prompt → capture it
    .readline_wrapper = function(prompt = "") {
      call_n <<- call_n + 1L
      if (call_n == 1L) { tmp_dir }
      else { captured_prompt <<- prompt; "" }
    },
    .package = "handcodeR"
  )
  handcodeR:::.autosave_menu("mydata")
  expect_true(grepl("enter to resume", captured_prompt))
})

test_that(".autosave_menu prefix prompt shows 'default' when no recovery file exists", {
  tmp_cfg <- tempfile()
  tmp_dir <- tempfile(); dir.create(tmp_dir)
  on.exit({ unlink(tmp_cfg); unlink(tmp_dir, recursive = TRUE) })
  local_mocked_bindings(.autosave_config_path = function() tmp_cfg, .package = "handcodeR")
  call_n <- 0L; captured_prompt <- NULL
  local_mocked_bindings(
    .menu_wrapper     = function(choices, ...) 3L,
    # call 1 = "Path: " prompt → return tmp_dir; call 2 = prefix prompt → capture it
    .readline_wrapper = function(prompt = "") {
      call_n <<- call_n + 1L
      if (call_n == 1L) { tmp_dir }
      else { captured_prompt <<- prompt; "" }
    },
    .package = "handcodeR"
  )
  handcodeR:::.autosave_menu("mydata")
  expect_true(grepl("default", captured_prompt))
})

# ---- integration: autosave default + cancel/abort flows across all entry points ----

test_that("handcode autosave default is FALSE", {
  expect_identical(formals(handcodeR:::handcode)[["autosave"]], FALSE)
})

test_that("handcode_binary autosave default is FALSE", {
  expect_identical(formals(handcodeR:::handcode_binary)[["autosave"]], FALSE)
})

test_that("handcode returns NULL when autosave menu is cancelled", {
  expect_autosave_cancel_returns_null(
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

test_that("handcode_binary returns NULL when autosave menu is cancelled", {
  expect_autosave_cancel_returns_null(
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
