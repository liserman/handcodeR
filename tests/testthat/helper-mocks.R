# ============================================================================ #
# I/O Mock Factories                                                           #
# ---------------------------------------------------------------------------- #
# Factories for .interactive, .menu_wrapper, and .readline_wrapper mocks.      #
# Loaded automatically by testthat before any test file.                       #
# ============================================================================ #

mock_interactive <- function(value = TRUE) function() value

mock_menu <- function(choice) {
  if (is.function(choice)) return(choice)
  function(...) choice
}

mock_readline <- function(answers) {
  idx <- 0L
  function(...) {
    idx <<- idx + 1L
    if (idx <= length(answers)) answers[[idx]] else ""
  }
}

with_mocked_io <- function(interactive_val = TRUE, menu_choice = 1L, readline_answers = "", code) {
  local_mocked_bindings(
    .interactive      = mock_interactive(interactive_val),
    .menu_wrapper     = mock_menu(menu_choice),
    .readline_wrapper = mock_readline(as.list(readline_answers)),
    .package          = "handcodeR"
  )
  force(code)
}

# ============================================================================ #
# Autosave Fixture Writers                                                     #
# ---------------------------------------------------------------------------- #
# Helpers that write fake autosave and quicksave RData files to tempdir.       #
# ============================================================================ #

write_fake_autosave <- function(dir, prefix, n = 4) {
  var_name <- paste0(prefix, "_autosave")
  df       <- make_ann_df(n)
  assign(var_name, df)
  save(list = var_name, file = file.path(dir, paste0(prefix, "_autosave.RData")))
  invisible(df)
}

write_fake_quicksave <- function(dir, prefix, n = 4) {
  var_name <- prefix
  df       <- make_ann_df(n)
  assign(var_name, df)
  ts <- as.integer(Sys.time())
  save(list = var_name, file = file.path(dir, paste0(prefix, "_quicksave_", ts, ".RData")))
  invisible(df)
}

# ============================================================================ #
# Shared Entry-Point Assertions                                                #
# ---------------------------------------------------------------------------- #
# Reusable expect_* helpers for autosave cancel and resume abort flows.        #
# ============================================================================ #

# Returns NULL when .autosave_menu raises a cancellation error.
expect_autosave_cancel_returns_null <- function(entry_fn, data, ...) {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  local_mocked_bindings(
    .autosave_menu = function(...) stop("autosave setup cancelled."),
    .package       = "handcodeR"
  )
  expect_null(entry_fn(data, ..., autosave = TRUE))
}

# Returns NULL when .resume_menu raises an abort error.
expect_resume_abort_returns_null <- function(entry_fn, data, ...) {
  local_mocked_bindings(.interactive = function() TRUE, .package = "handcodeR")
  local_mocked_bindings(
    .autosave_menu = function(...) list(dir = tempdir(), prefix = "x"),
    .resume_menu   = function(...) stop("handcodeR: session aborted by user."),
    .package       = "handcodeR"
  )
  expect_null(entry_fn(data, ..., autosave = TRUE))
}
