## Test environments

- Local: Windows 10 x64, R 4.5.2
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results

```
0 errors | 0 warnings | 1 note
```

The single NOTE is environmental ("unable to verify current time") and not
related to the package.

## Release summary

This release (0.2.1) is a substantial rewrite of the package:

- Full Roxygen2 documentation for the public API (`handcode()`,
  `handcode_binary()`, package-level help).
- Cleaned NAMESPACE: internal helpers are no longer exported.
- New features: `quickcode` mode, comparison annotation workflow, autosave
  with explicit user-confirmed save location (CRAN-policy compliant).
- Extended test suite.
- Updated documentation (DESCRIPTION, README, URL/BugReports fields).

## CRAN policy compliance

- Writes to user filespace (autosave/quicksave) only occur after explicit
  per-session user confirmation via an interactive menu.
- Configuration (last-used save directory) is stored in
  `tools::R_user_dir("handcodeR", "config")`, as permitted by CRAN policy.
- No examples write to disk; interactive Shiny entry points are wrapped in
  `\dontrun{}`.
