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
related to the package.1

## Release summary

This release (0.2.1) is a substantial rewrite of the package:

- Full Roxygen2 documentation for the public API (`handcode()`,
  `handcode_binary()`, package-level help).
- Cleaned NAMESPACE: internal helpers are no longer exported.
- New features: `quickcode` mode, comparison annotation workflow, optional
  quicksave snapshots written to a directory path supplied via the
  `quicksave` argument (CRAN-policy compliant).
- Extended test suite.
- Updated documentation (DESCRIPTION, README, URL/BugReports fields).

## CRAN policy compliance

- Writes to user filespace (quicksave snapshots) only occur when the user
  passes an explicit directory path as the `quicksave` argument.
- The default (`quicksave = FALSE`) performs no disk writes outside of
  `tempdir()`. Closing the app returns the annotated data as the function's
  return value (not written to disk or the global environment).
- No examples write to disk; interactive Shiny entry points are wrapped in
  `\dontrun{}`.
