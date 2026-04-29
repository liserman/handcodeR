# handcodeR 0.2.0

* **New features in `handcode()`:**
  - Added `comparison` input for comparing texts against a reference, with optional `comparison_pre` and `comparison_post` arguments to provide preceding and following context, similar to `pre` and `post` for `data`.
  - Added `add_notes` input to include a free text notes column.
    
* **Tweaks:**
  - Updated error messages to be more informative and user-friendly.
  - Modified behavior of `start = "first_empty"` and `"all_empty"`: now all rows with at least one uncoded observation in any classification variable are treated as empty. Previously, rows were considered empty only if all annotation variables were empty.

* **Documentation:**
  - Updated function documentation for greater clarity and consistency.



# handcodeR 0.1.2

* **Bug fixes:**
  - Fixed an error occurring while saving when `randomize = TRUE`. This error was introduced in version 0.1.1.

* **Tests:**
  - Added tests to cover the scenario in which the bug was occurring.


# handcodeR 0.1.1

* **Bug fixes:**
  - Fixed a bug occurring when `randomize = TRUE` and only one row of input data remained to be coded.

* **Improvements:**
  - The number of rows already coded is now visible.
  - Minor updates to some error messages for clarity.


# handcodeR 0.1.0

* **Initial release:**  
  - First CRAN submission.
