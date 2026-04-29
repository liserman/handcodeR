# handcodeR <img src="man/figures/logo.png" align="right" height="139" />

[![codecov](https://codecov.io/gh/liserman/handcodeR/branch/master/graph/badge.svg?token=GVL875HZ14)](https://app.codecov.io/gh/liserman/handcodeR)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/handcodeR)](https://cran.r-project.org/package=handcodeR)
[![CRAN_latest_release_date](https://www.r-pkg.org/badges/last-release/handcodeR)](https://cran.r-project.org/package=handcodeR)
[![Downloads](https://cranlogs.r-pkg.org/badges/handcodeR)](https://cran.r-project.org/package=handcodeR)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/handcodeR?color=yellow)](https://cran.r-project.org/package=handcodeR)
[![DOI](https://zenodo.org/badge/608736610.svg)](https://zenodo.org/badge/latestdoi/608736610)

# handcodeR

R-Package to facilitate the annotation of text data by hand in R.

The goal of the handcodeR package is to provide an easy-to-use app to
annotate text data by hand. Often times when we work with text data, we
rely on hand-coded annotations of texts either as a unit of analysis in
itself, or as training and test samples for supervised machine learning
tools to classify text data. handcodeR offers Shiny-Apps that can be run
within R to annotate individual texts one by one. The package provides
two complementary entry points:

- `handcode()` opens a Shiny-App for **categorial annotation**, allowing
  you to code each text into pre-defined categories across an arbitrary
  number of classification variables. It also supports **side-by-side
  comparison** of two text vectors.
- `handcode_binary()` opens a Shiny-App for **two-choice (binary)
  annotation**, where each variable presents exactly two color-coded
  options (e.g. positive/negative). It supports keyboard shortcuts and
  multi-/single-factorial coding.

Both functions return a data frame with your coded annotations, support
resuming an existing coding session, autosave and quicksave recovery,
free-text notes, randomized order, and contextual display of preceding
and following texts.

I present a short step-by-step guide as well as the functions in more
detail below.

## How to cite this package

To cite the handcodeR package, you can use:

> Isermann, Lukas and Klingenspohr, Dennis. (2026). handcodeR: Text
> annotation app. R package version 0.2.1.
> <http://doi.org/10.5281/zenodo.8075100>.

You can also access the preferred citation as well as the bibtex entry
for the handcodeR Package via R:

```r
citation("handcodeR")
#> To cite handcodeR in publications, please use:
#>
#>   Isermann, Lukas and Klingenspohr, Dennis. 2026. handcodeR: Text
#>   annotation app. R package version 0.2.1.
#>   https://doi.org/10.5281/zenodo.8075100
#>
#> Ein BibTeX-Eintrag für LaTeX-Benutzer ist
#>
#>   @Misc{,
#>     title = {handcodeR: Text annotation app},
#>     author = {Lukas Isermann and Dennis Klingenspohr},
#>     year = {2026},
#>     note = {R package version 0.2.1},
#>     doi = {10.5281/zenodo.8075100},
#>     url = {https://github.com/liserman/handcodeR},
#>   }
```

## Installation

A stable version of `handcodeR` can be directly accessed on CRAN:

```r
install.packages("handcodeR", force = TRUE)
```

To install the latest development version of `handcodeR` directly from
[GitHub](https://github.com/liserman/handcodeR) use:

```r
library(devtools) # Tools to Make Developing R Packages Easier
devtools::install_github("liserman/handcodeR", force = TRUE)
```

## How to use this package

First, load the package

```r
library(handcodeR) # classify texts by hand in R
```

In the following, we are going to exemplify the workflow of the package
using a minimal working example.

The workflow of the package follows a simple rule:

1.  **Starting a new coding session:**
    - Initialize coding with `handcode()` (categorial) or
      `handcode_binary()` (two-choice) by providing a **character
      vector** of texts you wish to annotate as the `data` input.
    - Supply one or more **named character vectors** defining the
      categories you want to code.
    - Hand-code as much data as you like and save your progress by
      clicking the **“Save and exit”** button. This will return a data
      frame containing your annotations.
2.  **Resuming an existing coding session:**
    - Continue coding by providing the **data frame output** from your
      previous session as the `data` input.
    - This allows you to pick up where you left off without losing any
      previous annotations.
    - If autosave or quicksave files exist for the same object name, a
      recovery menu will appear at session start to let you choose the
      most complete state.

### handcode

The main function of the **handcodeR** package is `handcode()`.

`handcode()` can take either:

- A **character vector** of texts along with one or more **named
  character vectors** defining classification categories, or
- A **data frame** previously returned by `handcode()` to resume an
  existing coding session.

The function launches an interactive **Shiny app** that allows users to
annotate texts using the predefined categories. Once coding is complete,
`handcode()` returns a **data frame** containing the texts along with
their corresponding annotations.

To demonstrate the functionality of `handcode()`, we first use the R
package
[`archiveRetriever`](https://github.com/liserman/archiveRetriever) to
download a New York Times article covering the presidential debate
between Joe Biden and Donald Trump during the 2020 U.S. presidential
campaign. We then split the article into individual sentences, which can
be annotated using `handcode()`.

```r
# Install pacman if not already installed
if (!require(pacman)) install.packages("pacman")

# Use pacman to install and load archiveRetriever and stringr
pacman::p_load(archiveRetriever,
               stringr)

# Use the archiveRetriever to download article
nytimes_article <- scrape_urls(Urls = "http://web.archive.org/web/20201001004918/https://www.nytimes.com/2020/09/30/opinion/biden-trump-2020-debate.html",
                               Paths = c(title = "//h1[@itemprop='headline']",
                                         author = "//span[@itemprop='name']",
                                         date = "//time//text()",
                                         article = "//section[@itemprop='articleBody']//p"))

# Split up the article in different sentences
sentences <- unlist(str_split(nytimes_article$article, pattern = "(?<=(?<!Mr)[\\.!?])\\s"))

head(sentences)
#> [1] "I wasn’t in the crowd of people who believed Joe Biden shouldn’t deign to debate President Trump, but put me in the crowd that believes he shouldn’t debate him again."
#> [2] "Not after Tuesday night’s horror show: a disgrace to the format, an insult to the country, a nearly pointless 90 minutes."
#> [3] "And, I should add, a degradation of the presidency itself, which Trump had degraded so thoroughly already."
#> [4] "He put on a performance so contemptuous, so puerile, so dishonest and so across-the-board repellent that the moderator, Chris Wallace, morphed into some amalgam of elementary-school principal, child psychologist, traffic cop and roadkill."
#> [5] "No matter how Wallace pleaded with Trump or admonished him, he couldn’t make him behave."
#> [6] "But then why should Wallace have an experience any different from that of Trump’s chiefs of staff, of all the other former administration officials who have fled for the hills, of the Republican lawmakers who just threw up their hands and threw away any scruples they had?"
```

We can now use these sentences as input to `handcode()` to annotate the
individual sentences from the New York Times article.

In this example, we will annotate **two variables**:

1.  The **candidate** a sentence refers to.
2.  The **sentiment** of the statement.

```r
annotated <- handcode(data = sentences,
                      candidate = c("Joe Biden", "Donald Trump"),
                      sentiment = c("positive", "negative"))
```

<img src="man/figures/App1.png" width="100%" />

If we want to see not only the sentence currently being coded but also
the surrounding sentences, we can use the option `context = TRUE`. This
displays the current sentence along with its **previous** and
**following** sentences. To avoid confusion about which sentence is
being evaluated, the surrounding sentences are shown in **gray**.

You can also pass `context = "FLEX"` to render a runtime checkbox in the
app that lets you toggle the context display on and off while coding.

```r
annotated <- handcode(data = sentences,
                      candidate = c("Joe Biden", "Donald Trump"),
                      sentiment = c("positive", "negative"),
                      context = TRUE)
```

<img src="man/figures/App2.png" width="100%" />

If your text vector does not form a continuous sequence, but you still
want to provide previous and next sentences as context, you can specify
**custom vectors** for the surrounding sentences using the `pre` and
`post` arguments.

```r
# Vectors of all previous and all subsequent sentences
previous <- c("", sentences[2:length(sentences)])
subsequent <- c(sentences[2:length(sentences)-1])

annotated <- handcode(data = sentences,
                      candidate = c("Joe Biden", "Donald Trump"),
                      sentiment = c("positive", "negative"),
                      context = TRUE,
                      pre = previous,
                      post = subsequent)
```

You can stop the annotation process at any point by clicking the **“Save
and exit”** button. Once this button is clicked, the app shows a
confirmation dialog, closes, and `handcode()` returns a **data frame**
containing your texts along with the corresponding annotations.

```r
annotated
#> # A tibble: 60 × 3
#>    texts                                                     candidate sentiment
#>    <chr>                                                     <fct>     <fct>
#>  1 I wasn’t in the crowd of people who believed Joe Biden s… "Joe Bid… "negativ…
#>  2 Not after Tuesday night’s horror show: a disgrace to the… "_Not ap… "negativ…
#>  3 And, I should add, a degradation of the presidency itsel… ""        ""
#>  4 He put on a performance so contemptuous, so puerile, so … ""        ""
#>  5 No matter how Wallace pleaded with Trump or admonished h… ""        ""
#>  6 But then why should Wallace have an experience any diffe… ""        ""
#>  7 Trump runs roughshod over everyone and everything, and o… ""        ""
#>  8 Almost from the start, he talked over Biden, taunting hi… ""        ""
#>  9 He interrupted him and interrupted him and then interrup… ""        ""
#> 10 “Mr. President, I’m the moderator of this debate, and I … ""        ""
#> # ℹ 50 more rows
```

You can resume the annotation process at any time by using the **data
frame** returned from your previous `handcode()` session as the `data`
input in a new call to `handcode()`. By default, the function resumes
annotation at the **first text** that has not yet been annotated.

```r
annotated <- handcode(data = annotated,
                      context = TRUE)
```

<img src="man/figures/App3.png" width="100%" />

To facilitate the classification process, `handcode()` supports the
following keyboard shortcuts:

- **Space**: go to the **previous** text.
- **Enter**: go to the **next** text.

When navigating to previously coded lines, the app automatically
displays your **existing annotations**. For new lines, the default
values for all annotation variables are `""`. If you reach the **last
row** of your data, pressing **Enter** will automatically save your
annotations and exit the Shiny app.

### Comparing texts

You can also use **handcodeR** to compare texts against each other. To
do so, use the optional `comparison` argument to pass a second text
vector. Each element of `comparison` will be displayed alongside the
corresponding element of `data`, allowing you to annotate how the two
texts relate or differ.

To illustrate this, we again use the R package
[`archiveRetriever`](https://github.com/liserman/archiveRetriever) to
scrape the individual paragraphs of the Wikipedia article on **Wombats**
at two different points in time – **July 2022** and **March 2024**. We
then use `handcode()` to compare the paragraphs and identify changes
between the two versions.

```r
# Download paragraphs of Wikipedia article on Wombats for July 2022 and March 2024
wombat_1 <- scrape_urls(Urls = "https://web.archive.org/web/20220703070930/https://en.wikipedia.org/wiki/Wombat",
                        Paths = c(text = "//text()"),
                        collapse = "//div[@class='mw-body-content mw-content-ltr']//p")

wombat_2 <- scrape_urls(Urls = "https://web.archive.org/web/20240329193637/https://en.wikipedia.org/wiki/Wombat",
                        Paths = c(text = "//text()"),
                        collapse = "//div[@class='mw-content-ltr mw-parser-output']//p")

# Delete empty first row
wombat_1 <- wombat_1[-1,]
wombat_2 <- wombat_2[-1,]
```

After downloading the data, we can use `handcode()` to compare the
individual paragraphs.  
We supply the **original version** of the Wikipedia article as the
`data` input, and the **revised version** of the article as the
`comparison` input. This allows us to view both versions side by side in
the Shiny app and annotate how each paragraph has changed over time.

```r
comparison <- handcode(data = wombat_1$text,
                       comparison = wombat_2$text,
                       content_changed = c("Unchanged", "Minor changes", "Shortened", "Lengthened")
                       )
```

<img src="man/figures/App4.png" width="100%" />

As before, you can use the option `context = TRUE` to display the
preceding and following text for both the `data` and the `comparison`
input.

If you want to provide custom context instead of relying on
automatically adjacent text, you can use `comparison_pre` and
`comparison_post` to specify custom vectors to display before and after
each comparison text—analogous to the `pre` and `post` arguments for the
main `data` input.

### Adding notes

The handcodeR package also allows you to add an additional column for
free-text notes.  
To enable this feature, set `add_notes = TRUE`. This lets you enter
comments or observations alongside your coded categories.

```r
comparison <- handcode(data = wombat_1$text,
         comparison = wombat_2$text,
         content_changed = c("Unchanged", "Minor changes", "Shortened", "Lengthened"),
         add_notes = TRUE
)
```

<img src="man/figures/App5.png" width="100%" />

By default, this option only needs to be set once at the beginning of a
new annotation process.  
If you use `handcode()` to resume an existing annotation process, the
information is automatically taken from the data frame provided in
`data`, so you do not need to specify `add_notes` again. The same
applies to `handcode_binary()`.

### Other Tweaks

#### Start values

By default, `handcode()` starts at the **first line** in the input data
that has not yet been annotated (`start = "first_empty"`).

The `start` option allows you to specify which observation to begin
coding from:

- Use a **numeric value** to start at a specific row number.
- Use `start = "all_empty"` to annotate all lines that have not yet been
  coded, including any unannotated rows that lie between already coded
  lines, in the order they appear.

#### Randomizing the order

Sometimes, you may want to display texts in a **random order** to ensure
that the context of a text within the larger body does not influence the
annotations.

To randomize the order of display, set the option `randomize = TRUE`.

Note: This only affects the order of texts in the Shiny app. The
resulting output data frame will retain the original order of the texts.

#### Missing values

By default, `handcode()` includes a single missing category: **“Not
applicable”**. If you want a different missing category or multiple
missing categories, you can provide a **character vector** to the
`missing` argument. Missing categories are displayed in **gray** in the
Shiny app. In the returned data frame, these values are stored with a
**leading and trailing `_`**.

```r
annotated <- handcode(data = sentences,
                      candidate = c("Joe Biden", "Donald Trump"),
                      sentiment = c("positive", "negative"),
                      missing = c("Not applicable", "Undecided"))
```

<img src="man/figures/App6.png" width="100%" />

Note that `handcode_binary()` only allows a **single** missing category,
since the binary interface uses one shared “(missing)” button per
variable.

### Binary annotation with `handcode_binary()`

For workflows where each variable has exactly **two possible values**
(e.g. yes/no, positive/negative, supports/opposes), the package provides
`handcode_binary()`. The function shares the same data contract and
resume behavior as `handcode()`, but renders each variable as a pair of
large color-coded buttons, optimized for high-throughput coding.

Each classification variable must be a **named character vector of
length 2**. The first element is rendered on the **left**, the second on
the **right**.

```r
binary_annotated <- handcode_binary(data = sentences,
                                    biden_mention   = c("Yes", "No"),
                                    trump_mention   = c("Yes", "No"),
                                    sentiment       = c("Positive", "Negative"))
```

<img src="man/figures/App7.png" width="100%" />

`handcode_binary()` adds three options on top of the standard
`handcode()` arguments:

- **`colors`**: a list with `left` and `right` 6-digit hex strings to
  override the default green/red palette,
  e.g. `colors = list(left = "#2563eb", right = "#f97316")`.
- **`multifactorial`** (default `TRUE`): when set to `FALSE`, choosing
  the _left_ value for any variable automatically sets all other
  variables on that row to their _right_ value, except those already
  explicitly marked as missing. This is useful for “mutually exclusive”
  coding tasks where exactly one variable should receive the positive
  label.
- **`enable_numeric`** (default `FALSE`): activates numeric keyboard
  shortcuts. Pressing keys **1**–**9** clicks the _left_ button of the
  corresponding variable in display order. Limited to 9 variables.

The standard navigation shortcuts (Space = previous, Enter = next) and
all `handcode()` features — `context`, `pre`/`post`, `randomize`,
`start`, `add_notes`, autosave, quicksave, resume — apply to
`handcode_binary()` as well.

### Saving and resuming work

handcodeR provides three layers of save functionality so coding progress
is never lost.

#### Save and exit

Clicking **“Save and Exit”** in the app cleanly closes the Shiny session
and returns the annotated data frame to your R session. A confirmation
dialog appears so you know your data was saved before the tab closes.

#### Quicksave

Clicking the **“Quicksave”** button writes a timestamped `.RData`
snapshot of the current annotation state to your working directory,
**without** ending the session. Files are named
`<object_name>_quicksave_<timestamp>.RData`. Quicksaves accumulate, so
multiple checkpoints can coexist for the same object.

#### Autosave

If the app terminates **unexpectedly** (browser closed, R session
killed, network drop), the package writes an autosave file
`<object_name>_autosave.RData` to the working directory. The autosave is
overwritten on each unexpected close, so it always reflects the most
recent recoverable state. Autosave is disabled by setting
`autosave = FALSE` in the call to `handcode()` or `handcode_binary()`.

#### Resume menu

When you call `handcode()` (or `handcode_binary()`) with a data frame
whose object name matches existing autosave or quicksave files, the
function shows an interactive console menu that lists every recoverable
state along with the number of annotated rows in each. You can then
select the most complete version to continue from, or abort to keep the
data frame as-is.

```r
annotated <- handcode(data = annotated,
                      context = TRUE)

# Saved version(s) found. Which data do you want to use?
#
# 1: Passed data frame (12 of 60 rows annotated)
# 2: Autosave 'annotated_autosave.RData' (24 of 60 rows annotated)
# 3: Latest quicksave 'annotated_quicksave_1714387234.RData' (31 of 60 rows, saved 2026-04-29 14:20)
# 4: Abort
```

The resume menu is skipped when no recovery files exist or when `data`
is a raw character vector.
