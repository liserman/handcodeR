
# handcodeR <img src="man/figures/logo.png" align="right" height="139" />

[![codecov](https://codecov.io/gh/liserman/handcodeR/branch/master/graph/badge.svg?token=GVL875HZ14)](https://app.codecov.io/gh/liserman/handcodeR)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/handcodeR)](https://cran.r-project.org/package=handcodeR)
[![CRAN_latest_release_date](https://www.r-pkg.org/badges/last-release/handcodeR)](https://cran.r-project.org/package=handcodeR)
[![Downloads](https://cranlogs.r-pkg.org/badges/handcodeR)](https://cran.r-project.org/package=handcodeR)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/handcodeR?color=yellow)](https://cran.r-project.org/package=handcodeR)
[![DOI](https://zenodo.org/badge/608736610.svg)](https://zenodo.org/badge/latestdoi/608736610)

# handcodeR

R-Package to facilitate the annotation of text data by hand in R.

The goal of the handcodeR package is to provide an easy to use app to
annotate text data by hand. Often times when we work with text data, we
rely on hand coded annotations of texts either as unit of analysis in
itself, or as training and test samples for supervised machine learning
tools to classify text data. handcodeR offers a Shiny-App that can be
run within R to annotate individual texts one by one in up to six
different variables. To do so, the package uses the function
`handcode()`:

- `handcode()` opens a Shiny-App which allows for hand-coding strings of
  text into pre-defined categories. You can code between one and six
  variables at a time. It returns a data frame with your coded
  annotations.

I present a short step-by-step guide as well as the functions in more
detail below.

## How to cite this package

To cite the handcodeR package, you can use:

> Isermann, Lukas. (2023). handcodeR: Text annotation app. R package
> version 0.2.0. <http://doi.org/10.5281/zenodo.8075100>.

You can also access the preferred citation as well as the bibtex entry
for the handcodeR Package via R:

``` r
citation("handcodeR")
#> To cite handcodeR in publications, please use:
#> 
#>   Isermann, Lukas. 2023. handcodeR: Text annotation app. R package
#>   version 0.2.0. https://doi.org/10.5281/zenodo.8075100
#> 
#> Ein BibTeX-Eintrag für LaTeX-Benutzer ist
#> 
#>   @Misc{,
#>     title = {handcodeR: Text annotation app},
#>     author = {Lukas Isermann},
#>     year = {2023},
#>     note = {R package version 0.2.0},
#>     doi = {10.5281/zenodo.8075100},
#>     url = {https://github.com/liserman/handcodeR},
#>   }
```

## Installation

A stable version of `handcodeR` can be directly accessed on CRAN:

``` r
install.packages("handcodeR", force = TRUE)
```

To install the latest development version of `handcodeR` directly from
[GitHub](https://github.com/liserman/handcodeR) use:

``` r
library(devtools) # Tools to Make Developing R Packages Easier
devtools::install_github("liserman/handcodeR", force = TRUE)
```

## How to use this package

First, load the package

``` r
library(handcodeR) # classify texts by hand in R
```

In the following, we are going to exemplify the workflow of the package
using a minimal working example.

The workflow of the package follows a simple rule:

1.  **Starting a new coding session:**
    - Initialize coding with `handcode()` by providing a **character
      vector** of texts you wish to annotate as the `data` input.
    - Supply up to six **named character vectors** defining the
      categories you want to code.
    - Hand-code as much data as you like and save your progress by
      clicking the **“Save and exit”** button. This will return a data
      frame containing your annotations.
2.  **Resuming an existing coding session:**
    - Continue coding by providing the **data frame output** from your
      previous `handcode()` session as the `data` input.
    - This allows you to pick up where you left off without losing any
      previous annotations.

### handcode

The main function of the **handcodeR** package is `handcode()`.

`handcode()` can take either:  
- A **character vector** of texts along with up to six **named character
vectors** defining classification categories, or  
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

``` r
# Install box if not already installed
if (!"box" %in% installed.packages()) install.packages("box")

# Use box to install and load archiveRetriever::scrape_urls() and stringr::str_split()
box::use(archiveRetriever[scrape_urls],
         stringr[str_split])

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

``` r
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

``` r
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

``` r
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
and exit”** button. Once this button is clicked, the app closes and
`handcode()` returns a **data frame** containing your texts along with
the corresponding annotations.

``` r
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

``` r
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

``` r
# Download paragraphs of Wikipedia article on Wombats for July 2022 and March 2024
wombat_1 <- scrape_urls(Urls = "https://web.archive.org/web/20220703070930/https://en.wikipedia.org/wiki/Wombat",
                        Paths = c(text = "//text()"),
                        collapse = "//div[@class='mw-body-content mw-content-ltr']//p")

wombat_2 <- scrape_urls(Urls = "https://web.archive.org/web/20240329193637/https://en.wikipedia.org/wiki/Wombat",
                        Paths = c(text = "//text()"),
                        collapse = "//div[@class='mw-content-ltr mw-parser-output']//p")

head(wombat_1$text)
#> [1] ""                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> [2] "Wombats are short-legged, muscular quadrupedal  marsupials that are native to Australia . They are about 1 m (40 in) in length with small, stubby tails and weigh between 20 and 35 kg (44 and 77 lb). All three of the extant species are members of the family  Vombatidae . They are adaptable and habitat tolerant, and are found in forested, mountainous, and heathland areas of southern and eastern Australia, including Tasmania, as well as an isolated patch of about 300 ha (740 acres) in Epping Forest National Park [2] in central Queensland."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> [3] "The name \"wombat\" comes from the now-nearly extinct Darug language spoken by the aboriginal Darug people , who originally inhabited the Sydney area. [3] It was first recorded in January 1798, when John Price and James Wilson, a white man who had adopted aboriginal ways, visited the area of what is now Bargo, New South Wales . Price wrote: \"We saw several sorts of dung of different animals, one of which Wilson called a \"Whom-batt\", which is an animal about 20 inches high, with short legs and a thick body with a large head, round ears, and very small eyes; is very fat, and has much the appearance of a badger.\" [4] Wombats were often called badgers by early settlers because of their size and habits. Because of this, localities such as Badger Creek, Victoria , and Badger Corner, Tasmania, were named after the wombat. [5] The spelling went through many variants over the years, including \"wambat\", \"whombat\", \"womat\", \"wombach\", and \"womback\", possibly reflecting dialectal differences in the Darug language. [3]"
#> [4] "Though genetic studies of the Vombatidae have been undertaken, evolution of the family is not well understood. Wombats are estimated to have diverged from other Australian marsupials relatively early, as long as 40 million years ago, while some estimates place divergence at around 25 million years. [6] : 10–  While some theories place wombats as miniaturised relatives of diprotodonts, such as the rhinoceros-sized Diprotodon , more recent studies place the Vombatiformes as having a distinct parallel evolution, hence their current classification as a separate family. [7]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> [5] "Wombats dig extensive burrow systems with their rodent-like front teeth and powerful claws. One distinctive adaptation of wombats is their backward pouch. The advantage of a backward-facing pouch is that when digging, the wombat does not gather soil in its pouch over its young. Although mainly crepuscular and nocturnal , wombats may also venture out to feed on cool or overcast days. They are not commonly seen, but leave ample evidence of their passage, treating fences as minor inconveniences to be gone through or under."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> [6] "Wombats leave distinctive cubic  feces . [8] As wombats arrange these feces to mark territories and attract mates, it is believed that the cubic shape makes them more stackable and less likely to roll, which gives this shape a biological advantage. The method by which the wombat produces them is not well understood, but it is believed that the wombat intestine stretches preferentially at the walls, with two flexible and two stiff areas around its intestines. [9] The adult wombat produces between 80 and 100, 2 cm (0.8 in) pieces of feces in a single night, and four to eight pieces each bowel movement. [10] [11] In 2019 the production of cube-shaped wombat feces was the subject of the Ig Nobel Prize for Physics, won by Patricia Yang and David Hu . [12]"
head(wombat_2$text)
#> [1] ""                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> [2] "Wombats are short-legged, muscular quadrupedal  marsupials of the family Vombatidae that are native to Australia . Living species are about 1 m (40 in) in length with small, stubby tails and weigh between 20 and 35 kg (44 and 77 lb). They are adaptable and habitat tolerant, and are found in forested, mountainous, and heathland areas of southern and eastern Australia, including Tasmania, as well as an isolated patch of about 300 ha (740 acres) in Epping Forest National Park [2] in central Queensland."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> [3] "The name \"wombat\" comes from the now nearly extinct Dharug language spoken by the aboriginal Dharug people , who originally inhabited the Sydney area. [3] It was first recorded in January 1798, when John Price and James Wilson, a white man who had adopted aboriginal ways, visited the area of what is now Bargo, New South Wales . Price wrote: \"We saw several sorts of dung of different animals, one of which Wilson called a 'Whom-batt', which is an animal about 20 inches [51 cm] high, with short legs and a thick body with a large head, round ears, and very small eyes; is very fat, and has much the appearance of a badger.\" [4] Wombats were often called badgers by early settlers because of their size and habits. Because of this, localities such as Badger Creek, Victoria , and Badger Corner, Tasmania, were named after the wombat. [5] The spelling went through many variants over the years, including \"wambat\", \"whombat\", \"womat\", \"wombach\", and \"womback\", possibly reflecting dialectal differences in the Darug language. [3]"
#> [4] "Though genetic studies of the Vombatidae have been undertaken, evolution of the family is not well understood. Wombats are estimated to have diverged from other Australian marsupials relatively early, as long as 40 million years ago, while some estimates place divergence at around 25 million years. [6] : 10–  Some prehistoric wombat genera greatly exceeded modern wombats in size. The largest known wombat, Phascolonus , which went extinct approximately 40,000 years ago, [7] is estimated to have had a body mass of up to 360 kilograms (790 lb). [8]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> [5] "Wombats dig extensive burrow systems with their rodent-like front teeth and powerful claws. One distinctive adaptation of wombats is their backward pouch. The advantage of a backward-facing pouch is that when digging, the wombat does not gather soil in its pouch over its young. Although mainly crepuscular and nocturnal , wombats may also venture out to feed on cool or overcast days. They are not commonly seen, but leave ample evidence of their passage, treating fences as minor inconveniences to be gone through or under."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#> [6] "Wombats leave distinctive cubic  feces . [9] As wombats arrange these feces to mark territories and attract mates, it is believed that the cubic shape makes them more stackable and less likely to roll, which gives this shape a biological advantage. The method by which the wombat produces them is not well understood, but it is believed that the wombat intestine stretches preferentially at the walls, with two flexible and two stiff areas around its intestines. [10] The adult wombat produces between 80 and 100, 2 cm (0.8 in) pieces of feces in a single night, and four to eight pieces each bowel movement. [11] [12] In 2019 the production of cube-shaped wombat feces was the subject of the Ig Nobel Prize for Physics, won by Patricia Yang and David Hu . [13]"

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

``` r
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

``` r
handcode(data = wombat_1$text,
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
`data`, so you do not need to specify `add_notes` again.

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

``` r
annotated <- handcode(data = sentences, 
                      candidate = c("Joe Biden", "Donald Trump"),
                      sentiment = c("positive", "negative"),
                      missing = c("Not applicable", "Undecided"))
```

<img src="man/figures/App6.png" width="100%" />
