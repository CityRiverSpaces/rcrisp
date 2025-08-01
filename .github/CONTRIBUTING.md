# Contributing to rcrisp

This outlines how to propose a change to rcrisp.
For a detailed discussion on contributing to packages, please see the tidyverse [development contributing guide](https://rstd.io/tidy-contrib) and [code review principles](https://code-review.tidyverse.org/).

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 
If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).
See our guide on [how to create a great issue](https://code-review.tidyverse.org/issues/) for more advice.

### Pull request process

*  Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("CityRiverSpaces/rcrisp", fork = TRUE)`.

*  Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
   If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing.
   
*  Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*  Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
   The title of your PR should briefly describe the change.
   The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.

### Code style

*  New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
   You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles, but please don't restyle code that has nothing to do with your PR.  

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

## Roadmap

We strive to develop `rcrisp` into a robust tool for delineation. Although the
package already produces reliable delineations, we do see room for improvement.

We plan to:

*  Expand the use of input data from OpenStreetMap to Overture Maps and from
   Copernicus GLO-30 Digital Elevation Models to Global Digital Terrain Models
   from OpenTopography.
*  Improve segmentation (1) by merging neighbouring corridor segments using
   morphological similarity learned from segment-level morphometrics through
   clustering and (2) by splitting large segments based on potential river
   crossings identified in the street network.
*  Improve the user interface of `rcrisp` and validate its output while building
   an inventory of use cases to meet the needs of a broad research community.
   
We will not:

*  Develop `rcrisp` into a package for urban river space analysis, but maintain
   its focus on providing morphologically grounded boundaries to be adopted in
   subsequent analyses using other domain-specific tools.

## Code of Conduct

Please note that the rcrisp project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
