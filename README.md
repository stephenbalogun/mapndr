
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mapndr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/stephenbalogun/mapndr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stephenbalogun/mapndr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of mapndr is to simplify geospatial analysis and plotting of
population pyramids of summary data from the Nigeria National Data
Repository (NDR). It aligns with the naming convention for `state`,
`LGA` and `sex` variables as obtained from most of the line-lists
obtained from the NDR.

## Installation

You can install the development version of mapndr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stephenbalogun/mapndr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mapndr)
#> 
#> Attaching package: 'mapndr'
#> The following object is masked from 'package:stats':
#> 
#>     spectrum
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

# `{r cars} # summary(cars) #`

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

# `{r pressure, echo = FALSE} # plot(pressure) #`

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

## Code of Conduct

Please note that the checkndr project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
