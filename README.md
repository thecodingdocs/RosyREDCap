
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RosyREDCap <img src="man/figures/logo.png" align="right" width="120"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/thecodingdocs/RosyREDCap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thecodingdocs/RosyREDCap/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/thecodingdocs/RosyREDCap/graph/badge.svg)](https://app.codecov.io/gh/thecodingdocs/RosyREDCap)
<!-- badges: end -->

At this time this package is **not suited for massive REDCap projects**.
More to come in future versions! The core functions are stable but
future development and changes are possible.

## Minimum Requirements

- R (and ideally RStudio) installed on your computer.
- Access to at least one REDCap project (real or test) with API Token
  Privileges according to User Rights.
- Permission to export and analyze date for this project.

![](man/figures/cover.jpg)

# What is `{RosyREDCap}`?

R and REDCap are both widely utilized in medicine, including basic
science, clinical research, and clinal trials. Both have independent
strengths, but together they can create powerful data pipelines. While
several R packages exist for extracting data using the REDCap API,
`{RosyREDCap}` stands out by offering comprehensive extraction of all
metadata and data from any REDCap project into a standardized R list
object, facilitating effortless downstream use in exports, imports,
transformation, and applications. This is showcased in the exploratory
data analysis shiny app included in the package. The three core aims of
`{RosyREDCap}` are to…

1.  Maintain a local version of the database (DB) object by only calling
    recently updated records using the REDCap log.
2.  Allow imports of non-coded versions of the dataset using R or
    Excel/CSV.
3.  Launch a shiny app that allows you to explore all of your REDCap
    projects.

By leveraging the combined strengths of R and REDCap, users can maintain
strong clinical data pipelines, collected and processed appropriately to
improve research and patient care. RosyREDCap can be used as a base data
object and data quality tool for most REDCap projects to aid in
collection, monitoring, transformation, and analysis.

## Installing RosyREDCap

The stable release can be found on CRAN and installed with:
**PLACEHOLDER NOT SUBMITTED TO CRAN YET**

``` r
#install.packages("RosyREDCap") #PLACEHOLDER NOT SUBMITTED TO CRAN YET
```

You can install the development version of RosyREDCap from GitHub by
using the `{remotes}` package. Be sure to install `{remotes}` if you
don’t have it already.

``` r
#install.packages("remotes)
remotes::install_github("thecodingdocs/RosyREDCap")
```

Note that the version used at the time of writing this book is
0.0.0.9003. You can check what version you have installed with the
following.

``` r
packageVersion("RosyREDCap")
#> [1] '0.0.0.9003'
```

If you have any issues, try downloading the most recent version of R at
RStudtio and update all packages in RStudio. See
[thecodingdocs.com/r/getting-started](https://www.thecodingdocs.com/r/getting-started "R Getting Started").

## Getting Started

``` r
library("RosyREDCap")
#run shiny app!
run_RosyREDCap() # will work with multiple REDCap projects!
```

## About the Name

Rosy with a capital ‘R’ evokes the R statistical programming language,
the primary developer’s last name, and the idea of making something more
beautiful and user-friendly. This prefix combined with REDCap
demonstrates the package’s goal of enhancing the REDCap experience by
creating data tools that are powerful and pleasant to use, combining the
best of R with best REDCap.

## Links

- The RosyREDCap package is at
  [github.com/thecodingdocs/RosyREDCap](https://github.com/thecodingdocs/RosyREDCap "RosyREDCap R package").
  See instructions above.
- Donate if I helped you out and want more development (anything helps)!
  [account.venmo.com/u/brandonerose](https://account.venmo.com/u/brandonerose "Venmo Donation")
- For more R coding visit
  [TheCodingDocs.com](https://www.thecodingdocs.com/ "TheCodingDocs.com")
- For correspondence/feedback/issues, please email
  <TheCodingDocs@gmail.com>!
- Follow us on Twitter
  [twitter.com/TheCodingDocs](https://twitter.com/TheCodingDocs "TheCodingDocs Twitter")
- Follow me on Twitter
  [twitter.com/BRoseMDMPH](https://twitter.com/BRoseMDMPH "BRoseMDMPH Twitter")

[![TheCodingDocs.com](man/figures/TCD.png)](http://www.thecodingdocs.com)
