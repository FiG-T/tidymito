
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidymito <img src="man/figures/logo.png" align="right" height="138" /></a>

**Convenient Analysis Of Mitochondrial Measurements & Data**

<!-- badges: start -->

<!-- badges: end -->

## Overview

tidymito aims to streamline (mitochondrial) biology research by
automating common and repetitive data processing and analysis steps.
Initial developments will be focused on reading and formatting measures
of mitochondrial physiology from the **Oroboros O2K High Resolution
Respirometer**, as well as more general measures of individual organism
fitness (coming soon).

> Note: This package is early in development and is being maintained by
> academics, not software developers! Please bear with us as we figure
> things, and check that outputs are reasonable. Let us know if you
> would like to contribute, all are welcome!

## Installation

tidymito is not (yet?) on CRAN, but you can install the development
version of tidymito from [GitHub](https://github.com/) with:

``` r
# using pak: 
# if (!require(pak)) install.packages("pak") 

# install from GitHub repository:
 pak::pak("FiG-T/tidymito")
#> ℹ Loading metadata database✔ Loading metadata database ... done
#>  
#> → Will update 1 package.
#> → Will download 1 package with unknown size.
#> + tidymito 0.0.0.9000 → 0.0.0.9000 👷🏻‍♂️🔧 ⬇ (GitHub: 8c991b6)
#> ℹ Getting 1 pkg with unknown size
#> ✔ Got tidymito 0.0.0.9000 (source) (9.21 MB)
#> ℹ Packaging tidymito 0.0.0.9000
#> ✔ Packaged tidymito 0.0.0.9000 (951ms)
#> ℹ Building tidymito 0.0.0.9000
#> ✔ Built tidymito 0.0.0.9000 (1.2s)
#> ✔ Installed tidymito 0.0.0.9000 (github::FiG-T/tidymito@8c991b6) (65ms)
#> ✔ 1 pkg + 33 deps: kept 33, upd 1, dld 1 (NA B) [8.8s]

# OR 

# using devtools: 
# if (!require(devtools)) install.packages("devtools") 
# 
# devtools::install_github("FiG-T/tidymito")
```

## Examples

### Oroboros Data Import

Going from Oroboros measurements to neat data can bit a bit of a
challenge (and often requires a lot of manual selections and
copy-pasting). The `read_o2k_oxy_csv()` function allows a user to go
straight from the .csv file exported from the Oroboros Instrument to a
tibble with the readings per state.

This is a basic example which shows you how to solve a common problem:

``` r
library(tidymito)

testdata <- read_o2k_oxy_csv(
  file_id = "2025-03-03.*.csv", # a patten to match your exported file (or files)
  directory_path = "path/to/data/",
  exclude_events = c("11As"),  # as this is added at the same time as 11Tm, remove 11As
  treat_opening = "after",  # if a chamber is opened during a state, place the window after then chamber has re-oxygenated and closed.
  window_sizes = 15,  # if a measurement is taken every 2 seconds, the signal must be stable for 30 (2x15) seconds to pass the cutoff.
  change_thresholds = 1,
  format_output = TRUE, # select that you want the output tibble to be formatted
  sample_identifiers = c("NDi1-OE x gal4", "NDi1-OE ctrl"), # specify your unique sample identifiers
  wide_output = FALSE # specify that you want the data to be returned in a 'wide' format.
  )
#> Sample identifiers used. Ensure that all filenames have identifiers for both Chambers.
```

## Issues and Help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/fig-t/tidymito/issues).
