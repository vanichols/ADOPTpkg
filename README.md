
<!-- README.md is generated from README.Rmd. Please edit that file -->

# opat

<!-- badges: start -->

<!-- badges: end -->

The goal of opat is to provide access to data and functions for
assessing the performance and impacts of pest management strategies.
OPAT stands for ‘online performance assessment tool’ and was developed
as part of the European Horizon project ADOPT-IPM.

## Installation

You can install the development version of opat from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("vanichols/opat")
```

## Example

This is a basic example demonstrating a function to visualize pesticide
loads, as calculated using Vandevoorde et al. (in revision). Here we
look at a compound that has been banned in the European Union, diquat:

``` r
library(opat)
make_rose_plot(compound_name = "diquat",
              data = opat_hpli)
```

<img src="man/figures/README-example-1.png" width="100%" />

We can compare it to the Bordeaux mixture, which is used to control
fungal and bacterial diseases:

``` r
make_rose_plot_pair(
  compound_name1 = "diquat",
  compound_name2 = "Bordeaux mixture",
  data = opat_hpli)
#> Joining with `by = join_by(compound)`
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

We can also see what the current science says about glyphosate:

``` r
make_rose_plot(compound_name = "glyphosate",
              data = opat_hpli)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
