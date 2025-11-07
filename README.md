
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

If you fill out the questionaire, you can see the performance of the
strategy in six areas:

``` r
make_ridge_plots(data = opat_example)
#> Warning in make_ridge_plots(data = opat_example): Hi! The dataset you provided has more than one strategy.
#> The plots will reflect the first one listed: Baseline - fungicides
#> Joining with `by = join_by(rating_numeric, confidence)`
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

You can also compare the performances of the two strategies:

``` r
make_ridge_plots_pair(opat_example)
#> Joining with `by = join_by(rating_numeric, confidence)`
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
