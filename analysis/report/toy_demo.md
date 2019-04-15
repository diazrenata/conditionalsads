Demo with toy data
================
Renata Diaz
4/14/2019

``` r
library(conditionalsads)
```

This analysis will also source functions from `meteR` and `feasiblesads`.

1. Generate toy data
--------------------

Generate toy dataset that captures 4 scenarios:

1.  S and N are the same for control and manipulated communities, and both are drawn from the statistical constraint.

2.  S and N change with manipulation, but both are drawn from their respective constraints.

3.  S and N remain the same, but the manipulated community is drawn from a near-uniform distribution rather than the statistical constraint.

4.  S and N change, and the manipulated community is drawn from a near-uniform distribution.

Within each scenario there are `nsamples` communities in each treatment group. The control communities always have `ctrl_s` species and `ctrl_n` individuals. For scenarios 2 and 4, the treatment communities have approximately 80% of the species and individuals as the control communities.

``` r
toy_fs = make_toy_data(constraint = "FS", nsamples = 1, ctrl_s = 5, ctrl_n = 100)
str(toy_fs)
```

    ## 'data.frame':    8 obs. of  7 variables:
    ##  $ h     : Factor w/ 4 levels "h1","h2","h3",..: 1 1 2 2 3 3 4 4
    ##  $ trtmnt: Factor w/ 2 levels "control","treatment": 1 2 1 2 1 2 1 2
    ##  $ 1     : num  3 7 1 5 2 16 4 18
    ##  $ 2     : num  15 14 5 8 11 16 12 18
    ##  $ 3     : num  17 14 25 29 13 19 15 20
    ##  $ 4     : num  18 23 28 38 15 21 15 24
    ##  $ 5     : num  47 42 41 NA 59 28 54 NA

``` r
head(toy_fs)
```

    ##    h    trtmnt  1  2  3  4  5
    ## 1 h1   control  3 15 17 18 47
    ## 2 h1 treatment  7 14 14 23 42
    ## 3 h2   control  1  5 25 28 41
    ## 4 h2 treatment  5  8 29 38 NA
    ## 5 h3   control  2 11 13 15 59
    ## 6 h3 treatment 16 16 19 21 28

2. Generate samples from constraints
------------------------------------

For each sample,

3. Calculate test statistics
----------------------------

4. Get quantiles of empirical test statistics vs. constraints
-------------------------------------------------------------

5. Compare quantiles across experimental treatments
---------------------------------------------------
