# Accessor and replacement functions for `fdrtprcurve` slot

Accessor and replacement functions for the `fdrtprcurve` slot in a
`COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
fdrtprcurve(x, ...)

fdrtprcurve(x, ...) <- value

# S4 method for class 'COBRAPerformance'
fdrtprcurve(x)

# S4 method for class 'COBRAPerformance,data.frame'
fdrtprcurve(x) <- value

# S4 method for class 'COBRAPlot,data.frame'
fdrtprcurve(x) <- value
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A data frame giving information necessary to generate curves of
  observed FDR vs TPR for each method and each stratification level.

## Value

The accessor function returns a data frame giving information necessary
to generate curves of observed FDR vs TPR for each method and each
stratification level.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example,
                                   binary_truth = "status",
                                   aspects = "fdrtprcurve")
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
head(fdrtprcurve(cobraperf))
#>         FDR       TPR NBR       CUTOFF  TP FP   TN  FN TOT_CALLED DIFF NONDIFF
#> 1       NaN 0.0000000   0         -Inf   0  0 1580 819       2399  831    3027
#> 2 0.1606648 0.3646209 361 0.000000e+00 303 58 1522 516       2399  831    3027
#> 3 0.1589041 0.3694344 365 1.110223e-16 307 58 1522 512       2399  831    3027
#> 4 0.1584699 0.3706378 366 2.220446e-16 308 58 1522 511       2399  831    3027
#> 5 0.1580381 0.3718412 367 3.330669e-16 309 58 1522 510       2399  831    3027
#> 6 0.1576087 0.3730445 368 5.551115e-16 310 58 1522 509       2399  831    3027
#>   method basemethod   meas    fullmethod splitval
#> 1  edgeR      edgeR __pval edgeR_overall  overall
#> 2  edgeR      edgeR __pval edgeR_overall  overall
#> 3  edgeR      edgeR __pval edgeR_overall  overall
#> 4  edgeR      edgeR __pval edgeR_overall  overall
#> 5  edgeR      edgeR __pval edgeR_overall  overall
#> 6  edgeR      edgeR __pval edgeR_overall  overall
```
