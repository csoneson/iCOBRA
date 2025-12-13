# Accessor and replacement functions for `roc` slot

Accessor and replacement functions for the `roc` slot in a
`COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
roc(x, ...)

roc(x, ...) <- value

# S4 method for class 'COBRAPerformance'
roc(x)

# S4 method for class 'COBRAPerformance,data.frame'
roc(x) <- value

# S4 method for class 'COBRAPlot,data.frame'
roc(x) <- value
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A data frame giving information necessary to generate ROC curves for
  each method and each stratification level.

## Value

The accessor function returns a data frame giving information necessary
to generate ROC curves for each method and each stratification level.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example,
                                   binary_truth = "status", aspects = "roc")
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
head(roc(cobraperf))
#>          FPR       TPR   ROC_CUTOFF method basemethod   meas    fullmethod
#> 1 0.00000000 0.0000000         -Inf  edgeR      edgeR __pval edgeR_overall
#> 2 0.01916089 0.3646209 0.000000e+00  edgeR      edgeR __pval edgeR_overall
#> 3 0.01916089 0.3694344 1.110223e-16  edgeR      edgeR __pval edgeR_overall
#> 4 0.01916089 0.3706378 2.220446e-16  edgeR      edgeR __pval edgeR_overall
#> 5 0.01916089 0.3718412 3.330669e-16  edgeR      edgeR __pval edgeR_overall
#> 6 0.01916089 0.3730445 5.551115e-16  edgeR      edgeR __pval edgeR_overall
#>   splitval
#> 1  overall
#> 2  overall
#> 3  overall
#> 4  overall
#> 5  overall
#> 6  overall
```
