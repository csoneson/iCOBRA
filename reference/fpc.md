# Accessor and replacement functions for `fpc` slot

Accessor and replacement functions for the `fpc` slot in a
`COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
fpc(x, ...)

fpc(x, ...) <- value

# S4 method for class 'COBRAPerformance'
fpc(x)

# S4 method for class 'COBRAPerformance,data.frame'
fpc(x) <- value

# S4 method for class 'COBRAPlot,data.frame'
fpc(x) <- value
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A data frame giving information necessary to generate false positive
  curves for each method and each stratification level.

## Value

The accessor function returns a data frame giving information necessary
to generate false positive curves for each method and each
stratification level.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example,
                                   binary_truth = "status", aspects = "fpc")
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
head(fpc(cobraperf))
#>   topN FP   FPC_CUTOFF method basemethod   meas    fullmethod splitval
#> 1    0  0         -Inf  edgeR      edgeR __pval edgeR_overall  overall
#> 2  361 58 0.000000e+00  edgeR      edgeR __pval edgeR_overall  overall
#> 3  365 58 1.110223e-16  edgeR      edgeR __pval edgeR_overall  overall
#> 4  366 58 2.220446e-16  edgeR      edgeR __pval edgeR_overall  overall
#> 5  367 58 3.330669e-16  edgeR      edgeR __pval edgeR_overall  overall
#> 6  368 58 5.551115e-16  edgeR      edgeR __pval edgeR_overall  overall
```
