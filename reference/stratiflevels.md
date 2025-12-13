# Accessor function for stratification levels

Accessor function to extract the stratification levels that are
represented in a `COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
stratiflevels(x, ...)

# S4 method for class 'COBRAPerformance'
stratiflevels(x)
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object

- ...:

  Additional arguments

## Value

A character vector of all stratification levels represented in the
object

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example,
                                   binary_truth = "status",
                                   aspects = "fdrtpr", splv = "expr_cat",
                                   maxsplit = 4)
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
#> column DESeq2 is being ignored for NBRS calculations
#> column DESeq2 is being ignored for TPR calculations
#> column DESeq2 is being ignored for FDR calculations
stratiflevels(cobraperf)
#> [1] "expr_cat:[0.00e+00,2.77e-01)" "expr_cat:[1.45e+01,2.43e+04]"
#> [3] "expr_cat:[2.77e-01,2.85e+00)" "expr_cat:[2.85e+00,1.45e+01)"
#> [5] "overall"                     
```
