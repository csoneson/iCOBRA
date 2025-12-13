# Accessor and replacement functions for `maxsplit` slot

Accessor and replacement functions for the `maxsplit` slot in a
`COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
maxsplit(x, ...)

maxsplit(x, ...) <- value

# S4 method for class 'COBRAPerformance'
maxsplit(x)

# S4 method for class 'COBRAPerformance,numeric'
maxsplit(x) <- value

# S4 method for class 'COBRAPlot,numeric'
maxsplit(x) <- value
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A numeric value giving the maximal number of strata to retain.

## Value

The accessor function returns a numeric value giving the maximal number
of strata to retain.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example,
                                   binary_truth = "status",
                                   aspects = "fdrtpr", splv = "expr_cat",
                                   maxsplit = 3)
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
#> column DESeq2 is being ignored for NBRS calculations
#> column DESeq2 is being ignored for TPR calculations
#> column DESeq2 is being ignored for FDR calculations
maxsplit(cobraperf)
#> [1] 3
```
