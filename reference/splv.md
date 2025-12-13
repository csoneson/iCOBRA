# Accessor and replacement functions for `splv` slot

Accessor and replacement functions for the `splv` slot in a
`COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
splv(x, ...)

splv(x, ...) <- value

# S4 method for class 'COBRAPerformance'
splv(x)

# S4 method for class 'COBRAPerformance,character'
splv(x) <- value

# S4 method for class 'COBRAPlot,character'
splv(x) <- value
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A character string giving the name of a feature annotation to use for
  stratification.

## Value

The accessor function returns a character string giving the name of a
feature annotation to use for stratification.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example,
                                   binary_truth = "status",
                                   aspects = "fdrtpr", splv = "expr_cat")
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
#> column DESeq2 is being ignored for NBRS calculations
#> column DESeq2 is being ignored for TPR calculations
#> column DESeq2 is being ignored for FDR calculations
splv(cobraperf)
#> [1] "expr_cat"
```
