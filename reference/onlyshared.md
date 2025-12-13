# Accessor and replacement functions for `onlyshared` slot

Accessor and replacement functions for the `onlyshared` slot in a
`COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
onlyshared(x, ...)

onlyshared(x, ...) <- value

# S4 method for class 'COBRAPerformance'
onlyshared(x)

# S4 method for class 'COBRAPerformance,logical'
onlyshared(x) <- value

# S4 method for class 'COBRAPlot,logical'
onlyshared(x) <- value
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A logical indicating whether only features that are shared between
  result and truth are retained, or if all features in the truth are
  used.

## Value

The accessor function returns a logical indicating whether only features
that are shared between result and truth are retained, or if all
features in the truth are used.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example,
                                   binary_truth = "status",
                                   aspects = "fdrtpr")
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
#> column DESeq2 is being ignored for NBRS calculations
#> column DESeq2 is being ignored for TPR calculations
#> column DESeq2 is being ignored for FDR calculations
head(onlyshared(cobraperf))
#> [1] FALSE
```
