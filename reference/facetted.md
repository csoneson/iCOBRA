# Accessor and replacement functions for `facetted` slot

Accessor and replacement functions for the `facetted` slot in an
`COBRAPlot` object.

## Usage

``` r
facetted(x, ...)

facetted(x, ...) <- value

# S4 method for class 'COBRAPlot'
facetted(x)

# S4 method for class 'COBRAPlot,logical'
facetted(x) <- value
```

## Arguments

- x:

  A `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A logical value, indicating whether the object is formatted for
  facetted plots (visualizing each stratification level in a separate
  panel) or not.

## Value

The accessor function returns a logical value, indicating whether the
object is formatted for facetted plots (visualizing each stratification
level in a separate panel) or not.

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
cobraplot <- prepare_data_for_plot(cobraperf)
facetted(cobraplot)
#> [1] TRUE
```
