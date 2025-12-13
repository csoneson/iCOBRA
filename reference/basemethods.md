# Accessor function for basemethods

Accessor function to extract the methods that are represented in an
`COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
basemethods(x, ...)

# S4 method for class 'COBRAPerformance'
basemethods(x)
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object.

- ...:

  Additional arguments.

## Value

A character vector of all methods represented in the object.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example,
                                   binary_truth = "status",
                                   aspects = "fdrtprcurve")
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
basemethods(cobraperf)
#> [1] "edgeR"  "voom"   "DESeq2"
```
