# Accessor and replacement functions for `plotcolors` slot

Accessor and replacement functions for the `plotcolors` slot in an
`COBRAPlot` object.

## Usage

``` r
plotcolors(x, ...)

plotcolors(x, ...) <- value

# S4 method for class 'COBRAPlot'
plotcolors(x)

# S4 method for class 'COBRAPlot,character'
plotcolors(x) <- value
```

## Arguments

- x:

  A `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A character vector giving the colors assigned to each of the methods
  (or method/stratification level combinations) represented in the
  `COBRAPlot` object.

## Value

The accessor function returns a character vector giving the colors
assigned to each of the methods (or method/stratification level
combinations) represented in the `COBRAPlot` object.

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
plotcolors(cobraplot)
#>            edgeR             voom    edgeR_overall     voom_overall 
#>        "#F8766D"        "#00BFC4"        "#F8766D"        "#00BFC4" 
#>         edgeRyes          voomyes edgeR_overallyes  voom_overallyes 
#>        "#F8766D"        "#00BFC4"        "#F8766D"        "#00BFC4" 
#>          edgeRno           voomno  edgeR_overallno   voom_overallno 
#>          "white"          "white"          "white"          "white" 
```
