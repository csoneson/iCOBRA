# Reorder levels in COBRAPlot object

Reorder levels in COBRAPlot object to achieve desired ordering in figure
legends etc. If facetted(cobraplot) is TRUE, the releveling will be
applied to the "method" column. If facetted(cobraplot) is FALSE, it will
be applied to the "fullmethod" column.

## Usage

``` r
reorder_levels(cobraplot, levels)
```

## Arguments

- cobraplot:

  A COBRAPlot object

- levels:

  A character vector giving the order of the levels. Any values not
  present in the COBRAPlot object will be removed. Any methods present
  in the COBRAPlot object but not contained in this vector will be added
  at the end.

## Value

A COBRAPlot object

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example_sval)
cobraperf <- calculate_performance(cobradata_example_sval,
                                   binary_truth = "status", aspects = "fpr")
#> column Method3 is being ignored for NBRS calculations
#> column Method3 is being ignored for FPR calculations
cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2",
                                   incltruth = TRUE)
cobraplot <- reorder_levels(cobraplot, c("Method2", "Method1"))
```
