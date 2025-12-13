# Prepare data for plotting

Prepare performance data provided in a `COBRAPerformance` object
(obtained by
[`calculate_performance`](https://csoneson.github.io/iCOBRA/reference/calculate_performance.md))
for plotting.

## Usage

``` r
prepare_data_for_plot(
  cobraperf,
  keepmethods = NULL,
  incloverall = TRUE,
  colorscheme = "hue_pal",
  facetted = TRUE,
  incltruth = TRUE,
  conditionalfill = TRUE
)
```

## Arguments

- cobraperf:

  A `COBRAPerformance` object.

- keepmethods:

  A character vector consisting of methods to retain for plotting (these
  should be a subset of `basemethods(cobraperf)`), or NULL (indicating
  that all methods represented in cobraperf should be retained).

- incloverall:

  A logical indicating whether the "overall" results should be included
  if the results are stratified by an annotation.

- colorscheme:

  Either a character string giving the color palette to use to define
  colors for the different methods, or a character vector with colors to
  use. The available pre-defined palettes depend on the number of
  different methods to distinguish. The choices are:

  \- `Accent`

  :   (max 8 methods)

  \- `Dark2`

  :   (max 8 methods)

  \- `Paired`

  :   (max 12 methods)

  \- `Pastel1`

  :   (max 9 methods)

  \- `Pastel2`

  :   (max 8 methods)

  \- `Set1`

  :   (max 9 methods)

  \- `Set2`

  :   (max 8 methods)

  \- `Set3`

  :   (max 12 methods)

  \- `hue_pal`

  :   

  \- `rainbow`

  :   

  \- `heat`

  :   

  \- `terrain`

  :   

  \- `topo`

  :   

  \- `cm`

  :   

  If the number of allowed methods is exceeded, the colorscheme defaults
  to `hue_pal`.

- facetted:

  A logical indicating whether the results should be split into
  subpanels when stratified by an annotation (`TRUE`), or kept in the
  same panel but shown with different colors (`FALSE`).

- incltruth:

  A logical indicating whether the truth should be included in Venn
  diagrams.

- conditionalfill:

  A logical indicating whether the points (in FDR/TPR, FDR/NBR, FSR/NBR
  plots) should be filled conditional on whether they satisfy the
  imposed criterion (e.g., false discovery rate control at imposed
  threshold).

## Value

A `COBRAPlot` object

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example,
                                   binary_truth = "status",
                                   cont_truth = "none",
                                   aspects = c("fdrtpr", "fdrtprcurve",
                                               "tpr", "roc"),
                                   thrs = c(0.01, 0.05, 0.1), splv = "none")
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
#> column DESeq2 is being ignored for NBRS calculations
#> column DESeq2 is being ignored for TPR calculations
#> column DESeq2 is being ignored for FDR calculations
cobraplot <- prepare_data_for_plot(cobraperf, keepmethods = NULL,
                                   colorscheme = "Dark2")

## User-specified colors
cobraplot2 <- prepare_data_for_plot(cobraperf, keepmethods = NULL,
                                   colorscheme = c("blue", "red", "green"))
```
