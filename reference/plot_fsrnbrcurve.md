# Plot number of features with s-value below threshold vs FSR

Plot the number of features with an s-value below a threshold vs the
observed false sign rate (FSR), for given adjusted p-value thresholds
and/or as curves traced out by considering all threshold values.

## Usage

``` r
plot_fsrnbrcurve(
  cobraplot,
  title = "",
  stripsize = 15,
  titlecol = "black",
  pointsize = 5,
  xaxisrange = c(0, 1),
  plottype = c("curve", "points"),
  linewidth = 1
)
```

## Arguments

- cobraplot:

  A `COBRAPlot` object.

- title:

  A character string giving the title of the plot.

- stripsize:

  A numeric value giving the size of the strip text, when the results
  are stratified by an annotation.

- titlecol:

  A character string giving the color of the title.

- pointsize:

  A numeric value giving the size of the plot characters.

- xaxisrange:

  A numeric vector with two elements, giving the lower and upper
  boundary of the x-axis, respectively.

- plottype:

  A character vector giving the type of plot to construct. Can be any
  combination of the two elements "curve" and "points".

- linewidth:

  The line width used for plotting

## Value

A ggplot object

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example_sval)
cobraperf <- calculate_performance(cobradata_example_sval,
                                   cont_truth = "logFC",
                                   aspects = c("fsrnbr", "fsrnbrcurve"))
#> column Method3 is being ignored for FSR/NBR calculations
#> column Method3 is being ignored for FSRNBR calculations
cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2",
                                   incltruth = TRUE)
plot_fsrnbrcurve(cobraplot, plottype = c("curve", "points"))
```
