# Accessor and replacement functions for `scatter` slot

Accessor and replacement functions for the `scatter` slot in a
`COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
scatter(x, ...)

scatter(x, ...) <- value

# S4 method for class 'COBRAPerformance'
scatter(x)

# S4 method for class 'COBRAPerformance,data.frame'
scatter(x) <- value

# S4 method for class 'COBRAPlot,data.frame'
scatter(x) <- value
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A data frame giving information necessary to generate scatter plots of
  observed vs true values for each method and each stratification level.

## Value

The accessor function returns a data frame giving information necessary
to generate scatter plots of observed vs true values for each method and
each stratification level.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example, cont_truth = "logFC",
                                   aspects = "scatter")
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
head(scatter(cobraperf))
#>   OBSERVATION      TRUTH         feature method basemethod    meas
#> 1  0.22706080 0.00000000 ENSG00000000457  edgeR      edgeR __score
#> 2  0.31050478 0.00000000 ENSG00000000460  edgeR      edgeR __score
#> 3  0.04985025 0.00000000 ENSG00000000938  edgeR      edgeR __score
#> 4  0.03900847 0.00000000 ENSG00000000971  edgeR      edgeR __score
#> 5  3.81423112 3.93942204 ENSG00000001460  edgeR      edgeR __score
#> 6  0.17201376 0.06022442 ENSG00000001461  edgeR      edgeR __score
#>      fullmethod splitval
#> 1 edgeR_overall  overall
#> 2 edgeR_overall  overall
#> 3 edgeR_overall  overall
#> 4 edgeR_overall  overall
#> 5 edgeR_overall  overall
#> 6 edgeR_overall  overall
```
