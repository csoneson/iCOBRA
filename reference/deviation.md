# Accessor and replacement functions for `deviation` slot

Accessor and replacement functions for the `deviation` slot in a
`COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
deviation(x, ...)

deviation(x, ...) <- value

# S4 method for class 'COBRAPerformance'
deviation(x)

# S4 method for class 'COBRAPerformance,data.frame'
deviation(x) <- value

# S4 method for class 'COBRAPlot,data.frame'
deviation(x) <- value
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A data frame giving information necessary to plots of deviations
  between observed and true scores for each method and each
  stratification level.

## Value

The accessor function returns a data frame giving information necessary
to plots of deviations between observed and true scores for each method
and each stratification level.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example, cont_truth = "logFC",
                                   aspects = "deviation")
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
head(deviation(cobraperf))
#>     DEVIATION         feature method basemethod    meas    fullmethod splitval
#> 1  0.22706080 ENSG00000000457  edgeR      edgeR __score edgeR_overall  overall
#> 2  0.31050478 ENSG00000000460  edgeR      edgeR __score edgeR_overall  overall
#> 3  0.04985025 ENSG00000000938  edgeR      edgeR __score edgeR_overall  overall
#> 4  0.03900847 ENSG00000000971  edgeR      edgeR __score edgeR_overall  overall
#> 5 -0.12519092 ENSG00000001460  edgeR      edgeR __score edgeR_overall  overall
#> 6  0.11178934 ENSG00000001461  edgeR      edgeR __score edgeR_overall  overall
```
