# Accessor and replacement functions for `corr` slot

Accessor and replacement functions for the `corr` slot in a
`COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
corr(x, ...)

corr(x, ...) <- value

# S4 method for class 'COBRAPerformance'
corr(x)

# S4 method for class 'COBRAPerformance,data.frame'
corr(x) <- value

# S4 method for class 'COBRAPlot,data.frame'
corr(x) <- value
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A data frame giving correlation values for each method and each
  stratification level.

## Value

The accessor function returns a data frame giving correlation values for
each method and each stratification level.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example, cont_truth = "logFC",
                                   aspects = "corr")
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
head(corr(cobraperf))
#>   thr method basemethod    meas     fullmethod splitval   PEARSON  SPEARMAN
#> 1   1 DESeq2     DESeq2 __score DESeq2_overall  overall 0.9396983 0.5453739
#> 2   1  edgeR      edgeR __score  edgeR_overall  overall 0.9400037 0.5441757
#> 3   1   voom       voom __score   voom_overall  overall 0.9337385 0.5406106
```
