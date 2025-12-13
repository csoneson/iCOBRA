# Accessor and replacement functions for `overlap` slot

Accessor and replacement functions for the `overlap` slot in a
`COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
overlap(x, ...)

overlap(x, ...) <- value

# S4 method for class 'COBRAPerformance'
overlap(x)

# S4 method for class 'COBRAPerformance,list_df'
overlap(x) <- value

# S4 method for class 'COBRAPlot,list_df'
overlap(x) <- value
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A data frame or a list, giving information about which feature that
  are classified as 'positive' by each method and for each
  stratification level.

## Value

The accessor function returns a data frame or a list, giving information
about which feature that are classified as 'positive' by each method and
for each stratification level.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example,
                                   binary_truth = "status",
                                   aspects = "overlap")
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
head(overlap(cobraperf))
#>                 edgeR voom truth
#> ENSG00000000457     0    0     0
#> ENSG00000000460     0    0     0
#> ENSG00000000938     0    0     0
#> ENSG00000000971     0    0     0
#> ENSG00000001460     1    1     1
#> ENSG00000001461     0    0     1
```
