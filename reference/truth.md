# Accessor and replacement functions for `truth` slot

Accessor and replacement functions for the `truth` slot in a `COBRAData`
object.

## Usage

``` r
truth(x, ...)

truth(x, ...) <- value

# S4 method for class 'COBRAData'
truth(x)

# S4 method for class 'COBRAData,data.frame'
truth(x) <- value
```

## Arguments

- x:

  A `COBRAData` object.

- ...:

  Additional arguments.

- value:

  A data frame containing true assignments and/or scores for features,
  together with other feature annotations to use for stratification of
  performance calculations.

## Value

The accessor function returns a data frame containing true assignments
and/or scores for features, together with other feature annotations to
use for stratification of performance calculations.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
head(truth(cobradata_example))
#>                 status n_isoforms      logFC       logFC_cat       expr
#> ENSG00000000457      0          5 0.00000000 [ 0.000, 0.297)   8.525774
#> ENSG00000000460      0         10 0.00000000 [ 0.000, 0.297)   3.314544
#> ENSG00000000938      0          8 0.00000000 [ 0.000, 0.297)  11.543626
#> ENSG00000000971      0          6 0.00000000 [ 0.000, 0.297) 163.547797
#> ENSG00000001460      1         13 3.93942204 [ 3.540,24.063]   3.792523
#> ENSG00000001461      1          8 0.06022442 [ 0.000, 0.297)  20.396122
#>                            expr_cat
#> ENSG00000000457 [2.85e+00,1.45e+01)
#> ENSG00000000460 [2.85e+00,1.45e+01)
#> ENSG00000000938 [2.85e+00,1.45e+01)
#> ENSG00000000971 [1.45e+01,2.43e+04]
#> ENSG00000001460 [2.85e+00,1.45e+01)
#> ENSG00000001461 [1.45e+01,2.43e+04]
```
