# Accessor and replacement functions for `score` slot

Accessor and replacement functions for the `score` slot in a `COBRAData`
object.

## Usage

``` r
score(x, ...)

score(x, ...) <- value

# S4 method for class 'COBRAData'
score(x)

# S4 method for class 'COBRAData,data.frame'
score(x) <- value
```

## Arguments

- x:

  A `COBRAData` object.

- ...:

  Additional arguments.

- value:

  A data frame containing scores for each feature and each method.

## Value

The accessor function regurns a data frame containing scores for each
feature and each method.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
head(score(cobradata_example))
#>                      edgeR       voom      DESeq2
#> ENSG00000000457 0.22706080 0.23980596 0.274811698
#> ENSG00000000460 0.31050478 0.27677537 0.359226203
#> ENSG00000000938 0.04985025 0.03344113 0.001526355
#> ENSG00000000971 0.03900847 0.02263371 0.008182751
#> ENSG00000001460 3.81423112 4.18152876 3.741077290
#> ENSG00000001461 0.17201376 0.17911584 0.217548745
```
