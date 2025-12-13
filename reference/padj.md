# Accessor and replacement functions for `padj` slot

Accessor and replacement functions for the `padj` slot in a `COBRAData`
object.

## Usage

``` r
padj(x, ...)

padj(x, ...) <- value

# S4 method for class 'COBRAData'
padj(x)

# S4 method for class 'COBRAData,data.frame'
padj(x) <- value
```

## Arguments

- x:

  A `COBRAData` object.

- ...:

  Additional arguments.

- value:

  A data frame containing adjusted p-values for each feature and each
  method.

## Value

The accessor function returns a data frame containing adjusted p-values
for each feature and each method.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
head(padj(cobradata_example))
#>                        edgeR         voom
#> ENSG00000000457 2.833295e-01 1.375146e-01
#> ENSG00000000460 2.700376e-01 2.826398e-01
#> ENSG00000000938 9.232794e-01 9.399669e-01
#> ENSG00000000971 9.382227e-01 9.492325e-01
#> ENSG00000001460 6.333969e-20 4.574862e-09
#> ENSG00000001461 4.417195e-01 3.096840e-01
```
