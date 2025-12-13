# Accessor and replacement functions for `pval` slot

Accessor and replacement functions for the `pval` slot in a `COBRAData`
object.

## Usage

``` r
pval(x, ...)

pval(x, ...) <- value

# S4 method for class 'COBRAData'
pval(x)

# S4 method for class 'COBRAData,data.frame'
pval(x) <- value
```

## Arguments

- x:

  A `COBRAData` object.

- ...:

  Additional arguments.

- value:

  A data frame containing p-values for each feature and each method.

## Value

The accessor function returns a data frame containing p-values for each
feature and each method.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
head(pval(cobradata_example))
#>                        edgeR         voom       DESeq2
#> ENSG00000000457 1.178024e-01 5.147484e-02 1.627063e-02
#> ENSG00000000460 1.101413e-01 1.276784e-01 3.281416e-02
#> ENSG00000000938 8.128245e-01 8.712829e-01 9.942761e-01
#> ENSG00000000971 8.449686e-01 8.939493e-01 9.627826e-01
#> ENSG00000001460 8.343203e-21 6.007010e-10 9.303159e-23
#> ENSG00000001461 2.200312e-01 1.444503e-01 8.245376e-02
```
