# Calculate adjusted p-values

Calculate adjusted p-values for methods where only nominal p-values are
available in a `COBRAData` object.

## Usage

``` r
calculate_adjp(cobradata, method = "BH")
```

## Arguments

- cobradata:

  A `COBRAData` object.

- method:

  A character string giving the method (selected from
  [`p.adjust.methods()`](https://rdrr.io/r/stats/p.adjust.html)) that
  will be used to perform the adjustment.

## Value

A `COBRAData` object, extended with the calculated adjusted p-values.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobradata_example <- calculate_adjp(cobradata_example, method = "BH")
#> Adding empty sval slot to object
#> Object up to date
```
