# Accessor and replacement functions for `sval` slot

Accessor and replacement functions for the `sval` slot in a `COBRAData`
object.

## Usage

``` r
sval(x, ...)

sval(x, ...) <- value

# S4 method for class 'COBRAData'
sval(x)

# S4 method for class 'COBRAData,data.frame'
sval(x) <- value
```

## Arguments

- x:

  A `COBRAData` object.

- ...:

  Additional arguments.

- value:

  A data frame containing s-values for each feature and each method. If
  the object does not have an s-value slot (older versions of the class
  did not have this slot), an empty data frame is returned for
  simplicity.

## Value

The accessor function returns a data frame containing s-values for each
feature and each method.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
head(sval(cobradata_example))
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
#> data frame with 0 columns and 0 rows
```
