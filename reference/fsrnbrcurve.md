# Accessor and replacement functions for `fsrnbrcurve` slot

Accessor and replacement functions for the `fsrnbrcurve` slot in a
`COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
fsrnbrcurve(x, ...)

fsrnbrcurve(x, ...) <- value

# S4 method for class 'COBRAPerformance'
fsrnbrcurve(x)

# S4 method for class 'COBRAPerformance,data.frame'
fsrnbrcurve(x) <- value

# S4 method for class 'COBRAPlot,data.frame'
fsrnbrcurve(x) <- value
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A data frame giving information necessary to generate curves of
  observed FSR vs number of features called positive for each method and
  each stratification level. If the object does not have an fsrnbrcurve
  slot (older versions of the class did not have this slot), an empty
  data frame is returned for simplicity.

## Value

The accessor function returns a data frame giving information necessary
to generate curves of observed FSR vs number of features called positive
for each method and each stratification level.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example_sval)

cobraperf <- calculate_performance(cobradata_example_sval, 
                                   cont_truth = "logFC", 
                                   aspects = "fsrnbrcurve")
#> column Method3 is being ignored for FSRNBR calculations
head(fsrnbrcurve(cobraperf))
#>   FSR NBR       CUTOFF TS FS TOT_CALLED POSSIGN NEGSIGN ZEROSIGN  method
#> 1   0   1 3.154663e-47  1  0       2418     420     422     3016 Method1
#> 2   0   2 7.291524e-47  2  0       2418     420     422     3016 Method1
#> 3   0   3 1.186072e-46  3  0       2418     420     422     3016 Method1
#> 4   0   4 4.595996e-45  4  0       2418     420     422     3016 Method1
#> 5   0   6 1.075890e-44  6  0       2418     420     422     3016 Method1
#> 6   0   7 1.171043e-41  7  0       2418     420     422     3016 Method1
#>   basemethod   meas      fullmethod splitval
#> 1    Method1 __sval Method1_overall  overall
#> 2    Method1 __sval Method1_overall  overall
#> 3    Method1 __sval Method1_overall  overall
#> 4    Method1 __sval Method1_overall  overall
#> 5    Method1 __sval Method1_overall  overall
#> 6    Method1 __sval Method1_overall  overall
```
