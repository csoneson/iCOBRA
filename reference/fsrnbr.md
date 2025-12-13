# Accessor and replacement functions for `fsrnbr` slot

Accessor and replacement functions for the `fsrnbr` slot in a
`COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
fsrnbr(x, ...)

fsrnbr(x, ...) <- value

# S4 method for class 'COBRAPerformance'
fsrnbr(x)

# S4 method for class 'COBRAPerformance,data.frame'
fsrnbr(x) <- value

# S4 method for class 'COBRAPlot,data.frame'
fsrnbr(x) <- value
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A data frame giving information about the observed FSR and the number
  of features called positive for each method and each stratification
  level, at various s-value thresholds. If the object does not have an
  fsrnbr slot (older versions of the class did not have this slot), an
  empty data frame is returned for simplicity.

## Value

The accessor function returns a data frame giving information about the
observed FSR and the number of features called positive for each method
and each stratification level, at various s-value thresholds.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example_sval)

cobraperf <- calculate_performance(cobradata_example_sval, 
                                   cont_truth = "logFC", 
                                   aspects = "fsrnbr")
#> column Method3 is being ignored for FSR/NBR calculations
head(fsrnbr(cobraperf))
#>       thr  method basemethod   meas      fullmethod splitval         FSR satis
#> 1 thr0.01 Method1    Method1 __sval Method1_overall  overall 0.000000000   yes
#> 2 thr0.01 Method2    Method2 __sval Method2_overall  overall 0.000000000   yes
#> 3 thr0.05 Method1    Method1 __sval Method1_overall  overall 0.001612903   yes
#> 4 thr0.05 Method2    Method2 __sval Method2_overall  overall 0.000000000   yes
#> 5  thr0.1 Method1    Method1 __sval Method1_overall  overall 0.002869440   yes
#> 6  thr0.1 Method2    Method2 __sval Method2_overall  overall 0.003072197   yes
#>         method.satis NBR  TS FS POSSIGN NEGSIGN ZEROSIGN TOT_CALLED
#> 1 Method1_overallyes 508 387  0     420     422     3016       2418
#> 2 Method2_overallyes 392 308  0     420     422     3016       2418
#> 3 Method1_overallyes 620 447  1     420     422     3016       2418
#> 4 Method2_overallyes 565 406  0     420     422     3016       2418
#> 5 Method1_overallyes 697 476  2     420     422     3016       2418
#> 6 Method2_overallyes 651 446  2     420     422     3016       2418
```
