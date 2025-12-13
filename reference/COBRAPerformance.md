# `COBRAPerformance` object and constructor

The `COBRAPerformance` class holds various types of calculated
performance measures. Objects from this class are typically generated
from `COBRAData` objects by means of the function
[`calculate_performance`](https://csoneson.github.io/iCOBRA/reference/calculate_performance.md).

## Usage

``` r
COBRAPerformance(
  fdrtpr = data.frame(),
  fdrtprcurve = data.frame(),
  fdrnbr = data.frame(),
  fdrnbrcurve = data.frame(),
  fsrnbr = data.frame(),
  fsrnbrcurve = data.frame(),
  tpr = data.frame(),
  fpr = data.frame(),
  splv = "",
  roc = data.frame(),
  fpc = data.frame(),
  deviation = data.frame(),
  onlyshared = NA,
  overlap = data.frame(),
  maxsplit = NA_integer_,
  corr = data.frame(),
  scatter = data.frame()
)
```

## Arguments

- fdrtpr:

  A data frame containing observed FDR and TPR values at various
  adjusted p-value thresholds.

- fdrtprcurve:

  A data frame containing observed FDR and TPR values for a (potentially
  large) number of cutoffs applied to a 'score' (that can be p-value,
  adjusted p-value or a more general score).

- fdrnbr:

  A data frame containing observed FDR and the number of features
  considered to be significant at various adjusted p-value thresholds.

- fdrnbrcurve:

  A data frame containing observed FDR and number of features considered
  to be significant for a (potentially large) number of cutoffs applied
  to a 'score' (that can be p-value, adjusted p-value or a more general
  score).

- fsrnbr:

  A data frame containing observed false sign rate (FSR) and the number
  of features considered to be significant at various s-value thresholds

- fsrnbrcurve:

  A data frame containing observed false sign rate (FSR) and number of
  features considered to be significant for a (potentially large) number
  of cutoffs applied to the s-value

- tpr:

  A data frame containing observed TPR values at various adjusted
  p-value thresholds.

- fpr:

  A data frame containing observed FPR values at various adjusted
  p-value thresholds.

- splv:

  A character string giving the name of the stratification factor,
  "none" if the results are not stratified.

- roc:

  A data frame containing observed FPR and TPR values for a (potentially
  large) number of cutoffs applied to a 'score' (that can be p-value,
  adjusted p-value or a more general score), which can be used to
  generate a ROC curve.

- fpc:

  A data frame containing observed numbers of false positive findings
  among the N top-ranked features (ranked by p-values, adjusted p-values
  or more general scores), for a (potentially large) number of Ns, which
  can be used to generate a false positive curve.

- deviation:

  A data frame containing deviations between observed scores and true
  scores.

- onlyshared:

  A logical value indicating whether only features shared between the
  results and the truth should be retained, or if all features present
  in the truth should be used.

- overlap:

  A data frame or list of data frames with binary values indicating, for
  each of a number of methods and number of features, whether the method
  consider the feature 'positive' (significant, 1) or 'negative'
  (non-significant, 0). If it is a list of data frames, each list
  element corresponds to one level of a stratifying factor.

- maxsplit:

  A numeric value indicating the largest number of levels to retain if
  the results have been stratified by an annotation.

- corr:

  A data frame containing observed (Pearson and Spearman) correlation
  values between observed and true scores.

- scatter:

  A data frame containing observed 'scores' (p-values, adjusted p-values
  or more general scores) and true scores, which can be used to generate
  scatter plots.

## Value

A `COBRAPerformance` object.

## Author

Charlotte Soneson

## Examples

``` r
## Empty COBRAPerformance object
COBRAPerformance()
#> An object of class "COBRAPerformance"
#> @fdrtpr
#> data frame with 0 columns and 0 rows
#> 
#> @fdrtprcurve
#> data frame with 0 columns and 0 rows
#> 
#> @fdrnbr
#> data frame with 0 columns and 0 rows
#> 
#> @fdrnbrcurve
#> data frame with 0 columns and 0 rows
#> 
#> @fsrnbr
#> data frame with 0 columns and 0 rows
#> 
#> @fsrnbrcurve
#> data frame with 0 columns and 0 rows
#> 
#> @deviation
#> data frame with 0 columns and 0 rows
#> 
#> @tpr
#> data frame with 0 columns and 0 rows
#> 
#> @fpr
#> data frame with 0 columns and 0 rows
#> 
#> @roc
#> data frame with 0 columns and 0 rows
#> 
#> @scatter
#> data frame with 0 columns and 0 rows
#> 
#> @fpc
#> data frame with 0 columns and 0 rows
#> 
#> @overlap
#> data frame with 0 columns and 0 rows
#> 
#> @corr
#> data frame with 0 columns and 0 rows
#> 
#> @maxsplit
#> [1] NA
#> 
#> @splv
#> [1] ""
#> 
#> @onlyshared
#> [1] NA
#> 
```
