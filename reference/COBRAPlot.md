# `COBRAPlot` object and constructor

The `COBRAPlot` class is similar to the `COBRAPerformance` class in that
it holds various types of calculated performance measures. However, it
also contains other attributes that are necessary for plotting, such as
color assignments. Several `COBRAPlot` objects can be generated from the
same `COBRAPerformance` object, without having to go through the
potentially time consuming task of recalculating all performance
measures. Objects from this class are typically generated from a
`COBRAPerformance` objects by means of the function
[`prepare_data_for_plot`](https://csoneson.github.io/iCOBRA/reference/prepare_data_for_plot.md).

## Usage

``` r
COBRAPlot(
  fdrtpr = data.frame(),
  fdrtprcurve = data.frame(),
  fsrnbr = data.frame(),
  fsrnbrcurve = data.frame(),
  fdrnbr = data.frame(),
  corr = data.frame(),
  fdrnbrcurve = data.frame(),
  tpr = data.frame(),
  fpr = data.frame(),
  roc = data.frame(),
  scatter = data.frame(),
  onlyshared = NA,
  fpc = data.frame(),
  overlap = data.frame(),
  plotcolors = "",
  splv = "",
  deviation = data.frame(),
  maxsplit = NA_integer_,
  facetted = NA
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

- fsrnbr:

  A data frame containing observed false sign rate (FSR) and the number
  of features considered to be significant at various s-value thresholds

- fsrnbrcurve:

  A data frame containing observed false sign rate (FSR) and number of
  features considered to be significant for a (potentially large) number
  of cutoffs applied to the s-value

- fdrnbr:

  A data frame containing observed FDR and the number of features
  considered to be significant at various adjusted p-value thresholds.

- corr:

  A data frame containing observed (Pearson and Spearman) correlation
  values between observed and true scores.

- fdrnbrcurve:

  A data frame containing observed FDR and number of features considered
  to be significant for a (potentially large) number of cutoffs applied
  to a 'score' (that can be p-value, adjusted p-value or a more general
  score).

- tpr:

  A data frame containing observed TPR values at various adjusted
  p-value thresholds.

- fpr:

  A data frame containing observed FPR values at various adjusted
  p-value thresholds.

- roc:

  A data frame containing observed FPR and TPR values for a (potentially
  large) number of cutoffs applied to a 'score' (that can be p-value,
  adjusted p-value or a more general score), which can be used to
  generate a ROC curve.

- scatter:

  A data frame containing observed 'scores' (p-values, adjusted p-values
  or more general scores) and true scores, which can be used to generate
  scatter plots.

- onlyshared:

  A logical value indicating whether only features shared between the
  results and the truth should be retained, or if all features present
  in the truth should be used.

- fpc:

  A data frame containing observed numbers of false positive findings
  among the N top-ranked features (ranked by p-values, adjusted p-values
  or more general scores), for a (potentially large) number of Ns, which
  can be used to generate a false positive curve.

- overlap:

  A data frame or list of data frames with binary values indicating, for
  each of a number of methods and number of features, whether the method
  consider the feature 'positive' (significant, 1) or 'negative'
  (non-significant, 0). If it is a list of data frames, each list
  element corresponds to one level of a stratifying factor.

- plotcolors:

  A character vector giving the color for each method (or
  method-stratification level combination).

- splv:

  A character string giving the name of the stratification factor,
  "none" if the results are not stratified.

- deviation:

  A data frame containing deviations between observed scores and true
  scores.

- maxsplit:

  A numeric value indicating the largest number of levels to retain if
  the results have been stratified by an annotation.

- facetted:

  A logical indicating whether the data is prepared for a facetted plot
  (separating different stratification levels into different panels) or
  for displaying all values in one plot panel.

## Value

A `COBRAPlot` object.

## Author

Charlotte Soneson

## Examples

``` r
## Empty COBRAPlot object
cobraplot <- COBRAPlot()
```
