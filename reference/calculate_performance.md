# Calculate performance measures

Calculate performance measures from a given collection of p-values,
adjusted p-values and scores provided in a `COBRAData` object.

## Usage

``` r
calculate_performance(
  cobradata,
  binary_truth = NULL,
  cont_truth = NULL,
  aspects = c("fdrtpr", "fdrtprcurve", "fdrnbr", "fdrnbrcurve", "tpr", "fpr", "roc",
    "fpc", "overlap", "corr", "scatter", "deviation", "fsrnbr", "fsrnbrcurve"),
  thrs = c(0.01, 0.05, 0.1),
  svalthrs = c(0.01, 0.05, 0.1),
  splv = "none",
  maxsplit = 3,
  onlyshared = FALSE,
  thr_venn = 0.05,
  type_venn = "adjp",
  topn_venn = 100,
  rank_by_abs = TRUE,
  prefer_pval = TRUE
)
```

## Arguments

- cobradata:

  A `COBRAData` object.

- binary_truth:

  A character string giving the name of the column of truth(cobradata)
  that contains the binary truth (true assignment of variables into two
  classes, represented by 0/1).

- cont_truth:

  A character string giving the name of the column of truth(cobradata)
  that contains the continuous truth (a continuous value that the
  observations can be compared to).

- aspects:

  A character vector giving the types of performance measures to
  calculate. Must be a subset of c("fdrtpr", "fdrtprcurve", "fdrnbr",
  "fdrnbrcurve", "tpr", "fpr", "roc", "fpc", "overlap", "corr",
  "scatter", "deviation", "fsrnbr", "fsrnbrcurve").

- thrs:

  A numeric vector of adjusted p-value thresholds for which to calculate
  the performance measures. Affects "fdrtpr", "fdrnbr", "tpr" and "fpr".

- svalthrs:

  A numeric vector of s-value thresholds for which to calculate the FSR.
  Affects "fsrnbr".

- splv:

  A character string giving the name of the column of truth(cobradata)
  that will be used to stratify the results. The default value is
  "none", indicating no stratification.

- maxsplit:

  A numeric value giving the maximal number of categories to keep in the
  stratification. The largest categories containing both positive and
  negative features will be retained. By setting this argument to
  \`Inf\` or \`NA_integer\_\`, all categories (as well as the order of
  categories) will be retained.

- onlyshared:

  A logical, indicating whether to only consider features for which both
  the true assignment and a result (p-value, adjusted p-value or score)
  is given. If FALSE, all features contained in the truth table are
  used.

- thr_venn:

  A numeric value giving the adjusted p-value threshold to use to create
  Venn diagrams (if `type_venn` is "adjp").

- type_venn:

  Either "adjp" or "rank", indicating whether Venn diagrams should be
  constructed based on features with adjusted p-values below a certain
  threshold, or based on the same number of top-ranked features by
  different methods.

- topn_venn:

  A numeric value giving the number of top-ranked features to compare
  between methods (if `type_venn` is "rank").

- rank_by_abs:

  Whether to take the absolute value of the score before using it to
  rank the variables for ROC, FPC, FDR/NBR and FDR/TPR curves.

- prefer_pval:

  Whether to preferentially rank variables by p-values or adjusted
  p-values rather than score for ROC and FPC calculations. From version
  1.5.5, this is the default behaviour. To obtain the behaviour of
  previous versions, set to `FALSE`.

## Value

A `COBRAPerformance` object

## Details

Depending on the collection of observations that are available for a
given method, the appropriate one will be chosen for each performance
measure. For `fpr`, `tpr`, `fdrtpr`, `fdrnbr` and `overlap` aspects,
results will only be calculated for methods where adjusted p-values are
included in the `COBRAData` object, since these calculations make use of
specific adjusted p-value cutoffs. For `fdrtprcurve` and `fdrnbrcurve`
aspects, the `score` observations will be preferentially used, given
that they are monotonically associated with the adjusted p-values (if
provided). If the `score` is not provided, the nominal p-values will be
used, given that they are monotonically associated with the adjusted
p-values (if provided). In other cases, the adjusted p-values will be
used also for these aspects. For `roc` and `fpc`, the `score`
observations will be used if they are provided, otherwise p-values and,
as a last instance, adjusted p-values. Finally, for the `fsrnbr`,
`corr`, `scatter` and `deviation` aspects, the `score` observations will
be used if they are provided, otherwise no results will be calculated.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example,
                                   binary_truth = "status",
                                   aspects = c("fdrtpr", "fdrtprcurve",
                                               "tpr", "roc"),
                                   thrs = c(0.01, 0.05, 0.1), splv = "none")
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
#> column DESeq2 is being ignored for NBRS calculations
#> column DESeq2 is being ignored for TPR calculations
#> column DESeq2 is being ignored for FDR calculations
```
