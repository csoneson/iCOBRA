# Accessor and replacement functions for `fdrtpr` slot

Accessor and replacement functions for the `fdrtpr` slot in a
`COBRAPerformance` or `COBRAPlot` object.

## Usage

``` r
fdrtpr(x, ...)

fdrtpr(x, ...) <- value

# S4 method for class 'COBRAPerformance'
fdrtpr(x)

# S4 method for class 'COBRAPerformance,data.frame'
fdrtpr(x) <- value

# S4 method for class 'COBRAPlot,data.frame'
fdrtpr(x) <- value
```

## Arguments

- x:

  A `COBRAPerformance` or `COBRAPlot` object.

- ...:

  Additional arguments.

- value:

  A data frame giving information about the observed FPR and TPR for
  each method and each stratification level, at various adjusted p-value
  thresholds.

## Value

The accessor function returns a data frame giving information about the
observed FPR and TPR for each method and each stratification level, at
various adjusted p-value thresholds.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example,
                                   binary_truth = "status",
                                   aspects = "fdrtpr")
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
#> column DESeq2 is being ignored for NBRS calculations
#> column DESeq2 is being ignored for TPR calculations
#> column DESeq2 is being ignored for FDR calculations
head(fdrtpr(cobraperf))
#>       thr method basemethod   meas    fullmethod splitval NBR  TP  FP   TN  FN
#> 1 thr0.01  edgeR      edgeR __padj edgeR_overall  overall 686 499 187 1393 320
#> 2 thr0.01   voom       voom __padj  voom_overall  overall 684 502 182 1398 317
#> 3 thr0.05  edgeR      edgeR __padj edgeR_overall  overall 761 526 235 1345 293
#> 4 thr0.05   voom       voom __padj  voom_overall  overall 783 543 240 1340 276
#> 5  thr0.1  edgeR      edgeR __padj edgeR_overall  overall 836 552 284 1296 267
#> 6  thr0.1   voom       voom __padj  voom_overall  overall 866 568 298 1282 251
#>   TOT_CALLED DIFF NONDIFF       TPR       FDR satis    method.satis
#> 1       2399  831    3027 0.6004813 0.2725948    no edgeR_overallno
#> 2       2399  831    3027 0.6040915 0.2660819    no  voom_overallno
#> 3       2399  831    3027 0.6329723 0.3088042    no edgeR_overallno
#> 4       2399  831    3027 0.6534296 0.3065134    no  voom_overallno
#> 5       2399  831    3027 0.6642599 0.3397129    no edgeR_overallno
#> 6       2399  831    3027 0.6835138 0.3441109    no  voom_overallno
```
