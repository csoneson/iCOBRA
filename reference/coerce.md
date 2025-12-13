# Convert an object to another class

Convert object between `COBRAPerformance` and `COBRAPlot` classes.

## Arguments

- from:

  The object that is to be coerced into another class.

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
cobraplot <- prepare_data_for_plot(cobraperf)

## Coerce COBRAPerformance object into COBRAPlot object
as(cobraperf, "COBRAPlot")
#> An object of class "COBRAPlot"
#> @plotcolors
#> [1] ""
#> 
#> @facetted
#> [1] TRUE
#> 
#> @fdrtpr
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
#> [1] 3
#> 
#> @splv
#> [1] "none"
#> 
#> @onlyshared
#> [1] FALSE
#> 

## Coerce COBRAPlot object into COBRAPerformance object
as(cobraplot, "COBRAPerformance")
#> An object of class "COBRAPerformance"
#> @fdrtpr
#>       thr method basemethod   meas    fullmethod splitval NBR  TP  FP   TN  FN
#> 1 thr0.01  edgeR      edgeR __padj edgeR_overall  overall 686 499 187 1393 320
#> 2 thr0.01   voom       voom __padj  voom_overall  overall 684 502 182 1398 317
#> 3 thr0.05  edgeR      edgeR __padj edgeR_overall  overall 761 526 235 1345 293
#> 4 thr0.05   voom       voom __padj  voom_overall  overall 783 543 240 1340 276
#> 5  thr0.1  edgeR      edgeR __padj edgeR_overall  overall 836 552 284 1296 267
#> 6  thr0.1   voom       voom __padj  voom_overall  overall 866 568 298 1282 251
#>   TOT_CALLED DIFF NONDIFF       TPR       FDR satis    method.satis num_method
#> 1       2399  831    3027 0.6004813 0.2725948    no edgeR_overallno          1
#> 2       2399  831    3027 0.6040915 0.2660819    no  voom_overallno          2
#> 3       2399  831    3027 0.6329723 0.3088042    no edgeR_overallno          1
#> 4       2399  831    3027 0.6534296 0.3065134    no  voom_overallno          2
#> 5       2399  831    3027 0.6642599 0.3397129    no edgeR_overallno          1
#> 6       2399  831    3027 0.6835138 0.3441109    no  voom_overallno          2
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
#> [1] 3
#> 
#> @splv
#> [1] "none"
#> 
#> @onlyshared
#> [1] FALSE
#> 
```
