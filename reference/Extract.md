# Subsetting `COBRAData`, `COBRAPerformance` or `COBRAPlot` objects

Functions to subset `COBRAData`, `COBRAPerformance` or `COBRAPlot`
objects. `COBRAData` objects are subset by features (rows), while
`COBRAPerformance` and `COBRAPlot` objects are subset by methods
(columns). Numeric indices are not allowed, since not all slots may be
arranged in the same order.

## Usage

``` r
# S4 method for class 'COBRAData'
x[i, j = "missing", drop = "missing"]

# S4 method for class 'COBRAPerformance'
x[i = "missing", j, drop = "missing"]

# S4 method for class 'COBRAPlot'
x[i = "missing", j, drop = "missing"]
```

## Arguments

- x:

  A `COBRAData`, `COBRAPerformance` or `COBRAPlot` object.

- i:

  For `COBRAData` objects, a character vector of feature names to
  retain.

- j:

  For `COBRAPerformance` and `COBRAPlot` objects, a character vector
  with method names to retain.

- drop:

  not used.

## Value

A subset of the original object, of the same class

## Examples

``` r
data(cobradata_example)
cobradata_example[c("ENSG00000000457", "ENSG00000000971",
                    "ENSG00000000460"), ]
#> An object of class "COBRAData"
#> @pval
#>                     edgeR       voom     DESeq2
#> ENSG00000000457 0.1178024 0.05147484 0.01627063
#> ENSG00000000971 0.8449686 0.89394930 0.96278259
#> ENSG00000000460 0.1101413 0.12767845 0.03281416
#> 
#> @padj
#>                     edgeR      voom
#> ENSG00000000457 0.2833295 0.1375146
#> ENSG00000000971 0.9382227 0.9492325
#> ENSG00000000460 0.2700376 0.2826398
#> 
#> @sval
#> data frame with 0 columns and 3 rows
#> 
#> @score
#>                      edgeR       voom      DESeq2
#> ENSG00000000457 0.22706080 0.23980596 0.274811698
#> ENSG00000000971 0.03900847 0.02263371 0.008182751
#> ENSG00000000460 0.31050478 0.27677537 0.359226203
#> 
#> @truth
#>                 status n_isoforms logFC       logFC_cat       expr
#> ENSG00000000457      0          5     0 [ 0.000, 0.297)   8.525774
#> ENSG00000000971      0          6     0 [ 0.000, 0.297) 163.547797
#> ENSG00000000460      0         10     0 [ 0.000, 0.297)   3.314544
#>                            expr_cat
#> ENSG00000000457 [2.85e+00,1.45e+01)
#> ENSG00000000971 [1.45e+01,2.43e+04]
#> ENSG00000000460 [2.85e+00,1.45e+01)
#> 
cobraperf <- calculate_performance(cobradata_example,
                                   binary_truth = "status",
                                   aspects = "fdrtpr")
#> Warning: Object doesn't have a slot sval. Please run update_cobradata(). For consistency, I will return an empty data.frame
#> column DESeq2 is being ignored for NBRS calculations
#> column DESeq2 is being ignored for TPR calculations
#> column DESeq2 is being ignored for FDR calculations
cobraperf[, c("voom")]
#> An object of class "COBRAPerformance"
#> @fdrtpr
#>       thr method basemethod   meas   fullmethod splitval NBR  TP  FP   TN  FN
#> 2 thr0.01   voom       voom __padj voom_overall  overall 684 502 182 1398 317
#> 4 thr0.05   voom       voom __padj voom_overall  overall 783 543 240 1340 276
#> 6  thr0.1   voom       voom __padj voom_overall  overall 866 568 298 1282 251
#>   TOT_CALLED DIFF NONDIFF       TPR       FDR satis   method.satis
#> 2       2399  831    3027 0.6040915 0.2660819    no voom_overallno
#> 4       2399  831    3027 0.6534296 0.3065134    no voom_overallno
#> 6       2399  831    3027 0.6835138 0.3441109    no voom_overallno
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
cobraplot <- prepare_data_for_plot(cobraperf)
cobraplot[, c("voom")]
#> Object up to date
#> An object of class "COBRAPlot"
#> @plotcolors
#>            voom    voom_overall         voomyes voom_overallyes          voomno 
#>       "#00BFC4"       "#00BFC4"       "#00BFC4"       "#00BFC4"         "white" 
#>  voom_overallno 
#>         "white" 
#> 
#> @facetted
#> [1] TRUE
#> 
#> @fdrtpr
#>       thr method basemethod   meas   fullmethod splitval NBR  TP  FP   TN  FN
#> 2 thr0.01   voom       voom __padj voom_overall  overall 684 502 182 1398 317
#> 4 thr0.05   voom       voom __padj voom_overall  overall 783 543 240 1340 276
#> 6  thr0.1   voom       voom __padj voom_overall  overall 866 568 298 1282 251
#>   TOT_CALLED DIFF NONDIFF       TPR       FDR satis   method.satis num_method
#> 2       2399  831    3027 0.6040915 0.2660819    no voom_overallno          2
#> 4       2399  831    3027 0.6534296 0.3065134    no voom_overallno          2
#> 6       2399  831    3027 0.6835138 0.3441109    no voom_overallno          2
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
