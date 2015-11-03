# iCOBRA - Interactive benchmarking of ranking and assignment methods

`iCOBRA` is a package to calculate and visualize performance metrics for 
ranking and binary assignment methods. A typical use case could be, 
for example, comparing methods calling differential expression in 
gene expression experiments, which could be seen as either a ranking 
problem (estimating the correct effect size and ordering the genes by 
significance) or a binary assignment problem (classifying the genes 
into differentially expressed and non-differentially expressed).

`iCOBRA` can be used either directly from the console, or via the 
interactive shiny application (see the function `COBRAapp()`). It can also 
be accessed from the web server [http://imlspenticton.uzh.ch:3838/iCOBRA/](http://imlspenticton.uzh.ch:3838/iCOBRA/)

## Installation

If `devtools` is available, `iCOBRA` can be installed as follows:

```
devtools::install_github("markrobinsonuzh/iCOBRA")
```

## Quick start guide

The `iCOBRA` workflow starts from an object of class `COBRAData`, 
containing (adjusted) p-values and/or scores for a set of features as 
well as the true status of the features. An example data set is provided in 
the package

```
library(iCOBRA)
data(cobradata_example)
```

The function `calculate_performance()` calculates the different performance 
metrics for a `COBRAData` object. By default, all performance metrics are 
calculated, but a subset can be selected using the `aspects` argument.

```
cobraperf <- calculate_performance(cobradata_example, binary_truth = "status",
                                   cont_truth = "logFC", 
                                   aspects = c("fdrtpr", "fdrtprcurve", 
                                               "corr"))
```

Next, the performance metrics are prepared for plotting using the 
`prepare_for_plot()` function. This function defines colors for the 
various methods and can also be used for selecting only a subset of the 
methods for visualization, without having to recalculate the performance metrics.

```
cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Set2",
                                   keepmethods = c("voom", "edgeR"))
```

The resulting object can then be used to generate plots of the selected aspects.

```
plot_fdrtprcurve(cobraplot)
plot_corr(cobraplot, corrtype = "spearman")
```

## Vignette

The vignette can be found in the `vignettes/` directory. Further 
information is also available in the 'Instructions' tab of the shiny app. 
After installation, the vignette can be accessed from the R console by typing

```
browseVignettes("iCOBRA")
```

## Benchmarking data set collection

To facilitate future benchmarking studies, we have collected a set of benchmarking 
data sets on [http://imlspenticton.uzh.ch/robinson_lab/benchmark_collection/](http://imlspenticton.uzh.ch/robinson_lab/benchmark_collection/). The page provides 
links to raw data as well as evaluation results suitable for import into `iCOBRA`. 
