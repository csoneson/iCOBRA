# iCOBRA - Interactive benchmarking of ranking and assignment methods

iCOBRA is a package to calculate and visualize performance metrics for 
ranking and binary assignment methods. A typical use case could be, 
for example, comparing methods calling differential expression in 
gene expression experiments, which could be seen as either a ranking 
problem (estimating the correct effect size and ordering the genes by 
significance) or a binary assignment problem (classifying the genes 
into differentially expressed and non-differentially expressed).

iCOBRA can be used either directly from the console, or via the interactive shiny application (see the function `COBRAapp()`).

## Installation

If `devtools` is available, `iCOBRA` can be installed as follows:

```
devtools::install_github("markrobinsonuzh/iCOBRA")
```

## Vignette
The vignette can be found in the vignettes/ directory. Further information is also available in the 'Instructions' tab of the shiny app. 