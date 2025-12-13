# Example data set with three differential gene expression methods

A data set consisting of p-values, adjusted p-values and estimated
absolute log fold changes (in the 'scores' slot) obtained by three
methods for differential expression analysis of RNA-seq data, applied to
a small synthetic data set of 3,858 human genes. The values are stored
in a `COBRAData` object together with a 'truth' data frame containing
the true differential expression status for each gene as well as various
additional annotations such as the true log fold change, the number of
isoforms of the gene and the average expression level.

## Format

A `COBRAData` object with five slots:

- pval:

  data frame with p-values for 2,399 genes, from three different
  methods.

- padj:

  data frame with adjusted p-values for 2,399 genes, from two different
  methods.

- sval:

  empty data frame

- score:

  data frame with estimated absolute log fold changes for 2,399 genes,
  from three different methods.

- truth:

  data frame with true differential expression status (status column),
  the number of isoforms (n_isoforms column), the true absolute log fold
  change (logFC and logFC_cat columns) and average expression level
  (expr and expr_cat columns) for 3,858 genes.

## Value

A `COBRAData` object.
