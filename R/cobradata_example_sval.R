#' Example data set with three differential gene expression methods
#'
#' A data set consisting of p-values, adjusted p-values, s-values and estimated
#' log fold changes (in the 'scores' slot) obtained by three methods for
#' differential expression analysis of RNA-seq data, applied to a small
#' synthetic data set of 3,858 human genes. The values are stored in a
#' \code{COBRAData} object together with a 'truth' data frame containing the
#' true differential expression status for each gene as well as various
#' additional annotations such as the true log fold change, the number of
#' isoforms of the gene and the average expression level.
#'
#' @format A \code{COBRAData} object with five slots:
#' \describe{
#'   \item{pval}{data frame with p-values for 2,399 genes, from three different
#'   methods.}
#'   \item{padj}{data frame with adjusted p-values for 2,399 genes, from two
#'   different methods.}
#'   \item{sval}{data frame representing s-values for 2,399 genes, from two
#'   different methods. Since the purpose of this data set is only to
#'   demonstrate functionality, the s-values were obtained by copying the
#'   adjusted p-value slot, and thus these values do not represent true
#'   s-values.}
#'   \item{score}{data frame with estimated log fold changes for 2,399 genes,
#'   from three different methods.}
#'   \item{truth}{data frame with true differential expression status
#'   (status column), the number of isoforms (n_isoforms column), the true
#'   log fold change (logFC and logFC_cat columns) and average expression level
#'   (expr and expr_cat columns) for 3,858 genes.}
#'   }
#' @return A \code{COBRAData} object.
"cobradata_example_sval"
