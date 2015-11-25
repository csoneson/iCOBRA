methods::setClassUnion("list_df", c("list", "data.frame"))

#' @rdname COBRAPerformance
#' @export
.COBRAPerformance <- setClass("COBRAPerformance",
                             slots = c(fdrtpr = "data.frame",
                                       fdrtprcurve = "data.frame",
                                       fdrnbr = "data.frame",
                                       fdrnbrcurve = "data.frame",
                                       deviation = "data.frame",
                                       tpr = "data.frame", fpr = "data.frame",
                                       roc = "data.frame",
                                       scatter = "data.frame",
                                       fpc = "data.frame", overlap = "list_df",
                                       corr = "data.frame",
                                       maxsplit = "numeric",
                                       splv = "character",
                                       onlyshared = "logical"))

#' \code{COBRAPerformance} object and constructor
#'
#' The \code{COBRAPerformance} class holds various types of calculated
#' performance measures. Objects from this class are typically generated from
#' \code{COBRAData} objects by means of the function
#' \code{\link{calculate_performance}}.
#'
#' @param fdrtpr A data frame containing observed FDR and TPR values at various
#'   adjusted p-value thresholds.
#' @param fdrtprcurve A data frame containing observed FDR and TPR values for a
#'   (potentially large) number of cutoffs applied to a 'score' (that can be
#'   p-value, adjusted p-value or a more general score).
#' @param fdrnbr A data frame containing observed FDR and the number of features
#'   considered to be significant at various adjusted p-value thresholds.
#' @param fdrnbrcurve A data frame containing observed FDR and number of
#'   features considered to be significant for a (potentially large) number of
#'   cutoffs applied to a 'score' (that can be p-value, adjusted p-value or a
#'   more general score).
#' @param tpr A data frame containing observed TPR values at various adjusted
#'   p-value thresholds.
#' @param fpr A data frame containing observed FPR values at various adjusted
#'   p-value thresholds.
#' @param roc A data frame containing observed FPR and TPR values  for a
#'   (potentially large) number of cutoffs applied to a 'score' (that can be
#'   p-value, adjusted p-value or a more general score), which can be used to
#'   generate a ROC curve.
#' @param scatter A data frame containing observed 'scores' (p-values, adjusted
#'   p-values or more general scores) and true scores, which can be used to
#'   generate scatter plots.
#' @param fpc A data frame containing observed numbers of false positive
#'   findings among the N top-ranked features (ranked by p-values, adjusted
#'   p-values or more general scores), for a (potentially large) number of Ns,
#'   which can be used to generate a false positive curve.
#' @param overlap A data frame or list of data frames with binary values
#'   indicating, for each of a number of methods and number of features, whether
#'   the method consider the feature 'positive' (significant, 1) or 'negative'
#'   (non-significant, 0). If it is a list of data frames, each list element
#'   corresponds to one level of a stratifying factor.
#' @param corr A data frame containing observed (Pearson and Spearman)
#'   correlation values between observed and true scores.
#' @param deviation A data frame containing deviations between observed scores
#'   and true scores.
#' @param maxsplit A numeric value indicating the largest number of levels to
#'   retain if the results have been stratified by an annotation.
#' @param splv A character string giving the name of the stratification factor,
#'   "none" if the results are not stratified.
#' @param onlyshared A logical value indicating whether only features shared
#'   between the results and the truth should be retained, or if all features
#'   present in the truth should be used.
#'
#' @include COBRAData.R
#' @return An \code{COBRAPerformance} object.
#'
#' @aliases COBRAPerformance COBRAPerformance-class
#'
#' @docType class
#'
#' @export
#' @rdname COBRAPerformance
#' @author Charlotte Soneson
#' @import methods
#' @examples
#' ## Empty COBRAPerformance object
#' COBRAPerformance()
COBRAPerformance <- function(fdrtpr = data.frame(), fdrtprcurve = data.frame(),
                            fdrnbr = data.frame(), fdrnbrcurve = data.frame(),
                            tpr = data.frame(), fpr = data.frame(), splv = "",
                            roc = data.frame(), fpc = data.frame(),
                            deviation = data.frame(), onlyshared = NA,
                            overlap = data.frame(), maxsplit = NA_integer_,
                            corr = data.frame(), scatter = data.frame()) {
  ## TODO: add some checks of the input arguments

  .COBRAPerformance(fdrtpr = fdrtpr, fdrtprcurve = fdrtprcurve,
                   deviation = deviation, fdrnbr = fdrnbr,
                   fdrnbrcurve = fdrnbrcurve, scatter = scatter,
                   tpr = tpr, fpr = fpr, roc = roc, fpc = fpc, corr = corr,
                   overlap = overlap, splv = splv, maxsplit = maxsplit,
                   onlyshared = onlyshared)
}

setMethod("show", "COBRAPerformance", function(object) {
  cat("An object of class \"", class(object), "\"\n", sep = "")
  for (sl in slotNames(object)) {
    x <- slot(object, sl)
    cat("@", sl, "\n", sep = "")
    .printHead(x)
    cat("\n")
  }
})

#' Accessor and replacement functions for \code{fdrtpr} slot
#'
#' Accessor and replacement functions for the \code{fdrtpr} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name fdrtpr
#' @rdname fdrtpr
#' @aliases fdrtpr fdrtpr,COBRAPerformance-method
#'   fdrtpr<-,COBRAPerformance,data.frame-method fdrtpr,COBRAPlot-method
#'   fdrtpr<-,COBRAPlot,data.frame-method
#' @return The accessor function returns a data frame giving information about
#'   the observed FPR and TPR for each method and each stratification level, at
#'   various adjusted p-value thresholds.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information about the observed FPR and TPR
#'   for each method and each stratification level, at various adjusted p-value
#'   thresholds.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "fdrtpr")
#' head(fdrtpr(cobraperf))
setMethod("fdrtpr", signature(x = "COBRAPerformance"), function(x) x@fdrtpr)
#' @name fdrtpr
#' @rdname fdrtpr
#' @exportMethod "fdrtpr<-"
setReplaceMethod("fdrtpr", signature(x = "COBRAPerformance",
                                     value = "data.frame"),
                 function(x, value) {
                   x@fdrtpr <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{onlyshared} slot
#'
#' Accessor and replacement functions for the \code{onlyshared} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name onlyshared
#' @rdname onlyshared
#' @aliases onlyshared onlyshared,COBRAPerformance-method
#'   onlyshared<-,COBRAPerformance,logical-method onlyshared,COBRAPlot-method
#'   onlyshared<-,COBRAPlot,logical-method
#' @return The accessor function returns a logical indicating whether only
#'   features that are shared between result and truth are retained, or if all
#'   features in the truth are used.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A logical indicating whether only features that are shared
#'   between result and truth are retained, or if all features in the truth are
#'   used.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "fdrtpr")
#' head(onlyshared(cobraperf))
setMethod("onlyshared", signature(x = "COBRAPerformance"),
          function(x) x@onlyshared)
#' @name onlyshared
#' @rdname onlyshared
#' @exportMethod "onlyshared<-"
setReplaceMethod("onlyshared", signature(x = "COBRAPerformance",
                                         value = "logical"),
                 function(x, value) {
                   x@onlyshared <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{fdrtprcurve} slot
#'
#' Accessor and replacement functions for the \code{fdrtprcurve} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name fdrtprcurve
#' @rdname fdrtprcurve
#' @aliases fdrtprcurve fdrtprcurve,COBRAPerformance-method
#'   fdrtprcurve<-,COBRAPerformance,data.frame-method
#'   fdrtprcurve,COBRAPlot-method fdrtprcurve<-,COBRAPlot,data.frame-method
#' @return The accessor function returns a data frame giving information
#'   necessary to generate curves of observed FDR vs TPR for each method and
#'   each stratification level.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information necessary to generate curves of
#'   observed FDR vs TPR for each method and each stratification level.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "fdrtprcurve")
#' head(fdrtprcurve(cobraperf))
setMethod("fdrtprcurve", "COBRAPerformance", function(x) x@fdrtprcurve)
#' @name fdrtprcurve
#' @rdname fdrtprcurve
#' @exportMethod "fdrtprcurve<-"
setReplaceMethod("fdrtprcurve", signature(x = "COBRAPerformance",
                                          value = "data.frame"),
                 function(x, value) {
                   x@fdrtprcurve <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{deviation} slot
#'
#' Accessor and replacement functions for the \code{deviation} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name deviation
#' @rdname deviation
#' @aliases deviation deviation,COBRAPerformance-method
#'   deviation<-,COBRAPerformance,data.frame-method deviation,COBRAPlot-method
#'   deviation<-,COBRAPlot,data.frame-method
#' @return The accessor function returns a data frame giving information
#'   necessary to plots of deviations between observed and true scores for each
#'   method and each stratification level.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information necessary to plots of deviations
#'   between observed and true scores for each method and each stratification
#'   level.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example, cont_truth = "logFC",
#'                                    aspects = "deviation")
#' head(deviation(cobraperf))
setMethod("deviation", "COBRAPerformance", function(x) x@deviation)
#' @name deviation
#' @rdname deviation
#' @exportMethod "deviation<-"
setReplaceMethod("deviation", signature(x = "COBRAPerformance",
                                        value = "data.frame"),
                 function(x, value) {
                   x@deviation <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{fdrnbr} slot
#'
#' Accessor and replacement functions for the \code{fdrnbr} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name fdrnbr
#' @rdname fdrnbr
#' @aliases fdrnbr fdrnbr,COBRAPerformance-method
#'   fdrnbr<-,COBRAPerformance,data.frame-method fdrnbr,COBRAPlot-method
#'   fdrnbr<-,COBRAPlot,data.frame-method
#' @return The accessor function returns a data frame giving information about
#'   the observed FPR and the number of features called positive for each method
#'   and each stratification level, at various adjusted p-value thresholds.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information about the observed FPR and the
#'   number of features called positive for each method and each stratification
#'   level, at various adjusted p-value thresholds.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "fdrnbr")
#' head(fdrnbr(cobraperf))
setMethod("fdrnbr", "COBRAPerformance", function(x) x@fdrnbr)
#' @name fdrnbr
#' @rdname fdrnbr
#' @exportMethod "fdrnbr<-"
setReplaceMethod("fdrnbr", signature(x = "COBRAPerformance",
                                     value = "data.frame"),
                 function(x, value) {
                   x@fdrnbr <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{fdrnbrcurve} slot
#'
#' Accessor and replacement functions for the \code{fdrnbrcurve} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name fdrnbrcurve
#' @rdname fdrnbrcurve
#' @aliases fdrnbrcurve fdrnbrcurve,COBRAPerformance-method
#'   fdrnbrcurve<-,COBRAPerformance,data.frame-method
#'   fdrnbrcurve,COBRAPlot-method fdrnbrcurve<-,COBRAPlot,data.frame-method
#' @return The accessor function returns a data frame giving information
#'   necessary to generate curves of observed FDR vs number of features called
#'   positive for each method and each stratification level.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information necessary to generate curves of
#'   observed FDR vs number of features called positive for each method and each
#'   stratification level.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "fdrnbrcurve")
#' head(fdrnbrcurve(cobraperf))
setMethod("fdrnbrcurve", "COBRAPerformance", function(x) x@fdrnbrcurve)
#' @name fdrnbrcurve
#' @rdname fdrnbrcurve
#' @exportMethod "fdrnbrcurve<-"
setReplaceMethod("fdrnbrcurve", signature(x = "COBRAPerformance",
                                          value = "data.frame"),
                 function(x, value) {
                   x@fdrnbrcurve <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{scatter} slot
#'
#' Accessor and replacement functions for the \code{scatter} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name scatter
#' @rdname scatter
#' @aliases scatter scatter,COBRAPerformance-method
#'   scatter<-,COBRAPerformance,data.frame-method scatter,COBRAPlot-method
#'   scatter<-,COBRAPlot,data.frame-method
#' @return The accessor function returns a data frame giving information
#'   necessary to generate scatter plots of observed vs true values for each
#'   method and each stratification level.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information necessary to generate scatter
#'   plots of observed vs true values for each method and each stratification
#'   level.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example, cont_truth = "logFC",
#'                                    aspects = "scatter")
#' head(scatter(cobraperf))
setMethod("scatter", "COBRAPerformance", function(x) x@scatter)
#' @name scatter
#' @rdname scatter
#' @exportMethod "scatter<-"
setReplaceMethod("scatter", signature(x = "COBRAPerformance",
                                      value = "data.frame"),
                 function(x, value) {
                   x@scatter <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{tpr} slot
#'
#' Accessor and replacement functions for the \code{tpr} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name tpr
#' @rdname tpr
#' @aliases tpr tpr,COBRAPerformance-method
#'   tpr<-,COBRAPerformance,data.frame-method tpr,COBRAPlot-method
#'   tpr<-,COBRAPlot,data.frame-method
#' @return The accessor function returns a data frame giving information about
#'   the observed TPR for each method and each stratification level, at various
#'   adjusted p-value thresholds.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information about the observed TPR for each
#'   method and each stratification level, at various adjusted p-value
#'   thresholds.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status", aspects = "tpr")
#' head(tpr(cobraperf))
setMethod("tpr", "COBRAPerformance", function(x) x@tpr)
#' @name tpr
#' @rdname tpr
#' @exportMethod "tpr<-"
setReplaceMethod("tpr", signature(x = "COBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@tpr <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{fpr} slot
#'
#' Accessor and replacement functions for the \code{fpr} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name fpr
#' @rdname fpr
#' @aliases fpr fpr,COBRAPerformance-method
#'   fpr<-,COBRAPerformance,data.frame-method fpr,COBRAPlot-method
#'   fpr<-,COBRAPlot,data.frame-method
#' @return The accessor function returns a data frame giving information about
#'   the observed FPR for each method and each stratification level, at various
#'   adjusted p-value thresholds.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information about the observed FPR for each
#'   method and each stratification level, at various adjusted p-value
#'   thresholds.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status", aspects = "fpr")
#' head(fpr(cobraperf))
setMethod("fpr", "COBRAPerformance", function(x) x@fpr)
#' @name fpr
#' @rdname fpr
#' @exportMethod fpr
setReplaceMethod("fpr", signature(x = "COBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@fpr <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{roc} slot
#'
#' Accessor and replacement functions for the \code{roc} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name roc
#' @rdname roc
#' @aliases roc roc,COBRAPerformance-method
#'   roc<-,COBRAPerformance,data.frame-method roc,COBRAPlot-method
#'   roc<-,COBRAPlot,data.frame-method
#' @return The accessor function returns a data frame giving information
#'   necessary to generate ROC curves for each method and each stratification
#'   level.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information necessary to generate ROC curves
#'   for each method and each stratification level.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status", aspects = "roc")
#' head(roc(cobraperf))
setMethod("roc", "COBRAPerformance", function(x) x@roc)
#' @name roc
#' @rdname roc
#' @exportMethod "roc<-"
setReplaceMethod("roc", signature(x = "COBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@roc <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{fpc} slot
#'
#' Accessor and replacement functions for the \code{fpc} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name fpc
#' @rdname fpc
#' @aliases fpc fpc,COBRAPerformance-method
#'   fpc<-,COBRAPerformance,data.frame-method fpc,COBRAPlot-method
#'   fpc<-,COBRAPlot,data.frame-method
#' @return The accessor function returns a data frame giving information
#'   necessary to generate false positive curves for each method and each
#'   stratification level.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information necessary to generate false
#'   positive curves for each method and each stratification level.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status", aspects = "fpc")
#' head(fpc(cobraperf))
setMethod("fpc", "COBRAPerformance", function(x) x@fpc)
#' @name fpc
#' @rdname fpc
#' @exportMethod "fpc<-"
setReplaceMethod("fpc", signature(x = "COBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@fpc <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{corr} slot
#'
#' Accessor and replacement functions for the \code{corr} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name corr
#' @rdname corr
#' @aliases corr corr,COBRAPerformance-method
#'   corr<-,COBRAPerformance,data.frame-method
#' @return The accessor function returns a data frame giving correlation values
#'   for each method and each stratification level.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving correlation values for each method and each
#'   stratification level.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example, cont_truth = "logFC",
#'                                    aspects = "corr")
#' head(corr(cobraperf))
setMethod("corr", "COBRAPerformance", function(x) x@corr)
#' @name corr
#' @rdname corr
#' @exportMethod "corr<-"
setReplaceMethod("corr", signature(x = "COBRAPerformance",
                                   value = "data.frame"),
                 function(x, value) {
                   x@corr <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{overlap} slot
#'
#' Accessor and replacement functions for the \code{overlap} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name overlap
#' @rdname overlap
#' @aliases overlap overlap,COBRAPerformance-method
#'   overlap<-,COBRAPerformance,list_df-method overlap,COBRAPlot-method
#'   overlap<-,COBRAPlot,list_df-method
#' @return The accessor function returns a data frame or a list, giving
#'   information about which feature that are classified as 'positive' by each
#'   method and for each stratification level.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame or a list, giving information about which feature
#'   that are classified as 'positive' by each method and for each
#'   stratification level.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "overlap")
#' head(overlap(cobraperf))
setMethod("overlap", "COBRAPerformance", function(x) x@overlap)
#' @name overlap
#' @rdname overlap
#' @exportMethod "overlap<-"
setReplaceMethod("overlap", signature(x = "COBRAPerformance",
                                      value = "list_df"),
                 function(x, value) {
                   x@overlap <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{splv} slot
#'
#' Accessor and replacement functions for the \code{splv} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name splv
#' @rdname splv
#' @aliases splv splv,COBRAPerformance-method
#'   splv<-,COBRAPerformance,character-method splv,COBRAPlot-method
#'   splv<-,COBRAPlot,character-method
#' @return The accessor function returns a character string giving the name of a
#'   feature annotation to use for stratification.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A character string giving the name of a feature annotation to
#'   use for stratification.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "fdrtpr", splv = "expr_cat")
#' splv(cobraperf)
setMethod("splv", "COBRAPerformance", function(x) x@splv)
#' @name splv
#' @rdname splv
#' @exportMethod "splv<-"
setReplaceMethod("splv", signature(x = "COBRAPerformance", value = "character"),
                 function(x, value) {
                   x@splv <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{maxsplit} slot
#'
#' Accessor and replacement functions for the \code{maxsplit} slot in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name maxsplit
#' @rdname maxsplit
#' @aliases maxsplit maxsplit,COBRAPerformance-method
#'   maxsplit<-,COBRAPerformance,numeric-method maxsplit,COBRAPlot-method
#'   maxsplit<-,COBRAPlot,numeric-method
#' @return The accessor function returns a numeric value giving the maximal
#'   number of strata to retain.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A numeric value giving the maximal number of strata to retain.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "fdrtpr", splv = "expr_cat",
#'                                    maxsplit = 3)
#' maxsplit(cobraperf)
setMethod("maxsplit", "COBRAPerformance", function(x) x@maxsplit)
#' @name maxsplit
#' @rdname maxsplit
#' @exportMethod "maxsplit<-"
setReplaceMethod("maxsplit", signature(x = "COBRAPerformance",
                                       value = "numeric"),
                 function(x, value) {
                   x@maxsplit <- value
                   if (validObject(x))
                     x
                 })

#' Accessor function for basemethods
#'
#' Accessor function to extract the methods that are represented in an
#' \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name basemethods
#' @rdname basemethods
#' @aliases basemethods basemethods,COBRAPerformance-method
#'   basemethods,COBRAPlot-method
#' @return A character vector of all methods represented in the object.
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "fdrtprcurve")
#' basemethods(cobraperf)
setMethod("basemethods", "COBRAPerformance", function(x) {
  x1 <- unlist(lapply(slotNames(x), function(w) {
    tryCatch(slot(x, w)$basemethod,
             error = function(e) NULL)
  }))
  if (class(x@overlap) == "list")
    x2 <- unlist(lapply(x@overlap, function(w) colnames(w)))
  else
    x2 <- colnames(x@overlap)
  unique(c(x1, x2))
})

#' Accessor function for stratification levels
#'
#' Accessor function to extract the stratification levels that are represented
#' in an \code{COBRAPerformance} or \code{COBRAPlot} object.
#'
#' @docType methods
#' @name stratiflevels
#' @rdname stratiflevels
#' @aliases stratiflevels stratiflevels,COBRAPerformance-method
#'   stratiflevels,COBRAPlot-method
#' @return A character vector of all stratification levels represented in the
#'   object
#'
#' @param x An \code{COBRAPerformance} or \code{COBRAPlot} object
#' @param ... Additional arguments
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "fdrtpr", splv = "expr_cat",
#'                                    maxsplit = 4)
#' stratiflevels(cobraperf)
setMethod("stratiflevels", "COBRAPerformance", function(x) {
  x1 <- unlist(lapply(slotNames(x), function(w) {
    tryCatch(as.character(slot(x, w)$splitval),
             error = function(e) NULL)
  }))
  if (class(x@overlap) == "list")
    x2 <- paste0(x@splv, ":", names(x@overlap))
  else
    x2 <- "overall"
  unique(c(x1, x2))
})

#' @docType methods
#' @name Extract
#' @rdname Extract
#' @aliases [ [,COBRAPerformance-method \S4method{[}{COBRAPerformance,ANY,ANY}
#' @return A subset of the original object, of the same class
#' @export
setMethod("[", "COBRAPerformance",
          function(x, i = "missing", j, drop = "missing") {
            if (length(intersect(j, basemethods(x))) == 0)
              stop("none of the provided method found in the object, ",
                   "no subsetting done")
            if (length(x@tpr) != 0 && length(intersect(j,x@tpr$basemethod)) > 0)
              x@tpr <- x@tpr[which(x@tpr$basemethod %in% j), ]
            else
              x@tpr <- data.frame()

            if (length(x@fpr) != 0 && length(intersect(j,x@fpr$basemethod)) > 0)
              x@fpr <- x@fpr[which(x@fpr$basemethod %in% j), ]
            else
              x@fpr <- data.frame()

            if (length(x@roc) != 0 && length(intersect(j,x@roc$basemethod)) > 0)
              x@roc <- x@roc[which(x@roc$basemethod %in% j), ]
            else
              x@roc <- data.frame()

            if (length(x@fpc) != 0 && length(intersect(j,x@fpc$basemethod)) > 0)
              x@fpc <- x@fpc[which(x@fpc$basemethod %in% j), ]
            else
              x@fpc <- data.frame()

            if (length(x@fdrtpr) != 0 &&
                length(intersect(j, x@fdrtpr$basemethod)) > 0)
              x@fdrtpr <- x@fdrtpr[which(x@fdrtpr$basemethod %in% j), ]
            else
              x@fdrtpr <- data.frame()

            if (length(x@fdrnbr) != 0 &&
                length(intersect(j, x@fdrnbr$basemethod)) > 0)
              x@fdrnbr <- x@fdrnbr[which(x@fdrnbr$basemethod %in% j), ]
            else
              x@fdrnbr <- data.frame()

            if (length(x@deviation) != 0 &&
                length(intersect(j, x@deviation$basemethod)) > 0)
              x@deviation <- x@deviation[which(x@deviation$basemethod %in% j), ]
            else
              x@deviation <- data.frame()

            if (length(x@fdrtprcurve) != 0 &&
                length(intersect(j, x@fdrtprcurve$basemethod)) > 0)
              x@fdrtprcurve <-
                x@fdrtprcurve[which(x@fdrtprcurve$basemethod %in% j), ]
            else
              x@fdrtprcurve <- data.frame()

            if (length(x@fdrnbrcurve) != 0 &&
                length(intersect(j,x@fdrnbrcurve$basemethod)) > 0)
              x@fdrnbrcurve <-
                x@fdrnbrcurve[which(x@fdrnbrcurve$basemethod %in% j), ]
            else
              x@fdrnbrcurve <- data.frame()

            if (length(x@corr) != 0 &&
                length(intersect(j, x@corr$basemethod)) > 0)
              x@corr <- x@corr[which(x@corr$basemethod %in% j), ]
            else
              x@corr <- data.frame()

            if (length(x@scatter) != 0 &&
                length(intersect(j, x@scatter$basemethod)) > 0)
              x@scatter <- x@scatter[which(x@scatter$basemethod %in% j), ]
            else
              x@scatter <- data.frame()

            if (length(x@overlap) != 0) {
              if (class(x@overlap) == "data.frame") {
                if (length(intersect(c(j, "truth"), colnames(x@overlap))) > 0) {
                  x@overlap <-
                    x@overlap[, which(colnames(x@overlap) %in% c(j, "truth")),
                              drop = FALSE]
                } else {
                  x@overlap <- data.frame()
                }
              } else {
                if (length(intersect(c(j, "truth"),
                                     colnames(x@overlap[[1]]))) > 0) {
                  x@overlap <-
                    lapply(x@overlap, function(w) {
                      w[, which(colnames(w) %in% c(j, "truth")), drop = FALSE]
                    })
                } else {
                  x@overlap <- list()
                }
              }
            }
            x
          })

## Validity
setValidity("COBRAPerformance",
            function(object) {
              msg <- NULL
              valid <- TRUE
              if (valid) TRUE else msg
            })
