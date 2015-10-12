methods::setClassUnion("list_df", c("list", "data.frame"))

#' @rdname IBRAPerformance
#' @export
.IBRAPerformance <- setClass("IBRAPerformance",
                             slots = c(fdrtpr = "data.frame",
                                       fdrtprcurve = "data.frame",
                                       fdrnbr = "data.frame",
                                       fdrnbrcurve = "data.frame",
                                       deviation = "data.frame",
                                       tpr = "data.frame", fpr = "data.frame",
                                       roc = "data.frame", scatter = "data.frame",
                                       fpc = "data.frame", overlap = "list_df",
                                       corr = "data.frame", maxsplit = "numeric",
                                       splv = "character"))

#' \code{IBRAPerformance} object and constructor
#'
#' The \code{IBRAPerformance} class holds various types of calculated
#' performance measures. Objects from this class are typically generated from
#' \code{IBRAData} object by means of the function
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
#' @param object_to_extend An \code{IBRAPerformance} object to extend with the
#'   provided information.
#'
#' @include IBRAData.R
#'
#' @aliases IBRAPerformance IBRAPerformance-class
#'
#' @docType class
#'
#' @export
#' @rdname IBRAPerformance
#' @author Charlotte Soneson
#' @import methods
IBRAPerformance <- function(fdrtpr = data.frame(), fdrtprcurve = data.frame(),
                            fdrnbr = data.frame(), fdrnbrcurve = data.frame(),
                            tpr = data.frame(), fpr = data.frame(), splv = "",
                            roc = data.frame(), fpc = data.frame(),
                            deviation = data.frame(),
                            overlap = data.frame(), maxsplit = 3,
                            corr = data.frame(), scatter = data.frame(),
                            object_to_extend = NULL) {
  ## TODO: add some checks of the input arguments

  if (!(is.null(object_to_extend))) {
    if (!(class(object_to_extend) == "IBRAPerformance")) {
      stop("object_to_extend must be a IBRAPerformance object")
    } else {
      if (length(object_to_extend@fdrtpr) != 0)
        fdrtpr <- object_to_extend@fdrtpr
      if (length(object_to_extend@fdrtprcurve) != 0)
        fdrtprcurve <- object_to_extend@fdrtprcurve
      if (length(object_to_extend@fdrnbr) != 0)
        fdrnbr <- object_to_extend@fdrnbr
      if (length(object_to_extend@fdrnbrcurve) != 0)
        fdrnbrcurve <- object_to_extend@fdrnbrcurve
      if (length(object_to_extend@tpr) != 0)
        tpr <- object_to_extend@tpr
      if (length(object_to_extend@fpr) != 0)
        fpr <- object_to_extend@fpr
      if (length(object_to_extend@roc) != 0)
        roc <- object_to_extend@roc
      if (length(object_to_extend@fpc) != 0)
        fpc <- object_to_extend@fpc
      if (length(object_to_extend@corr) != 0)
        corr <- object_to_extend@corr
      if (length(object_to_extend@deviation) != 0)
        deviation <- object_to_extend@deviation
      if (length(object_to_extend@scatter) != 0)
        scatter <- object_to_extend@scatter
      if (length(object_to_extend@overlap) != 0)
        overlap <- object_to_extend@overlap
      if (!is.null(object_to_extend@maxsplit))
        maxsplit <- object_to_extend@maxsplit
      if ((object_to_extend@splv) != "")
        splv <- object_to_extend@splv
    }
  }
  .IBRAPerformance(fdrtpr = fdrtpr, fdrtprcurve = fdrtprcurve,
                   deviation = deviation,
                   fdrnbr = fdrnbr, fdrnbrcurve = fdrnbrcurve, scatter = scatter,
                   tpr = tpr, fpr = fpr, roc = roc, fpc = fpc, corr = corr,
                   overlap = overlap, splv = splv, maxsplit = maxsplit)
}

setMethod("show", "IBRAPerformance", function(object) {
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
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name fdrtpr
#' @rdname fdrtpr
#' @aliases fdrtpr fdrtpr,IBRAPerformance-method
#'   fdrtpr<-,IBRAPerformance,data.frame-method fdrtpr,IBRAPlot-method
#'   fdrtpr<-,IBRAPlot,data.frame-method
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information about the observed FPR and TPR
#'   for each method and each stratification level, at various adjusted p-value
#'   thresholds.
#' @author Charlotte Soneson
#' @export
setMethod("fdrtpr", signature(x = "IBRAPerformance"), function(x) x@fdrtpr)

#' @name fdrtpr
#' @rdname fdrtpr
#' @exportMethod "fdrtpr<-"
setReplaceMethod("fdrtpr", signature(x = "IBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@fdrtpr <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor and replacement functions for \code{fdrtprcurve} slot
#'
#' Accessor and replacement functions for the \code{fdrtprcurve} slot in an
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name fdrtprcurve
#' @rdname fdrtprcurve
#' @aliases fdrtprcurve fdrtprcurve,IBRAPerformance-method
#'   fdrtprcurve<-,IBRAPerformance,data.frame-method fdrtprcurve,IBRAPlot-method
#'   fdrtprcurve<-,IBRAPlot,data.frame-method
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information necessary to generate curves of
#'   observed FDR vs TPR for each method and each stratification level.
#' @author Charlotte Soneson
#' @export
setMethod("fdrtprcurve", "IBRAPerformance", function(x) x@fdrtprcurve)
#' @name fdrtprcurve
#' @rdname fdrtprcurve
#' @exportMethod "fdrtprcurve<-"
setReplaceMethod("fdrtprcurve", signature(x = "IBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@fdrtprcurve <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor and replacement functions for \code{deviation} slot
#'
#' Accessor and replacement functions for the \code{deviation} slot in an
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name deviation
#' @rdname deviation
#' @aliases deviation deviation,IBRAPerformance-method
#'   deviation<-,IBRAPerformance,data.frame-method deviation,IBRAPlot-method
#'   deviation<-,IBRAPlot,data.frame-method
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information necessary to plots of deviations
#'   between observed and true scores for each method and each stratification
#'   level.
#' @author Charlotte Soneson
#' @export
setMethod("deviation", "IBRAPerformance", function(x) x@deviation)
#' @name deviation
#' @rdname deviation
#' @exportMethod "deviation<-"
setReplaceMethod("deviation", signature(x = "IBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@deviation <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor and replacement functions for \code{fdrnbr} slot
#'
#' Accessor and replacement functions for the \code{fdrnbr} slot in an
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name fdrnbr
#' @rdname fdrnbr
#' @aliases fdrnbr fdrnbr,IBRAPerformance-method
#'   fdrnbr<-,IBRAPerformance,data.frame-method fdrnbr,IBRAPlot-method
#'   fdrnbr<-,IBRAPlot,data.frame-method
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information about the observed FPR and the
#'   number of features called positive for each method and each stratification
#'   level, at various adjusted p-value thresholds.
#' @author Charlotte Soneson
#' @export
setMethod("fdrnbr", "IBRAPerformance", function(x) x@fdrnbr)
#' @name fdrnbr
#' @rdname fdrnbr
#' @exportMethod "fdrnbr<-"
setReplaceMethod("fdrnbr", signature(x = "IBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@fdrnbr <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor and replacement functions for \code{fdrnbrcurve} slot
#'
#' Accessor and replacement functions for the \code{fdrnbrcurve} slot in an
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name fdrnbrcurve
#' @rdname fdrnbrcurve
#' @aliases fdrnbrcurve fdrnbrcurve,IBRAPerformance-method
#'   fdrnbrcurve<-,IBRAPerformance,data.frame-method fdrnbrcurve,IBRAPlot-method
#'   fdrnbrcurve<-,IBRAPlot,data.frame-method
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information necessary to generate curves of
#'   observed FDR vs number of features called positive for each method and each
#'   stratification level.
#' @author Charlotte Soneson
#' @export
setMethod("fdrnbrcurve", "IBRAPerformance", function(x) x@fdrnbrcurve)
#' @name fdrnbrcurve
#' @rdname fdrnbrcurve
#' @exportMethod "fdrnbrcurve<-"
setReplaceMethod("fdrnbrcurve", signature(x = "IBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@fdrnbrcurve <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor and replacement functions for \code{scatter} slot
#'
#' Accessor and replacement functions for the \code{scatter} slot in an
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name scatter
#' @rdname scatter
#' @aliases scatter scatter,IBRAPerformance-method
#'   scatter<-,IBRAPerformance,data.frame-method scatter,IBRAPlot-method
#'   scatter<-,IBRAPlot,data.frame-method
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information necessary to generate scatter
#'   plots of observed vs true values for each method and each stratification
#'   level.
#' @author Charlotte Soneson
#' @export
setMethod("scatter", "IBRAPerformance", function(x) x@scatter)
#' @name scatter
#' @rdname scatter
#' @exportMethod "scatter<-"
setReplaceMethod("scatter", signature(x = "IBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@scatter <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor and replacement functions for \code{tpr} slot
#'
#' Accessor and replacement functions for the \code{tpr} slot in an
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name tpr
#' @rdname tpr
#' @aliases tpr tpr,IBRAPerformance-method
#'   tpr<-,IBRAPerformance,data.frame-method tpr,IBRAPlot-method
#'   tpr<-,IBRAPlot,data.frame-method
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information about the observed TPR for each
#'   method and each stratification level, at various adjusted p-value
#'   thresholds.
#' @author Charlotte Soneson
#' @export
setMethod("tpr", "IBRAPerformance", function(x) x@tpr)
#' @name tpr
#' @rdname tpr
#' @exportMethod "tpr<-"
setReplaceMethod("tpr", signature(x = "IBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@tpr <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor and replacement functions for \code{fpr} slot
#'
#' Accessor and replacement functions for the \code{fpr} slot in an
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name fpr
#' @rdname fpr
#' @aliases fpr fpr,IBRAPerformance-method
#'   fpr<-,IBRAPerformance,data.frame-method fpr,IBRAPlot-method
#'   fpr<-,IBRAPlot,data.frame-method
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information about the observed FPR for each
#'   method and each stratification level, at various adjusted p-value
#'   thresholds.
#' @author Charlotte Soneson
#' @export
setMethod("fpr", "IBRAPerformance", function(x) x@fpr)
#' @name fpr
#' @rdname fpr
#' @exportMethod fpr
setReplaceMethod("fpr", signature(x = "IBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@fpr <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor and replacement functions for \code{roc} slot
#'
#' Accessor and replacement functions for the \code{roc} slot in an
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name roc
#' @rdname roc
#' @aliases roc roc,IBRAPerformance-method
#'   roc<-,IBRAPerformance,data.frame-method roc,IBRAPlot-method
#'   roc<-,IBRAPlot,data.frame-method
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information necessary to generate ROC curves
#'   for each method and each stratification level.
#' @author Charlotte Soneson
#' @export
setMethod("roc", "IBRAPerformance", function(x) x@roc)
#' @name roc
#' @rdname roc
#' @exportMethod "roc<-"
setReplaceMethod("roc", signature(x = "IBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@roc <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor and replacement functions for \code{fpc} slot
#'
#' Accessor and replacement functions for the \code{fpc} slot in an
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name fpc
#' @rdname fpc
#' @aliases fpc fpc,IBRAPerformance-method
#'   fpc<-,IBRAPerformance,data.frame-method fpc,IBRAPlot-method
#'   fpc<-,IBRAPlot,data.frame-method
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving information necessary to generate false
#'   positive curves for each method and each stratification level.
#' @author Charlotte Soneson
#' @export
setMethod("fpc", "IBRAPerformance", function(x) x@fpc)
#' @name fpc
#' @rdname fpc
#' @exportMethod "fpc<-"
setReplaceMethod("fpc", signature(x = "IBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@fpc <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor and replacement functions for \code{corr} slot
#'
#' Accessor and replacement functions for the \code{corr} slot in an
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name corr
#' @rdname corr
#' @aliases corr corr,IBRAPerformance-method
#'   corr<-,IBRAPerformance,data.frame-method
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame giving correlation values for each method and each
#'   stratification level.
#' @author Charlotte Soneson
#' @export
setMethod("corr", "IBRAPerformance", function(x) x@corr)
#' @name corr
#' @rdname corr
#' @exportMethod "corr<-"
setReplaceMethod("corr", signature(x = "IBRAPerformance", value = "data.frame"),
                 function(x, value) {
                   x@corr <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor and replacement functions for \code{overlap} slot
#'
#' Accessor and replacement functions for the \code{overlap} slot in an
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name overlap
#' @rdname overlap
#' @aliases overlap overlap,IBRAPerformance-method
#'   overlap<-,IBRAPerformance,list_df-method overlap,IBRAPlot-method
#'   overlap<-,IBRAPlot,list_df-method
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A data frame or a list, giving information about which feature
#'   that are classified as 'positive' by each method and for each
#'   stratification level.
#' @author Charlotte Soneson
#' @export
setMethod("overlap", "IBRAPerformance", function(x) x@overlap)
#' @name overlap
#' @rdname overlap
#' @exportMethod "overlap<-"
setReplaceMethod("overlap", signature(x = "IBRAPerformance", value = "list_df"),
                 function(x, value) {
                   x@overlap <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor and replacement functions for \code{splv} slot
#'
#' Accessor and replacement functions for the \code{splv} slot in an
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name splv
#' @rdname splv
#' @aliases splv splv,IBRAPerformance-method
#'   splv<-,IBRAPerformance,character-method splv,IBRAPlot-method
#'   splv<-,IBRAPlot,character-method
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A character string giving the name of a feature annotation to
#'   use for stratification.
#' @author Charlotte Soneson
#' @export
setMethod("splv", "IBRAPerformance", function(x) x@splv)
#' @name splv
#' @rdname splv
#' @exportMethod "splv<-"
setReplaceMethod("splv", signature(x = "IBRAPerformance", value = "character"),
                 function(x, value) {
                   x@splv <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor and replacement functions for \code{maxsplit} slot
#'
#' Accessor and replacement functions for the \code{maxsplit} slot in an
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name maxsplit
#' @rdname maxsplit
#' @aliases maxsplit maxsplit,IBRAPerformance-method
#'   maxsplit<-,IBRAPerformance,numeric-method maxsplit,IBRAPlot-method
#'   maxsplit<-,IBRAPlot,numeric-method
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A numeric value giving the maximal number of strata to retain.
#' @author Charlotte Soneson
#' @export
setMethod("maxsplit", "IBRAPerformance", function(x) x@maxsplit)
#' @name maxsplit
#' @rdname maxsplit
#' @exportMethod "maxsplit<-"
setReplaceMethod("maxsplit", signature(x = "IBRAPerformance", value = "numeric"),
                 function(x, value) {
                   x@maxsplit <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor function for basemethods
#'
#' Accessor function to extract the methods that are represented in an
#' \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name basemethods
#' @rdname basemethods
#' @aliases basemethods basemethods,IBRAPerformance-method
#'   basemethods,IBRAPlot-method
#' @return A character vector of all methods represented in the object.
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @author Charlotte Soneson
#' @export
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100), row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)), row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#' ibraperf <- calculate_performance(ibradata, binary_truth = "status",
#'                                   aspects = "fpr", thrs = c(0.05, 0.1))
#' basemethods(ibraperf)
setMethod("basemethods", "IBRAPerformance", function(x) {
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
#' in an \code{IBRAPerformance} or \code{IBRAPlot} object.
#'
#' @docType methods
#' @name stratiflevels
#' @rdname stratiflevels
#' @aliases stratiflevels stratiflevels,IBRAPerformance-method
#'   stratiflevels,IBRAPlot-method
#' @return A character vector of all stratification levels represented in the
#'   object
#'
#' @param x An \code{IBRAPerformance} or \code{IBRAPlot} object
#' @param ... Additional arguments
#' @author Charlotte Soneson
#' @export
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100), row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)), row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#' ibraperf <- calculate_performance(ibradata, binary_truth = "status",
#'                                   aspects = "fpr", thrs = c(0.05, 0.1))
#' stratiflevels(ibraperf)
setMethod("stratiflevels", "IBRAPerformance", function(x) {
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
#' @aliases [ [,IBRAPerformance-method \S4method{[}{IBRAPerformance,ANY,ANY}
#' @export
setMethod("[", "IBRAPerformance",
          function(x, i = "missing", j, drop = "missing") {
            if (length(intersect(j, basemethods(x))) == 0)
              stop("None of the provided method found in the object. No subsetting done.")
            if (length(x@tpr) != 0)
              x@tpr <- x@tpr[which(x@tpr$basemethod %in% j), ]
            if (length(x@fpr) != 0)
              x@fpr <- x@fpr[which(x@fpr$basemethod %in% j), ]
            if (length(x@roc) != 0)
              x@roc <- x@roc[which(x@roc$basemethod %in% j), ]
            if (length(x@fpc) != 0)
              x@fpc <- x@fpc[which(x@fpc$basemethod %in% j), ]
            if (length(x@fdrtpr) != 0)
              x@fdrtpr <- x@fdrtpr[which(x@fdrtpr$basemethod %in% j), ]
            if (length(x@fdrnbr) != 0)
              x@fdrnbr <- x@fdrnbr[which(x@fdrnbr$basemethod %in% j), ]
            if (length(x@deviation) != 0)
              x@deviation <- x@deviation[which(x@deviation$basemethod %in% j), ]
            if (length(x@fdrtprcurve) != 0)
              x@fdrtprcurve <-
                x@fdrtprcurve[which(x@fdrtprcurve$basemethod %in% j), ]
            if (length(x@fdrnbrcurve) != 0)
              x@fdrnbrcurve <-
                x@fdrnbrcurve[which(x@fdrnbrcurve$basemethod %in% j), ]
            if (length(x@corr) != 0)
              x@corr <- x@corr[which(x@corr$basemethod %in% j), ]
            if (length(x@scatter) != 0)
              x@scatter <- x@scatter[which(x@scatter$basemethod %in% j), ]
            if (length(x@overlap) != 0) {
              if (class(x@overlap) == "list") {
                x@overlap <-
                  lapply(x@overlap, function(w) {
                      w[, which(colnames(w) %in% c(j, "truth")), drop = FALSE]
                  })
              } else {
                  x@overlap <-
                    x@overlap[, which(colnames(x@overlap) %in% c(j, "truth")),
                              drop = FALSE]
              }
            }
            x
          })

## Validity
setValidity("IBRAPerformance",
            function(object) {
              msg <- NULL
              valid <- TRUE
              if (valid) TRUE else msg
            })
