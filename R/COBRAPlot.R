#' @rdname COBRAPlot
#' @export
.COBRAPlot <- setClass("COBRAPlot",
                      slots = c(plotcolors = "character",
                                facetted = "logical"),
                      contains = "COBRAPerformance")

#' \code{COBRAPlot} object and constructor
#'
#' The \code{COBRAPlot} class is similar to the \code{COBRAPerformance} class in
#' that it holds various types of calculated performance measures. However, it
#' also contains other attributes that are necessary for plotting, such as color
#' assignments. Several \code{COBRAPlot} objects can be generated from the same
#' \code{COBRAPerformance} object, without having to go through the potentially
#' time consuming task of recalculating all performance measures. Objects from
#' this class are typically generated from an \code{COBRAPerformance} objects by
#' means of the function \code{\link{prepare_data_for_plot}}.
#'
#' @param plotcolors A character vector giving the color for each method (or
#'   method-stratification level combination).
#' @param facetted A logical indicating whether the data is prepared for a
#'   facetted plot (separating different stratification levels into different
#'   panels) or for displaying all values in one plot panel.
#'
#' @return An \code{COBRAPlot} object.
#' @inheritParams COBRAPerformance
#' @include COBRAPerformance.R
#'
#' @aliases COBRAPlot COBRAPlot-class
#'
#' @docType class
#'
#' @export
#' @rdname COBRAPlot
#' @author Charlotte Soneson
#' @examples
#' ## Empty COBRAPlot object
#' cobraplot <- COBRAPlot()
COBRAPlot <- function(fdrtpr = data.frame(), fdrtprcurve = data.frame(),
                      fdrnbr = data.frame(), corr = data.frame(),
                      fdrnbrcurve = data.frame(), tpr = data.frame(),
                      fpr = data.frame(), roc = data.frame(),
                      scatter = data.frame(), onlyshared = NA,
                      fpc = data.frame(), overlap = data.frame(),
                      plotcolors = "", splv = "", deviation = data.frame(),
                      maxsplit = NA_integer_, facetted = NA) {

  .COBRAPlot(fdrtpr = fdrtpr, fdrtprcurve = fdrtprcurve,
             onlyshared = onlyshared, fdrnbr = fdrnbr,
             fdrnbrcurve = fdrnbrcurve, deviation = deviation,
             tpr = tpr, fpr = fpr, roc = roc, fpc = fpc, scatter = scatter,
             overlap = overlap, plotcolors = plotcolors, corr = corr,
             splv = splv, maxsplit = maxsplit, facetted = facetted)
}

setMethod("show", "COBRAPlot", function(object) {
  cat("An object of class \"", class(object), "\"\n", sep = "")
  for (sl in slotNames(object)) {
    x <- slot(object, sl)
    cat("@", sl, "\n", sep = "")
    .printHead(x)
    cat("\n")
  }
})

#' @rdname fdrtpr
#' @aliases fdrtpr fdrtpr,COBRAPlot-method fdrtpr<-,COBRAPlot,data.frame-method
setReplaceMethod("fdrtpr", signature(x = "COBRAPlot",
                                     value = "data.frame"),
                 function(x, value) {
                   x@fdrtpr <- value
                   if (validObject(x))
                     x
                 })

#' @rdname onlyshared
#' @aliases onlyshared onlyshared,COBRAPlot-method
#'   onlyshared<-,COBRAPlot,logical-method
setReplaceMethod("onlyshared", signature(x = "COBRAPlot",
                                         value = "logical"),
                 function(x, value) {
                   x@onlyshared <- value
                   if (validObject(x))
                     x
                 })

#' @rdname fdrtprcurve
#' @aliases fdrtprcurve fdrtprcurve,COBRAPlot-method
#'   fdrtprcurve<-,COBRAPlot,data.frame-method
setReplaceMethod("fdrtprcurve", signature(x = "COBRAPlot",
                                          value = "data.frame"),
                 function(x, value) {
                   x@fdrtprcurve <- value
                   if (validObject(x))
                     x
                 })

#' @rdname deviation
#' @aliases deviation deviation,COBRAPlot-method
#'   deviation<-,COBRAPlot,data.frame-method
setReplaceMethod("deviation", signature(x = "COBRAPlot",
                                        value = "data.frame"),
                 function(x, value) {
                   x@deviation <- value
                   if (validObject(x))
                     x
                 })

#' @rdname fdrnbr
#' @aliases fdrnbr fdrnbr,COBRAPlot-method fdrnbr<-,COBRAPlot,data.frame-method
setReplaceMethod("fdrnbr", signature(x = "COBRAPlot",
                                     value = "data.frame"),
                 function(x, value) {
                   x@fdrnbr <- value
                   if (validObject(x))
                     x
                 })

#' @rdname fdrnbrcurve
#' @aliases fdrnbrcurve fdrnbrcurve,COBRAPlot-method
#'   fdrnbrcurve<-,COBRAPlot,data.frame-method
setReplaceMethod("fdrnbrcurve", signature(x = "COBRAPlot",
                                          value = "data.frame"),
                 function(x, value) {
                   x@fdrnbrcurve <- value
                   if (validObject(x))
                     x
                 })

#' @rdname scatter
#' @aliases scatter scatter,COBRAPlot-method
#'   scatter<-,COBRAPlot,data.frame-method
setReplaceMethod("scatter", signature(x = "COBRAPlot",
                                      value = "data.frame"),
                 function(x, value) {
                   x@scatter <- value
                   if (validObject(x))
                     x
                 })

#' @rdname tpr
#' @aliases tpr tpr,COBRAPlot-method tpr<-,COBRAPlot,data.frame-method
setReplaceMethod("tpr", signature(x = "COBRAPlot", value = "data.frame"),
                 function(x, value) {
                   x@tpr <- value
                   if (validObject(x))
                     x
                 })

#' @rdname fpr
#' @aliases fpr fpr,COBRAPlot-method fpr<-,COBRAPlot,data.frame-method
setReplaceMethod("fpr", signature(x = "COBRAPlot", value = "data.frame"),
                 function(x, value) {
                   x@fpr <- value
                   if (validObject(x))
                     x
                 })

#' @rdname roc
#' @aliases roc roc,COBRAPlot-method roc<-,COBRAPlot,data.frame-method
setReplaceMethod("roc", signature(x = "COBRAPlot", value = "data.frame"),
                 function(x, value) {
                   x@roc <- value
                   if (validObject(x))
                     x
                 })

#' @rdname fpc
#' @aliases fpc fpc,COBRAPlot-method fpc<-,COBRAPlot,data.frame-method
setReplaceMethod("fpc", signature(x = "COBRAPlot", value = "data.frame"),
                 function(x, value) {
                   x@fpc <- value
                   if (validObject(x))
                     x
                 })

#' @rdname corr
#' @aliases corr corr,COBRAPlot-method corr<-,COBRAPlot,data.frame-method
setReplaceMethod("corr", signature(x = "COBRAPlot",
                                   value = "data.frame"),
                 function(x, value) {
                   x@corr <- value
                   if (validObject(x))
                     x
                 })

#' @rdname overlap
#' @aliases overlap overlap,COBRAPlot-method overlap<-,COBRAPlot,list_df-method
setReplaceMethod("overlap", signature(x = "COBRAPlot",
                                      value = "list_df"),
                 function(x, value) {
                   x@overlap <- value
                   if (validObject(x))
                     x
                 })

#' @rdname splv
#' @aliases splv splv,COBRAPlot-method splv<-,COBRAPlot,character-method
setReplaceMethod("splv", signature(x = "COBRAPlot", value = "character"),
                 function(x, value) {
                   x@splv <- value
                   if (validObject(x))
                     x
                 })

#' @rdname maxsplit
#' @aliases maxsplit maxsplit,COBRAPlot-method
#'   maxsplit<-,COBRAPlot,numeric-method
setReplaceMethod("maxsplit", signature(x = "COBRAPlot",
                                       value = "numeric"),
                 function(x, value) {
                   x@maxsplit <- value
                   if (validObject(x))
                     x
                 })



#' Accessor and replacement functions for \code{plotcolors} slot
#'
#' Accessor and replacement functions for the \code{plotcolors} slot in an
#' \code{COBRAPlot} object.
#'
#' @docType methods
#' @name plotcolors
#' @rdname plotcolors
#' @aliases plotcolors plotcolors,COBRAPlot-method
#'   plotcolors<-,COBRAPlot,character-method
#' @return The accessor function returns a character vector giving the colors
#'   assigned to each of the methods (or method/stratification level
#'   combinations) represented in the \code{COBRAPlot} object.
#'
#' @param x An \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A character vector giving the colors assigned to each of the
#'   methods (or method/stratification level combinations) represented in the
#'   \code{COBRAPlot} object.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "fdrtpr")
#' cobraplot <- prepare_data_for_plot(cobraperf)
#' plotcolors(cobraplot)
setMethod("plotcolors", "COBRAPlot", function(x) x@plotcolors)
#' @name plotcolors
#' @rdname plotcolors
#' @exportMethod "plotcolors<-"
setReplaceMethod("plotcolors", signature(x = "COBRAPlot", value = "character"),
                 function(x, value) {
                   x@plotcolors <- value
                   if (validObject(x))
                     x
                 })

#' Accessor and replacement functions for \code{facetted} slot
#'
#' Accessor and replacement functions for the \code{facetted} slot in an
#' \code{COBRAPlot} object.
#'
#' @docType methods
#' @name facetted
#' @rdname facetted
#' @aliases facetted facetted,COBRAPlot-method
#'   facetted<-,COBRAPlot,logical-method
#' @return The accessor function returns a logical value, indicating whether the
#'   object is formatted for facetted plots (visualizing each stratification
#'   level in a separate panel) or not.
#'
#' @param x An \code{COBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A logical value, indicating whether the object is formatted for
#'   facetted plots (visualizing each stratification level in a separate panel)
#'   or not.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "fdrtpr")
#' cobraplot <- prepare_data_for_plot(cobraperf)
#' facetted(cobraplot)
setMethod("facetted", "COBRAPlot", function(x) x@facetted)
#' @name facetted
#' @rdname facetted
#' @exportMethod "facetted<-"
setReplaceMethod("facetted", signature(x = "COBRAPlot", value = "logical"),
                 function(x, value) {
                   x@facetted <- value
                   if (validObject(x))
                     x
                 })

#' @docType methods
#' @name Extract
#' @rdname Extract
#' @aliases [ [,COBRAPlot-method \S4method{[}{COBRAPlot,ANY,ANY} \S4method{[}{COBRAPlot,ANY,ANY,ANY}
#' @export
#' @examples
#' data(cobradata_example)
#' cobradata_example[c("ENSG00000000457", "ENSG00000000971",
#'                     "ENSG00000000460"), ]
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "fdrtpr")
#' cobraperf[, c("voom")]
#' cobraplot <- prepare_data_for_plot(cobraperf)
#' cobraplot[, c("voom")]
setMethod("[", "COBRAPlot",
          function(x, i = "missing", j, drop = "missing") {
            if (length(intersect(j, basemethods(x))) == 0)
              stop("none of the provided method found in the object, ",
                   "no subsetting done")
            if (length(x@plotcolors) != 0) {
              combs <-
                expand.grid(c(j, "truth"), c("", paste0("_", stratiflevels(x))),
                            c("", "yes", "no"))
              keepcols <- paste0(combs[, 1], combs[, 2], combs[, 3])
              x@plotcolors <- x@plotcolors[names(x@plotcolors) %in% keepcols]
            }
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
              if (class(x@overlap) == "data.frame") {
                x@overlap <-
                  x@overlap[, which(colnames(x@overlap) %in% c(j, "truth")),
                            drop = FALSE]
              } else {
                x@overlap <-
                  lapply(x@overlap, function(w) {
                    w[, which(colnames(w) %in% c(j, "truth")), drop = FALSE]
                  })
              }
            }
            x
          })

setValidity("COBRAPlot",
            function(object) {
              msg <- NULL
              valid <- TRUE
              if (valid) TRUE else msg
            })

#' Convert an object to another class
#'
#' Convert object between \code{COBRAPerformance} and \code{COBRAPlot} classes.
#'
#' @docType methods
#' @name coerce
#' @rdname coerce
#' @aliases coerce coerce,COBRAPerformance,COBRAPlot-method coerce,
#'   COBRAPlot,COBRAPerformance-method
#'
#' @param from The object that is to be coerced into another class.
#' @author Charlotte Soneson
#' @export
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "fdrtpr")
#' cobraplot <- prepare_data_for_plot(cobraperf)
#'
#' ## Coerce COBRAPerformance object into COBRAPlot object
#' as(cobraperf, "COBRAPlot")
#'
#' ## Coerce COBRAPlot object into COBRAPerformance object
#' as(cobraplot, "COBRAPerformance")
setAs("COBRAPerformance", "COBRAPlot",
      function(from) {
        .COBRAPlot(fdrtpr = from@fdrtpr, fdrtprcurve = from@fdrtprcurve,
                   fdrnbr = from@fdrnbr, fdrnbrcurve = from@fdrnbrcurve,
                   tpr = from@tpr, fpr = from@fpr, roc = from@roc,
                   fpc = from@fpc, onlyshared = from@onlyshared,
                   scatter = from@scatter, deviation = from@deviation,
                   overlap = from@overlap, plotcolors = "", corr = from@corr,
                   splv = from@splv, maxsplit = from@maxsplit, facetted = TRUE)
      })

#' @docType methods
#' @name coerce
#' @rdname coerce
#' @aliases coerce coerce,COBRAPlot,COBRAPerformance-method
#' @export
setAs("COBRAPlot", "COBRAPerformance",
      function(from) {
        .COBRAPerformance(fdrtpr = from@fdrtpr, fdrtprcurve = from@fdrtprcurve,
                          fdrnbr = from@fdrnbr, fdrnbrcurve = from@fdrnbrcurve,
                          tpr = from@tpr, fpr = from@fpr, roc = from@roc,
                          fpc = from@fpc, deviation = from@deviation,
                          corr = from@corr, onlyshared = from@onlyshared,
                          scatter = from@scatter, overlap = from@overlap,
                          splv = from@splv, maxsplit = from@maxsplit)
      })
