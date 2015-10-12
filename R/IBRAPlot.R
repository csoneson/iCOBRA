#' @rdname IBRAPlot
#' @export
.IBRAPlot <- setClass("IBRAPlot",
                      slots = c(plotcolors = "character",
                                facetted = "logical"),
                      contains = "IBRAPerformance")

#' \code{IBRAPlot} object and constructor
#'
#' The \code{IBRAPlot} class is similar to the \code{IBRAPerformance} class in
#' that it holds various types of calculated performance measures. However, it
#' also contains other attributes that are necessary for plotting, such as color
#' assignments. Several \code{IBRAPlot} objects can be generated from the same
#' \code{IBRAPerformance} object, without having to go through the potentially
#' time consuming task of recalculating all performance measures. Objects from
#' this class are typically generated from an \code{IBRAPerformance} object by
#' means of the function \code{\link{prepare_data_for_plot}}.
#'
#' @param plotcolors A character vector giving the color for each method (or
#'   method-stratification level combination).
#' @param facetted A logical indicating whether the data is prepared for a
#'   facetted plot (separating different stratification levels into different
#'   panels) or for displaying all values in one plot panel.
#' @param object_to_extend An \code{IBRAPlot} object to extend with the provided
#'   information.
#'
#' @inheritParams IBRAPerformance
#' @include IBRAPerformance.R
#'
#' @aliases IBRAPlot IBRAPlot-class
#'
#' @docType class
#'
#' @export
#' @rdname IBRAPlot
#' @author Charlotte Soneson
IBRAPlot <- function(fdrtpr = data.frame(), fdrtprcurve = data.frame(),
                     fdrnbr = data.frame(), corr = data.frame(),
                     fdrnbrcurve = data.frame(), tpr = data.frame(),
                     fpr = data.frame(), roc = data.frame(), scatter = data.frame(),
                     fpc = data.frame(), overlap = data.frame(),
                     plotcolors = c(), splv = "",
                     maxsplit = NA_integer_, facetted = TRUE, object_to_extend = NULL) {

  if (!(is.null(object_to_extend))) {
    if (!(class(object_to_extend) == "IBRAPlot")) {
      stop("object_to_extend must be a IBRAPlot object")
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
      if (length(object_to_extend@scatter) != 0)
        scatter <- object_to_extend@scatter
      if (length(object_to_extend@overlap) != 0)
        overlap <- object_to_extend@overlap
      if (length(object_to_extend@plotcolors) != 0)
        plotcolors <- c(object_to_extend@plotcolors,
                        plotcolors[setdiff(names(plotcolors),
                                           names(object_to_extend@plotcolors))])
      if ((object_to_extend@splv) != "")
        splv <- object_to_extend@splv
      if (!is.na(object_to_extend@maxsplit))
        maxsplit <- object_to_extend@maxsplit
      facetted <- object_to_extend@facetted
    }
  }
  .IBRAPlot(fdrtpr = fdrtpr, fdrtprcurve = fdrtprcurve,
            fdrnbr = fdrnbr, fdrnbrcurve = fdrnbrcurve,
            tpr = tpr, fpr = fpr, roc = roc, fpc = fpc, scatter = scatter,
            overlap = overlap, plotcolors = plotcolors, corr = corr,
            splv = splv, maxsplit = maxsplit, facetted = facetted)
}

setMethod("show", "IBRAPlot", function(object) {
  cat("An object of class \"", class(object), "\"\n", sep = "")
  for (sl in slotNames(object)) {
    x <- slot(object, sl)
    cat("@", sl, "\n", sep = "")
    .printHead(x)
    cat("\n")
  }
})

#' Accessor and replacement functions for \code{plotcolors} slot
#'
#' Accessor and replacement functions for the \code{plotcolors} slot in an
#' \code{IBRAPlot} object.
#'
#' @docType methods
#' @name plotcolors
#' @rdname plotcolors
#' @aliases plotcolors plotcolors,IBRAPlot-method
#'   plotcolors<-,IBRAPlot,character-method
#'
#' @param x An \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A character vector giving the colors assigned to each of the
#'   methods (or method/stratification level combinations) represented in the
#'   \code{IBRAPlot} object.
#' @author Charlotte Soneson
#' @export
setMethod("plotcolors", "IBRAPlot", function(x) x@plotcolors)

#'@name plotcolors
#'@rdname plotcolors
#'@exportMethod "plotcolors<-"
setReplaceMethod("plotcolors", signature(x = "IBRAPlot", value = "character"),
                 function(x, value) {
                   x@plotcolors <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessor and replacement functions for \code{facetted} slot
#'
#' Accessor and replacement functions for the \code{facetted} slot in an
#' \code{IBRAPlot} object.
#'
#' @docType methods
#' @name facetted
#' @rdname facetted
#' @aliases facetted facetted,IBRAPlot-method facetted<-,IBRAPlot,logical-method
#'
#' @param x An \code{IBRAPlot} object.
#' @param ... Additional arguments.
#' @param value A logical value, indicating whether the object is formatted for
#'   facetted plots (visualizing each stratification level in a separate panel)
#'   or not.
#' @author Charlotte Soneson
#' @export
setMethod("facetted", "IBRAPlot", function(x) x@facetted)

#'@name facetted
#'@rdname facetted
#'@exportMethod "facetted<-"
setReplaceMethod("facetted", signature(x = "IBRAPlot", value = "logical"),
                 function(x, value) {
                   x@facetted <- value
                   if (validObject(x))
                     return(x)
                 })

#' @docType methods
#' @name Extract
#' @rdname Extract
#' @aliases [ [,IBRAPlot-method \S4method{[}{IBRAPlot,ANY,ANY}
#' @export
setMethod("[", "IBRAPlot",
          function(x, i = "missing", j, drop = "missing") {
            if (length(intersect(j, basemethods(x))) == 0)
              stop("None of the provided method found in the object. No subsetting done.")
            if (length(x@plotcolors) != 0) {
              combs <- expand.grid(c(j, "truth"), c("", paste0("_", stratiflevels(x))), c("", "yes", "no"))
              keepcols <- paste0(combs[, 1], combs[, 2], combs[, 3])
              x@plotcolors <- x@plotcolors[names(x@plotcolors) %in% keepcols]
            }
            if (length(x@tpr) != 0) x@tpr <- x@tpr[which(x@tpr$basemethod %in% j), ]
            if (length(x@fpr) != 0) x@fpr <- x@fpr[which(x@fpr$basemethod %in% j), ]
            if (length(x@roc) != 0) x@roc <- x@roc[which(x@roc$basemethod %in% j), ]
            if (length(x@fpc) != 0) x@fpc <- x@fpc[which(x@fpc$basemethod %in% j), ]
            if (length(x@fdrtpr) != 0) x@fdrtpr <- x@fdrtpr[which(x@fdrtpr$basemethod %in% j), ]
            if (length(x@fdrnbr) != 0) x@fdrnbr <- x@fdrnbr[which(x@fdrnbr$basemethod %in% j), ]
            if (length(x@fdrtprcurve) != 0) x@fdrtprcurve <-
                x@fdrtprcurve[which(x@fdrtprcurve$basemethod %in% j), ]
            if (length(x@fdrnbrcurve) != 0) x@fdrnbrcurve <-
                x@fdrnbrcurve[which(x@fdrnbrcurve$basemethod %in% j), ]
            if (length(x@corr) != 0) x@corr <- x@corr[which(x@corr$basemethod %in% j), ]
            if (length(x@scatter) != 0) x@scatter <- x@scatter[which(x@scatter$basemethod %in% j), ]
            if (length(x@overlap) != 0) {
              if (class(x@overlap) == "list") {
                x@overlap <-
                  lapply(x@overlap, function(w) {
                    w[, which(colnames(w) %in% c(j, "truth")), drop = FALSE]
                  })
              } else {
                x@overlap <-
                  x@overlap[, which(colnames(x@overlap) %in% c(j, "truth")), drop = FALSE]
              }
            }
            x
          })

setValidity("IBRAPlot",
            function(object) {
              msg <- NULL
              valid <- TRUE
              if (valid) TRUE else msg
            })

#' Convert an object to another class
#'
#' Convert object between \code{IBRAPerformance} and \code{IBRAPlot} classes.
#'
#' @docType methods
#' @name coerce
#' @rdname coerce
#' @aliases coerce coerce,IBRAPerformance,IBRAPlot-method coerce, IBRAPlot,IBRAPerformance-method
#'
#' @param from The object that is to be coerced into another class.
#' @author Charlotte Soneson
#' @export
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100), row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)), row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#' ibraperf <- calculate_performance(ibradata, binary_truth = "status",
#'                                   aspects = c("fdrtpr", "fdrtprcurve",
#'                                               "tpr", "roc"),
#'                                   thrs = c(0.01, 0.05, 0.1), splv = "none")
#'
#' ## Coerce IBRAPerformance object into IBRAPlot object
#' ibraplot <- as(ibraperf, "IBRAPlot")
#'
#' ## Coerce IBRAPlot object into IBRAPerformance object
#' ibraperf2 <- as(ibraplot, "IBRAPerformance")
setAs("IBRAPerformance", "IBRAPlot",
      function(from) {
        .IBRAPlot(fdrtpr = from@fdrtpr, fdrtprcurve = from@fdrtprcurve,
                  fdrnbr = from@fdrnbr, fdrnbrcurve = from@fdrnbrcurve,
                  tpr = from@tpr, fpr = from@fpr, roc = from@roc, fpc = from@fpc,
                  scatter = from@scatter,
                  overlap = from@overlap, plotcolors = "", corr = from@corr,
                  splv = from@splv, maxsplit = from@maxsplit, facetted = TRUE)
      })

#' @docType methods
#' @name coerce
#' @rdname coerce
#' @aliases coerce coerce,IBRAPlot,IBRAPerformance-method
#' @export
setAs("IBRAPlot", "IBRAPerformance",
      function(from) {
        .IBRAPerformance(fdrtpr = from@fdrtpr, fdrtprcurve = from@fdrtprcurve,
                         fdrnbr = from@fdrnbr, fdrnbrcurve = from@fdrnbrcurve,
                         tpr = from@tpr, fpr = from@fpr, roc = from@roc, fpc = from@fpc,
                         scatter = from@scatter, overlap = from@overlap, corr = from@corr,
                         splv = from@splv, maxsplit = from@maxsplit)
      })
