#' @rdname IBRAData
#' @export
.IBRAData <- setClass("IBRAData",
                      slots = c(pval = "data.frame", padj = "data.frame",
                                score = "data.frame", truth = "data.frame"))

#' IBRAData object and constructor
#'
#' The IBRAData class contains slots to hold calculated p-values, adjusted
#' p-values and general 'scores' for a set of features. The slots can contain
#' values from multiple methods, and each method can contribute to one or more
#' slots. The class also contains a slot giving the 'truth' (a binary assignment
#' and/or a continuous score) for each feature, as well as additional
#' annotations that can be used to stratify the performance calculations.
#'
#' If adjusted p-values are missing for some methods, for which nominal p-values
#' are available, the adjusted p-values can be calculated using the
#' \code{calculate_adjp} function.
#'
#' @param pval A data frame with features as rows and methods as columns,
#'   containing nominal p-values. Missing values (\code{NA}s) are allowed. The
#'   row names should be feature names.
#' @param padj A data frame with features as rows and methods as columns,
#'   containing adjusted p-values. Missing values (\code{NA}s) are allowed. The
#'   row names should be feature names.
#' @param score A data frame with features as rows and methods as columns,
#'   containing generic scores. In case of comparison to a binary truth, larger
#'   values of the scores should correspond to 'more significant' features.
#'   Missing values (\code{NA}s) are allowed. The row names should be feature
#'   names.
#' @param truth A data frame with features as rows columns containing feature
#'   annotations such as, e.g., binary and continuous truths and additional
#'   annotations that can be used to stratify the performance calculations. The
#'   row names should be feature names.
#' @param object_to_extend An IBRAData object
#'
#' @return An IBRAData object
#'
#' @aliases IBRAData IBRAData-class
#'
#' @docType class
#'
#' @export
#' @rdname IBRAData
#' @author Charlotte Soneson
IBRAData <- function(pval = data.frame(), padj = data.frame(),
                     score = data.frame(), truth = data.frame(),
                     object_to_extend = NULL) {

  if (!(is.null(object_to_extend))) {
    if (!(class(object_to_extend) == "IBRAData")) {
      stop("object_to_extend must be a IBRAData object")
    } else {
      if (length(object_to_extend@pval) != 0) {
        if (length(pval) != 0 && length(setdiff(colnames(pval), colnames(object_to_extend@pval))) > 0) {
          pval <- pval[, setdiff(colnames(pval), colnames(object_to_extend@pval)), drop = FALSE]
          pval <- merge(object_to_extend@pval, pval, by = 0, all = TRUE)
          rownames(pval) <- pval$Row.names
          pval$Row.names <- NULL
        } else {
          pval <- object_to_extend@pval
        }
      }

      if (length(object_to_extend@padj) != 0) {
        if (length(padj) != 0 && length(setdiff(colnames(padj), colnames(object_to_extend@padj))) > 0) {
          padj <- padj[, setdiff(colnames(padj), colnames(object_to_extend@padj)), drop = FALSE]
          padj <- merge(object_to_extend@padj, padj, by = 0, all = TRUE)
          rownames(padj) <- padj$Row.names
          padj$Row.names <- NULL
        } else {
          padj <- object_to_extend@padj
        }
      }

      if (length(object_to_extend@score) != 0) {
        if (length(score) != 0 && length(setdiff(colnames(score), colnames(object_to_extend@score))) > 0) {
          score <- score[, setdiff(colnames(score), colnames(object_to_extend@score)), drop = FALSE]
          score <- merge(object_to_extend@score, score, by = 0, all = TRUE)
          rownames(score) <- score$Row.names
          score$Row.names <- NULL
        } else {
          score <- object_to_extend@score
        }
      }

      if (length(object_to_extend@truth) != 0) {
        truth <- object_to_extend@truth
      }
    }
  }
  .IBRAData(pval = pval, padj = padj, score = score, truth = truth)
}

setMethod("show", "IBRAData",
          function(object) {
            cat("An object of class \"", class(object), "\"\n", sep = "")
            for (sl in slotNames(object)) {
              x <- slot(object, sl)
              cat("@", sl, "\n", sep = "")
              .printHead(x)
              cat("\n")
            }
          })

#' Accessors for the 'pval' slot of an IBRAData object
#'
#' @docType methods
#' @name pval
#' @rdname pval
#' @aliases pval pval,IBRAData-method pval<-,IBRAData,data.frame-method
#' @param x An IBRAData object
#' @param ... Additional arguments
#' @param value A data frame
#' @author Charlotte Soneson
#' @export
setMethod("pval", "IBRAData", function(x) x@pval)
#' @name pval
#' @rdname pval
#' @exportMethod "pval<-"
setReplaceMethod("pval", signature(x = "IBRAData", value = "data.frame"),
                 function(x, value) {
                   x@pval <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessors for the 'padj' slot of an IBRAData object
#'
#' @docType methods
#' @name padj
#' @rdname padj
#' @aliases padj padj,IBRAData-method padj<-,IBRAData,data.frame-method
#' @param x An IBRAData object
#' @param ... Additional arguments
#' @param value A data frame
#' @author Charlotte Soneson
#' @export
setMethod("padj", "IBRAData", function(x) x@padj)
#' @name padj
#' @rdname padj
#' @exportMethod "padj<-"
setReplaceMethod("padj", signature(x = "IBRAData", value = "data.frame"),
                 function(x, value) {
                   x@padj <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessors for the 'score' slot of an IBRAData object
#'
#' @docType methods
#' @name score
#' @rdname score
#' @aliases score score,IBRAData-method score<-,IBRAData,data.frame-method
#' @param x An IBRAData object
#' @param ... Additional arguments
#' @param value A data frame
#' @author Charlotte Soneson
#' @export
setMethod("score", "IBRAData", function(x) x@score)
#' @name score
#' @rdname score
#' @exportMethod "score<-"
setReplaceMethod("score", signature(x = "IBRAData", value = "data.frame"),
                 function(x, value) {
                   x@score <- value
                   if (validObject(x))
                     return(x)
                 })

#' Accessors for the 'truth' slot of an IBRAData object
#'
#' @docType methods
#' @name truth
#' @rdname truth
#' @aliases truth truth,IBRAData-method truth<-,IBRAData,data.frame-method
#' @param x An IBRAData object
#' @param ... Additional arguments
#' @param value A data frame
#' @author Charlotte Soneson
#' @export
setMethod("truth", "IBRAData", function(x) x@truth)
#' @name truth
#' @rdname truth
#' @exportMethod "truth<-"
setReplaceMethod("truth", signature(x = "IBRAData", value = "data.frame"),
                 function(x, value) {
                   x@truth <- value
                   if (validObject(x))
                     return(x)
                 })

#' Subsetting an IBRAData, IBRAPerformance or IBRAPlot object
#'
#' @docType methods
#' @name Extract
#' @rdname Extract
#' @aliases \S4method{[}{IBRAData,ANY,ANY} [ [,IBRAData-method
#' @param x An IBRAData, IBRAPerformance or IBRAPlot object
#' @param i For IBRAData objects, a character vector of feature names to retain
#' @param j For IBRAPerformance and IBRAPlot objects, a character vector with
#'   method names to retain
#' @param drop not used
#' @export
setMethod("[", "IBRAData",
          function(x, i, j = "missing", drop = "missing") {
            if (length(x@pval) != 0 & length(intersect(rownames(x@pval), i)) == 0)
              stop("None of the provided features found in the pval slot.")
            if (length(x@padj) != 0 & length(intersect(rownames(x@padj), i)) == 0)
              stop("None of the provided features found in the padj slot.")
            if (length(x@score) != 0 & length(intersect(rownames(x@score), i)) == 0)
              stop("None of the provided features found in the score slot.")
            if (length(x@truth) != 0 & length(intersect(rownames(x@truth), i)) == 0)
              stop("None of the provided features found in the truth slot.")
            .pval <- x@pval[match(i, rownames(x@pval)), , drop = FALSE]
            .padj <- x@padj[match(i, rownames(x@padj)), , drop = FALSE]
            .score <- x@score[match(i, rownames(x@score)), , drop = FALSE]
            .truth <- x@truth[match(i, rownames(x@truth)), , drop = FALSE]
            .IBRAData(pval = .pval, padj = .padj, score = .score,
                      truth = .truth)
          })

## Validity
setValidity("IBRAData",
            function(object) {
              msg <- NULL
              valid <- TRUE
              if (length(object@pval) != 0 & length(object@truth) != 0 &
                  length(intersect(rownames(object@pval),
                                   rownames(object@truth))) == 0) {
                valid <- FALSE
                msg <- c(msg, "pval slot does not share any features with truth slot")
              }
              if (length(object@padj) != 0 & length(object@truth) != 0 &
                  length(intersect(rownames(object@padj),
                                   rownames(object@truth))) == 0) {
                valid <- FALSE
                msg <- c(msg, "padj slot does not share any features with truth slot")
              }
              if (length(object@score) != 0 & length(object@truth) != 0 &
                  length(intersect(rownames(object@score),
                                   rownames(object@truth))) == 0) {
                valid <- FALSE
                msg <- c(msg, "score slot does not share any features with truth slot")
              }
              if (valid) TRUE else msg
            })

## TODO
# setAs("IBRAData", "SimResults",
#       function(from) {
#         x <- rgb(from@red, from@green, from@blue)
#         col <- unique(x)
#         x <- match(x, col)
#         x <- matrix(x, nrow = from@size[1], ncol = from@size[2])
#         new("SimResults", size = from@size, index = x, col = col)
#       })


