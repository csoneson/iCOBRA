## This function is almost verbatim taken from limma. The only change is on
## lines 35 and 43, adding drop = FALSE, and removal of some unncessary lines.
.printHead <- function (x) {
  if (is.atomic(x)) {
    d <- dim(x)
    if (length(d) < 2)
      which <- "OneD"
    if (length(d) == 2)
      which <- "TwoD"
#     if (length(d) > 2)
#       which <- "Array"
  } else {
    if (inherits(x, "data.frame")) {
      d <- dim(x)
      which <- "TwoD"
    } else {
#       if (is.call(x))
#         which <- "Call"
#       else {
        if (is.recursive(x))
          which <- "Recursive"
        else which <- "Other"
      # }
    }
  }
  switch(which, OneD = {
    n <- length(x)
    if (n > 20) {
      print(x[1:5])
      cat(n - 5, "more elements ...\n")
    } else print(x)
  }, TwoD = {
    n <- d[1]
    if (n > 10) {
      print(x[1:5, , drop = FALSE])
      cat(n - 5, "more rows ...\n")
    } else print(x)
#   }, Array = {
#     n <- d[1]
#     if (n > 10) {
#       dn <- dimnames(x)
#       dim(x) <- c(d[1], prod(d[-1]))
#       x <- x[1:5, , drop = FALSE]
#       dim(x) <- c(5, d[-1])
#       if (!is.null(dn[[1]])) dn[[1]] <- dn[[1]][1:5]
#       dimnames(x) <- dn
#       print(x)
#       cat(n - 5, "more rows ...\n")
#     } else print(x)
  }, Recursive = {
    n <- length(x)
    if (n) {
      i <- names(x)
      if (is.null(i)) i <- seq_len(n)
      for (what in i) {
        y <- x[[what]]
        cat("$", what, "\n", sep = "")
        Recall(y)
        cat("\n")
      }
    }
  },
# Call = ,
Other = print(x))
}
