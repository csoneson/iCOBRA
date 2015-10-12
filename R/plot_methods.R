## ------------------- FDR or TPR or CORR ----------------------------- ##
#' @import ggplot2
plot_fpr_tpr <- function(ibraplot, title, stripsize, titlecol, pointsize,
                         xaxisrange, aspc) {
  if (aspc == "FPR")
    plot_data <- fpr(ibraplot)
  else if (aspc == "TPR")
    plot_data <- tpr(ibraplot)
  else if (aspc %in% c("SPEARMAN", "PEARSON"))
    plot_data <- corr(ibraplot)

  if (!(isTRUE(facetted(ibraplot)))) {
    plot_data$method <- plot_data$fullmethod
  }

  pp <-ggplot(plot_data, aes_string(x = aspc, y = "method", group = "method")) +
    geom_point(size = pointsize + 1,
               aes_string(colour = "method"), shape = 19) +
    scale_color_manual(values = plotcolors(ibraplot), name = "") +
    xlim(xaxisrange[1], xaxisrange[2]) +
    plot_theme(stripsize = stripsize, titlecol = titlecol) +
    ggtitle(title)
  if (isTRUE(facetted(ibraplot))) {
    pp + facet_wrap(~ splitval, nrow = ceiling((maxsplit(ibraplot) + 1)/3))
  } else {
    pp
  }
}

#' Plot FPR
#'
#' Plot observed false positive rate (FPR) for given adjusted p-value
#' thresholds.
#'
#' @param ibraplot An \code{IBRAPlot} object.
#' @param title A character string giving the title of the plot.
#' @param stripsize A numeric value giving the size of the strip text, when the
#'   results are stratified by an annotation.
#' @param titlecol A character string giving the color of the title.
#' @param pointsize A numeric value giving the size of the plot characters.
#' @param xaxisrange A numeric vector with two elements, giving the lower and
#'   upper boundary of the x-axis, respectively.
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#' @author Charlotte Soneson
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100), row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)), row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#' ibraperf <- calculate_performance(ibradata, binary_truth = "status",
#'                                   aspects = "fpr", thrs = c(0.05, 0.1))
#' ibraplot <- prepare_data_for_plot(ibraperf, keepmethods = c("m1", "m2"),
#'                                   colorscheme = "Dark2", facetted = FALSE)
#' plot_fpr(ibraplot, xaxisrange = c(0, 0.5))
plot_fpr <- function(ibraplot, title = "", stripsize = 15, titlecol = "black",
                     pointsize = 5, xaxisrange = c(0, 1)) {
  plot_fpr_tpr(ibraplot = ibraplot, title = title, stripsize = stripsize,
               titlecol = titlecol, pointsize = pointsize, xaxisrange = xaxisrange,
               aspc = "FPR")
}

#' Plot TPR
#'
#' Plot observed true positive rate (TPR) for given adjusted p-value thresholds.
#'
#' @param ibraplot An \code{IBRAPlot} object.
#' @param title A character string giving the title of the plot.
#' @param stripsize A numeric value giving the size of the strip text, when the
#'   results are stratified by an annotation.
#' @param titlecol A character string giving the color of the title.
#' @param pointsize A numeric value giving the size of the plot characters.
#' @param xaxisrange A numeric vector with two elements, giving the lower and
#'   upper boundary of the x-axis, respectively.
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#' @author Charlotte Soneson
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100), row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)), row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#' ibraperf <- calculate_performance(ibradata, binary_truth = "status",
#'                                   aspects = "tpr", thrs = c(0.05, 0.1))
#' ibraplot <- prepare_data_for_plot(ibraperf, keepmethods = c("m1", "m2"),
#'                                   colorscheme = "Dark2", facetted = FALSE)
#' plot_tpr(ibraplot, xaxisrange = c(0, 0.5))
plot_tpr <- function(ibraplot, title = "", stripsize = 15, titlecol = "black",
                     pointsize = 5, xaxisrange = c(0, 1)) {
  plot_fpr_tpr(ibraplot = ibraplot, title = title, stripsize = stripsize,
               titlecol = titlecol, pointsize = pointsize, xaxisrange = xaxisrange,
               aspc = "TPR")
}

#' Plot correlations
#'
#' Plot correlations between observations and a continuous truth value.
#'
#' @param ibraplot An \code{IBRAPlot} object.
#' @param title A character string giving the title of the plot.
#' @param stripsize A numeric value giving the size of the strip text, when the
#'   results are stratified by an annotation.
#' @param titlecol A character string giving the color of the title.
#' @param pointsize A numeric value giving the size of the plot characters.
#' @param xaxisrange A numeric vector with two elements, giving the lower and
#'   upper boundary of the x-axis, respectively.
#' @param corrtype A character string giving the type of correlation to show.
#'   Either "pearson" or "spearman".
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#' @author Charlotte Soneson
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100), row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)), row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#' ibraperf <- calculate_performance(ibradata, cont_truth = "status", aspects = "corr")
#' ibraplot <- prepare_data_for_plot(ibraperf, keepmethods = c("m1", "m2"),
#'                                   colorscheme = "Dark2", facetted = FALSE)
#' plot_corr(ibraplot, corrtype = "pearson")
plot_corr <- function(ibraplot, title = "", stripsize = 15, titlecol = "black",
                      pointsize = 5, xaxisrange = c(-1, 1), corrtype = "pearson") {
  plot_fpr_tpr(ibraplot = ibraplot, title = title, stripsize = stripsize,
               titlecol = titlecol, pointsize = pointsize, xaxisrange = xaxisrange,
               aspc = toupper(corrtype))
}

## ---------------------- ROC or FPC --------------------------------- ##
#' @import ggplot2
plot_roc_fpc <- function(ibraplot, title, stripsize, titlecol, xaxisrange,
                         yaxisrange, maxnfdc, aspc) {
  if (aspc == "roc")
    plot_data <- roc(ibraplot)
  else if (aspc == "fpc")
    plot_data <- fpc(ibraplot)
  if (!(isTRUE(facetted(ibraplot)))) {
    plot_data$method <- plot_data$fullmethod
  }
  pp <- ggplot(plot_data, aes_string(x = ifelse(aspc == "roc", "FPR", "topN"),
                                     y = ifelse(aspc == "roc", "TPR", "FP"),
                                     group = "method", colour = "method")) +
    geom_path(size = 1) +
    scale_color_manual(values = plotcolors(ibraplot), name = "") +
    plot_theme(stripsize = stripsize, titlecol = titlecol) +
    xlim(ifelse(aspc == "roc", xaxisrange[1], 0), ifelse(aspc == "roc",
                                                         xaxisrange[2], maxnfdc)) +
    ylim(ifelse(aspc == "roc", yaxisrange[1], 0),
         ifelse(aspc == "roc", yaxisrange[2],
                max(plot_data$FP[plot_data$topN <= maxnfdc]))) +
    ggtitle(title)
  if (isTRUE(facetted(ibraplot))) {
    pp + facet_wrap(~ splitval, nrow = ceiling((maxsplit(ibraplot) + 1)/3))
  } else {
    pp
  }
}

#' Plot ROC curves
#'
#' Plot receiver operating characteristics (ROC) curves.
#'
#' @param ibraplot An \code{IBRAPlot} object.
#' @param title A character string giving the title of the plot.
#' @param stripsize A numeric value giving the size of the strip text, when the
#'   results are stratified by an annotation.
#' @param titlecol A character string giving the color of the title.
#' @param xaxisrange A numeric vector with two elements, giving the lower and
#'   upper boundary of the x-axis, respectively.
#' @param yaxisrange A numeric vector with two elements, giving the lower and
#'   upper boundary of the y-axis, respectively.
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#' @author Charlotte Soneson
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100), row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)), row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#' ibraperf <- calculate_performance(ibradata, binary_truth = "status",
#'                                   aspects = "roc")
#' ibraplot <- prepare_data_for_plot(ibraperf, keepmethods = c("m1", "m2"),
#'                                   colorscheme = "Dark2", facetted = FALSE)
#' plot_roc(ibraplot, xaxisrange = c(0, 1), yaxisrange = c(0, 1))
plot_roc <- function(ibraplot, title = "", stripsize = 15, titlecol = "black",
                     xaxisrange = c(0, 1), yaxisrange = c(0, 1)) {
  plot_roc_fpc(ibraplot = ibraplot, title = title, stripsize = stripsize,
               titlecol = titlecol, xaxisrange = xaxisrange, yaxisrange = yaxisrange,
               maxnfdc = NULL, aspc = "roc")
}

#' Plot FP curves
#'
#' Plot false positive curves, indicating the number of false positives among
#' the top-ranked N variables, for varying values of N.
#'
#' @param ibraplot An \code{IBRAPlot} object.
#' @param title A character string giving the title of the plot.
#' @param stripsize A numeric value giving the size of the strip text, when the
#'   results are stratified by an annotation.
#' @param titlecol A character string giving the color of the title.
#' @param maxnfdc A numeric value giving the largest N to consider.
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#' @author Charlotte Soneson
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100), row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)), row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#' ibraperf <- calculate_performance(ibradata, binary_truth = "status",
#'                                   aspects = "fpc")
#' ibraplot <- prepare_data_for_plot(ibraperf, keepmethods = c("m1", "m2"),
#'                                   colorscheme = "Dark2", facetted = FALSE)
#' plot_fpc(ibraplot, maxnfdc = 100)
plot_fpc <- function(ibraplot, title = "", stripsize = 15, titlecol = "black",
                     maxnfdc = 500) {
  plot_roc_fpc(ibraplot = ibraplot, title = title, stripsize = stripsize,
               titlecol = titlecol, xaxisrange = NULL, yaxisrange = NULL,
               maxnfdc = maxnfdc, aspc = "fpc")
}

## ------------------------- SCATTER --------------------------------- ##
#' Plot scatter plots
#'
#' Plot scatter plots, indicating the relationship between observed values and a
#' continuous truth.
#'
#' @param ibraplot An \code{IBRAPlot} object.
#' @param title A character string giving the title of the plot.
#' @param stripsize A numeric value giving the size of the strip text, when the
#'   results are stratified by an annotation.
#' @param titlecol A character string giving the color of the title.
#' @param pointsize A numeric value giving the size of the plot characters.
#' @param doflip A logical indicating whether to flip the axes when results are
#'   stratified by an annotation. By default (\code{doflip = FALSE}),
#'   stratification levels are shown as columns and methods as rows in the plot.
#' @param dolog A logical indicating whether to log10-transform values before
#'   plotting.
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#' @author Charlotte Soneson
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100), row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)), row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#' ibraperf <- calculate_performance(ibradata, cont_truth = "status",
#'                                   aspects = "scatter")
#' ibraplot <- prepare_data_for_plot(ibraperf, keepmethods = c("m1", "m2"),
#'                                   colorscheme = "Dark2", facetted = TRUE)
#' plot_scatter(ibraplot, doflip = TRUE)
plot_scatter <- function(ibraplot, title = "", stripsize = 10, titlecol = "black",
                         pointsize = 3, doflip = FALSE, dolog = FALSE) {
  plot_data <- scatter(ibraplot)

  if (isTRUE(facetted(ibraplot))) {
    plot_data$fullmethod <- plot_data$method
  }
  pp <- ggplot(plot_data, aes_string(x = "OBSERVATION", y = "TRUTH",
                                     colour = "fullmethod")) +
    geom_point(size = pointsize) +
    scale_color_manual(values = plotcolors(ibraplot), name = "") +
    plot_theme(stripsize = stripsize, titlecol = titlecol) +
    ggtitle(title)
  if (isTRUE(facetted(ibraplot))) {
    if (isTRUE(doflip))
      pp <- pp + facet_grid(splitval ~ method)
    else
      pp <- pp + facet_grid(method ~ splitval)
  } else {
    pp <- pp + facet_wrap(~ method)
  }
  if (isTRUE(dolog))
    pp <- pp + scale_x_log10() + scale_y_log10()
  pp
}


## ------------------- FDRTPR or FDRNBR ------------------------------ ##
#' @import ggplot2
plot_fdrcurve <- function(ibraplot, title, stripsize, titlecol, pointsize,
                          xaxisrange, yaxisrange, plottype, aspc) {
  if (aspc == "TPR") {
    plot_data_lines <- fdrtprcurve(ibraplot)
    plot_data_points <- fdrtpr(ibraplot)
  } else if (aspc == "NBR") {
    plot_data_lines <- fdrnbrcurve(ibraplot)
    plot_data_points <- fdrnbr(ibraplot)
  }

  thresholds <- sort(unique(as.numeric(gsub("thr", "", plot_data_points$thr))))
  plot_data_points$method2.satis <- paste0(plot_data_points$method,
                                           plot_data_points$satis)

  if (!(isTRUE(facetted(ibraplot)))) {
    plot_data_points$method <- plot_data_points$fullmethod
    plot_data_lines$method <- plot_data_lines$fullmethod
    plot_data_points$method2.satis <- paste0(plot_data_points$fullmethod,
                                             plot_data_points$satis)
  }

  if ("curve" %in% plottype & "points" %in% plottype) {
    pp <- ggplot(plot_data_lines, aes_string(x = "FDR", y = aspc,
                                             group = "method", colour = "method")) +
      geom_vline(xintercept = seq(0, xaxisrange[2], 0.1),
                 colour = "lightgrey", linetype = "dashed") +
      geom_vline(xintercept = thresholds, linetype = "dashed") +
      geom_path(size = 1) +
      geom_point(data = plot_data_points, size = pointsize + 1,
                 aes_string(colour = "method"), shape = 19) +
      geom_point(data = plot_data_points, size = pointsize,
                 aes_string(fill = "method2.satis", colour = "method"), shape = 21) +
      scale_fill_manual(values = plotcolors(ibraplot), guide = FALSE, name = "") +
      scale_color_manual(values = plotcolors(ibraplot), name = "") +
      ylim(ifelse(aspc == "TPR", yaxisrange[1], 0),
           ifelse(aspc == "TPR", yaxisrange[2],
                  max(plot_data_lines$NBR[plot_data_lines$FDR <= xaxisrange[2]]))) +
      scale_x_continuous(breaks = c(0, thresholds, seq(0.1, xaxisrange[2], 0.1)),
                         labels = c("", thresholds, seq(0.1, xaxisrange[2], 0.1)),
                         limits = c(xaxisrange[1], xaxisrange[2])) +
      plot_theme(stripsize = stripsize, titlecol = titlecol) +
      ggtitle(title)
  } else if ("curve" %in% plottype) {
    pp <- ggplot(plot_data_lines, aes_string(x = "FDR", y = aspc,
                                             group = "method", colour = "method")) +
      geom_path(size = 1) +
      xlim(xaxisrange[1], xaxisrange[2]) +
      ylim(ifelse(aspc == "TPR", yaxisrange[1], 0),
           ifelse(aspc == "TPR", yaxisrange[2],
                  max(plot_data_lines$NBR[plot_data_lines$FDR <= xaxisrange[2]]))) +
      scale_color_manual(values = plotcolors(ibraplot), name = "") +
      plot_theme(stripsize = stripsize, titlecol = titlecol) +
      ggtitle(title)
  } else if ("points" %in% plottype) {
    pp <- ggplot(plot_data_points, aes_string(x = "FDR", y = aspc, group = "method")) +
      geom_vline(xintercept = seq(0, xaxisrange[2], 0.1),
                 colour = "lightgrey", linetype = "dashed") +
      geom_vline(xintercept = thresholds, linetype = "dashed") +
      geom_path(size = 1, aes_string(colour = "method")) +
      geom_point(size = pointsize + 1,
                 aes_string(colour = "method"), shape = 19) +
      geom_point(size = pointsize,
                 aes_string(fill = "method2.satis", colour = "method"), shape = 21) +
      scale_fill_manual(values = plotcolors(ibraplot), guide = FALSE, name = "") +
      scale_color_manual(values = plotcolors(ibraplot), name = "") +
      ylim(ifelse(aspc == "TPR", yaxisrange[1], 0),
           ifelse(aspc == "TPR", yaxisrange[2],
                  max(plot_data_lines$NBR[plot_data_lines$FDR <= xaxisrange[2]]))) +
      scale_x_continuous(breaks = c(0, thresholds, seq(0.1, xaxisrange[2], 0.1)),
                         labels = c("", thresholds, seq(0.1, xaxisrange[2], 0.1)),
                         limits = c(xaxisrange[1], xaxisrange[2])) +
      plot_theme(stripsize = stripsize, titlecol = titlecol) +
      ggtitle(title)
  }
  if (isTRUE(facetted(ibraplot))) {
    pp + facet_wrap(~ splitval, nrow = ceiling((maxsplit(ibraplot) + 1)/3))
  } else {
    pp
  }
}

#' Plot TPR vs FDR
#'
#' Plot observed true positive rate (TPR) vs observed false discovery rate
#' (FDR), for given adjusted p-value thresholds and/or as curves traced out by
#' considering all threshold values.
#'
#' @param ibraplot An \code{IBRAPlot} object.
#' @param title A character string giving the title of the plot.
#' @param stripsize A numeric value giving the size of the strip text, when the
#'   results are stratified by an annotation.
#' @param titlecol A character string giving the color of the title.
#' @param pointsize A numeric value giving the size of the plot characters.
#' @param xaxisrange A numeric vector with two elements, giving the lower and
#'   upper boundary of the x-axis, respectively.
#' @param yaxisrange A numeric vector with two elements, giving the lower and
#'   upper boundary of the y-axis, respectively.
#' @param plottype A character vector giving the type of plot to construct. Can
#'   be any combination of the two elements "curve" and "points".
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#' @author Charlotte Soneson
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100),
#'                    row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)), row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#' ibraperf <- calculate_performance(ibradata, binary_truth = "status",
#'                                   aspects = c("fdrtpr", "fdrtprcurve"),
#'                                   thrs = c(0.05, 0.1))
#' ibraplot <- prepare_data_for_plot(ibraperf, keepmethods = c("m1", "m2"),
#'                                   colorscheme = "Dark2", facetted = FALSE)
#' plot_fdrtprcurve(ibraplot, plottype = c("curve", "points"))
plot_fdrtprcurve <- function(ibraplot, title = "", stripsize = 15,
                             titlecol = "black", pointsize = 5, xaxisrange = c(0, 1),
                             yaxisrange = c(0, 1), plottype = c("curve", "points")) {
  plot_fdrcurve(ibraplot = ibraplot, title = title, stripsize = stripsize,
                titlecol = titlecol, pointsize = pointsize, xaxisrange = xaxisrange,
                yaxisrange = yaxisrange, plottype = plottype, aspc = "TPR")
}

#' Plot number of significant features vs FDR
#'
#' Plot the number of features considered significant vs observed false
#' discovery rate (FDR), for given adjusted p-value thresholds and/or as curves
#' traced out by considering all threshold values.
#'
#' @param ibraplot An \code{IBRAPlot} object.
#' @param title A character string giving the title of the plot.
#' @param stripsize A numeric value giving the size of the strip text, when the
#'   results are stratified by an annotation.
#' @param titlecol A character string giving the color of the title.
#' @param pointsize A numeric value giving the size of the plot characters.
#' @param xaxisrange A numeric vector with two elements, giving the lower and
#'   upper boundary of the x-axis, respectively.
#' @param plottype A character vector giving the type of plot to construct. Can
#'   be any combination of the two elements "curve" and "points".
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#' @author Charlotte Soneson
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100),
#'                    row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)), row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#' ibraperf <- calculate_performance(ibradata, binary_truth = "status",
#'                                   aspects = c("fdrnbr", "fdrnbrcurve"),
#'                                   thrs = c(0.05, 0.1))
#' ibraplot <- prepare_data_for_plot(ibraperf, keepmethods = c("m1", "m2"),
#'                                   colorscheme = "Dark2", facetted = FALSE)
#' plot_fdrnbrcurve(ibraplot, plottype = c("curve", "points"))
plot_fdrnbrcurve <- function(ibraplot, title = "", stripsize = 15,
                             titlecol = "black", pointsize = 5, xaxisrange = c(0, 1),
                             plottype = c("curve", "points")) {
  plot_fdrcurve(ibraplot = ibraplot, title = title, stripsize = stripsize,
                titlecol = titlecol, pointsize = pointsize, xaxisrange = xaxisrange,
                yaxisrange = NULL, plottype = plottype, aspc = "NBR")
}

## ------------------------ OVERLAP ---------------------------------- ##
#' Plot Venn diagram
#'
#' Plot a Venn diagram showing the overlaps among sets of significant feature
#' for a given adjusted p-value threshold. Optionally, the truth can be included
#' as a "perfect" method. Note that maximally five methods (including the truth,
#' if applicable) can be compared.
#'
#' @param ibraplot An \code{IBRAPlot} object.
#' @param ... Additional arguments to \code{limma::vennDiagram}.
#'
#' @return Nothing, displays a graph
#'
#' @export
#' @author Charlotte Soneson
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100),
#'                    row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)), row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#' ibraperf <- calculate_performance(ibradata, binary_truth = "status",
#'                                   aspects = "overlap", thr_venn = 0.1)
#' ibraplot <- prepare_data_for_plot(ibraperf, keepmethods = c("m1", "m2"),
#'                                   colorscheme = "Dark2", incltruth = TRUE)
#' plot_overlap(ibraplot)
plot_overlap <- function(ibraplot, ...) {
  overlap_table <- overlap(ibraplot)
  circle.col <- plotcolors(ibraplot)
  if (length(overlap_table) == 0)
    return(NULL)

  if (class(overlap_table) != "list") {
    if (ncol(overlap_table) < 6) {
      if (ncol(overlap_table) == 5)
        cols <- rep(circle.col[colnames(overlap_table)], 2)[2:6]
      else
        cols <- circle.col[colnames(overlap_table)]
      limma::vennDiagram(overlap_table, circle.col = cols, ...)
    } else {
      NULL
    }
  } else {
    ncl <- ceiling(sqrt(length(overlap_table)))
    nrw <- ceiling(length(overlap_table)/ncl)
    par(mfrow = c(nrw, ncl), mar = c(4, 1, 1, 1))
    for (i in 1:length(overlap_table)) {
      if (ncol(overlap_table[[i]]) < 6) {
        if (ncol(overlap_table[[i]]) == 5)
          cols <- rep(circle.col[colnames(overlap_table[[i]])], 2)[2:6]
        else
          cols <- circle.col[colnames(overlap_table[[i]])]
        limma::vennDiagram(overlap_table[[i]], circle.col = cols,
                           main = paste0(splv(ibraplot), ":",
                                         names(overlap_table)[i]), ...)
      } else {
        NULL
      }
    }
    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
  }
}
