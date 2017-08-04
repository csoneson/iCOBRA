## ------------------- FDR or TPR or CORR ----------------------------- ##
#' @import ggplot2
plot_fpr_tpr <- function(cobraplot, title, stripsize, titlecol, pointsize,
                         xaxisrange, aspc) {
  if (aspc == "FPR")
    plot_data <- fpr(cobraplot)
  else if (aspc == "TPR")
    plot_data <- tpr(cobraplot)
  else if (aspc %in% c("SPEARMAN", "PEARSON"))
    plot_data <- corr(cobraplot)

  if (!(isTRUE(facetted(cobraplot)))) {
    plot_data$method <- plot_data$fullmethod
  }

  pp <- ggplot(plot_data, aes_string(x = aspc, y = "method", group = "method")) +
    geom_point(size = pointsize + 1,
               aes_string(colour = "method"), shape = 19) +
    scale_color_manual(values = plotcolors(cobraplot), name = "") +
    xlim(xaxisrange[1], xaxisrange[2]) +
    plot_theme(stripsize = stripsize, titlecol = titlecol) +
    ggtitle(title)
  if (isTRUE(facetted(cobraplot))) {
    if (!is.finite(maxsplit(cobraplot)))
      msp <- length(unique(plot_data$splitval))
    else
      msp <- maxsplit(cobraplot)
    pp + facet_wrap(~ splitval, nrow = ceiling((msp + 1)/3))
  } else {
    pp
  }
}

#' Plot FPR
#'
#' Plot observed false positive rate (FPR) for given adjusted p-value
#' thresholds.
#'
#' @param cobraplot A \code{COBRAPlot} object.
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
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status", aspects = "fpr")
#' cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2",
#'                                    incltruth = TRUE)
#' plot_fpr(cobraplot, xaxisrange = c(0, 0.25))
plot_fpr <- function(cobraplot, title = "", stripsize = 15, titlecol = "black",
                     pointsize = 5, xaxisrange = c(0, 1)) {
  plot_fpr_tpr(cobraplot = cobraplot, title = title, stripsize = stripsize,
               titlecol = titlecol, pointsize = pointsize,
               xaxisrange = xaxisrange, aspc = "FPR")
}

#' Plot TPR
#'
#' Plot observed true positive rate (TPR) for given adjusted p-value thresholds.
#'
#' @param cobraplot A \code{COBRAPlot} object.
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
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status", aspects = "tpr")
#' cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2",
#'                                    incltruth = TRUE)
#' plot_tpr(cobraplot)
plot_tpr <- function(cobraplot, title = "", stripsize = 15, titlecol = "black",
                     pointsize = 5, xaxisrange = c(0, 1)) {
  plot_fpr_tpr(cobraplot = cobraplot, title = title, stripsize = stripsize,
               titlecol = titlecol, pointsize = pointsize,
               xaxisrange = xaxisrange, aspc = "TPR")
}

#' Plot correlations
#'
#' Plot correlations between observations and a continuous truth value.
#'
#' @param cobraplot A \code{COBRAPlot} object.
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
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example, cont_truth = "logFC",
#'                                    aspects = "corr")
#' cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2",
#'                                    incltruth = TRUE)
#' plot_corr(cobraplot, corrtype = "spearman")
plot_corr <- function(cobraplot, title = "", stripsize = 15, titlecol = "black",
                      pointsize = 5, xaxisrange = c(-1, 1),
                      corrtype = "pearson") {
  plot_fpr_tpr(cobraplot = cobraplot, title = title, stripsize = stripsize,
               titlecol = titlecol, pointsize = pointsize,
               xaxisrange = xaxisrange, aspc = toupper(corrtype))
}

## ---------------------- ROC or FPC --------------------------------- ##
#' @import ggplot2
plot_roc_fpc <- function(cobraplot, title, stripsize, titlecol, xaxisrange,
                         yaxisrange, maxnfdc, aspc, linewidth) {
  if (aspc == "roc")
    plot_data <- roc(cobraplot)
  else if (aspc == "fpc")
    plot_data <- fpc(cobraplot)
  if (!(isTRUE(facetted(cobraplot)))) {
    plot_data$method <- plot_data$fullmethod
  }
  
  ## Number of colors/linetypes
  nlevs <- length(unique(plot_data$method))
  
  pp <- ggplot(plot_data, aes_string(x = ifelse(aspc == "roc", "FPR", "topN"),
                                     y = ifelse(aspc == "roc", "TPR", "FP"),
                                     group = "method", colour = "method")) +
    geom_path(size = linewidth, aes_string(linetype = "method")) +
    scale_linetype_manual(values = rep("solid", nlevs), guide = FALSE) + 
    scale_color_manual(values = plotcolors(cobraplot), name = "") +
    plot_theme(stripsize = stripsize, titlecol = titlecol) +
    xlim(ifelse(aspc == "roc", xaxisrange[1], 0),
         ifelse(aspc == "roc", xaxisrange[2], maxnfdc)) +
    ylim(ifelse(aspc == "roc", yaxisrange[1], 0),
         ifelse(aspc == "roc", yaxisrange[2],
                max(plot_data$FP[plot_data$topN <= maxnfdc]))) +
    ggtitle(title)
  if (isTRUE(facetted(cobraplot))) {
    if (!is.finite(maxsplit(cobraplot)))
      msp <- length(unique(plot_data$splitval))
    else
      msp <- maxsplit(cobraplot)
    pp + facet_wrap(~ splitval, nrow = ceiling((msp + 1)/3))
  } else {
    pp
  }
}

#' Plot ROC curves
#'
#' Plot receiver operating characteristics (ROC) curves.
#'
#' @param cobraplot A \code{COBRAPlot} object.
#' @param title A character string giving the title of the plot.
#' @param stripsize A numeric value giving the size of the strip text, when the
#'   results are stratified by an annotation.
#' @param titlecol A character string giving the color of the title.
#' @param xaxisrange A numeric vector with two elements, giving the lower and
#'   upper boundary of the x-axis, respectively.
#' @param yaxisrange A numeric vector with two elements, giving the lower and
#'   upper boundary of the y-axis, respectively.
#' @param linewidth The line width used for plotting
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#' @author Charlotte Soneson
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status", aspects = "roc")
#' cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2",
#'                                    incltruth = TRUE)
#' plot_roc(cobraplot)
plot_roc <- function(cobraplot, title = "", stripsize = 15, titlecol = "black",
                     xaxisrange = c(0, 1), yaxisrange = c(0, 1), 
                     linewidth = 1) {
  plot_roc_fpc(cobraplot = cobraplot, title = title, stripsize = stripsize,
               titlecol = titlecol, xaxisrange = xaxisrange,
               yaxisrange = yaxisrange, maxnfdc = NULL, aspc = "roc",
               linewidth = linewidth)
}

#' Plot FP curves
#'
#' Plot false positive curves, indicating the number of false positives among
#' the top-ranked N variables, for varying values of N.
#'
#' @param cobraplot A \code{COBRAPlot} object.
#' @param title A character string giving the title of the plot.
#' @param stripsize A numeric value giving the size of the strip text, when the
#'   results are stratified by an annotation.
#' @param titlecol A character string giving the color of the title.
#' @param maxnfdc A numeric value giving the largest N to consider.
#' @param linewidth The line width used for plotting
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#' @author Charlotte Soneson
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status", aspects = "fpc")
#' cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2",
#'                                    incltruth = TRUE)
#' plot_fpc(cobraplot, maxnfdc = 750)
plot_fpc <- function(cobraplot, title = "", stripsize = 15, titlecol = "black",
                     maxnfdc = 500, linewidth = 1) {
  plot_roc_fpc(cobraplot = cobraplot, title = title, stripsize = stripsize,
               titlecol = titlecol, xaxisrange = NULL, yaxisrange = NULL,
               maxnfdc = maxnfdc, aspc = "fpc", linewidth = linewidth)
}

## ------------------------- SCATTER --------------------------------- ##
#' Plot scatter plots
#'
#' Plot scatter plots, indicating the relationship between observed values and a
#' continuous truth.
#'
#' @param cobraplot A \code{COBRAPlot} object.
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
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example, cont_truth = "logFC",
#'                                    aspects = "scatter")
#' cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2",
#'                                    incltruth = TRUE)
#' plot_scatter(cobraplot)
plot_scatter <- function(cobraplot, title = "", stripsize = 10,
                         titlecol = "black", pointsize = 3, doflip = FALSE,
                         dolog = FALSE) {
  plot_data <- scatter(cobraplot)

  if (isTRUE(dolog)) {
    plot_data$OBSERVATION[which(plot_data$OBSERVATION < 1e-305)] <- 0
    plot_data$TRUTH[which(plot_data$TRUTH < 1e-305)] <- 0
  }

  if (isTRUE(facetted(cobraplot))) {
    plot_data$fullmethod <- plot_data$method
  }
  pp <- ggplot(plot_data, aes_string(x = "OBSERVATION", y = "TRUTH",
                                     colour = "fullmethod")) +
    geom_point(size = pointsize) +
    scale_color_manual(values = plotcolors(cobraplot), name = "") +
    plot_theme(stripsize = stripsize, titlecol = titlecol) +
    ggtitle(title)
  if (isTRUE(facetted(cobraplot))) {
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
plot_fdrcurve <- function(cobraplot, title, stripsize, titlecol, pointsize,
                          xaxisrange, yaxisrange, plottype, aspc, linewidth) {
  if (aspc == "TPR") {
    plot_data_lines <- fdrtprcurve(cobraplot)
    plot_data_points <- fdrtpr(cobraplot)
    xasp <- "FDR"
    yasp <- aspc
  } else if (aspc == "NBR") {
    plot_data_lines <- fdrnbrcurve(cobraplot)
    plot_data_points <- fdrnbr(cobraplot)
    xasp <- "FDR"
    yasp <- aspc
  } else if (aspc == "FSR") {
    plot_data_lines <- fsrnbrcurve(cobraplot)
    plot_data_points <- fsrnbr(cobraplot)
    xasp <- "FSR"
    yasp <- "NBR"
  }
  
  if ("curve" %in% plottype && length(plot_data_lines) == 0) {
    message("The provided 'plottype' argument includes 'curve' but the required values are not calculated. Please include the appropriate aspect in calculate_performance().")
    plottype <- setdiff(plottype, "curve")
  }
  if ("points" %in% plottype && length(plot_data_points) == 0) {
    message("The provided 'plottype' argument includes 'points' but the required values are not calculated. Please include the appropriate aspect in calculate_performance().")
    plottype <- setdiff(plottype, "points")
  }
  pp <- ggplot()

  thresholds <- sort(unique(as.numeric(gsub("thr", "", plot_data_points$thr))))
  plot_data_points$method2.satis <- paste0(plot_data_points$method,
                                           plot_data_points$satis)

  if (!(isTRUE(facetted(cobraplot)))) {
    plot_data_points$method <- plot_data_points$fullmethod
    plot_data_lines$method <- plot_data_lines$fullmethod
    plot_data_points$method2.satis <- paste0(plot_data_points$fullmethod,
                                             plot_data_points$satis)
  }

  ## Number of colors/linetypes
  nlevs <- length(unique(plot_data_lines$method))
  
  if ("curve" %in% plottype && "points" %in% plottype) {
    pp <- ggplot(plot_data_lines, aes_string(x = xasp, y = yasp,
                                             group = "method",
                                             colour = "method")) +
      geom_vline(xintercept = seq(0, xaxisrange[2], 0.1),
                 colour = "lightgrey", linetype = "dashed") +
      geom_vline(xintercept = thresholds, linetype = "dashed") +
      geom_path(size = linewidth, aes_string(linetype = "method")) +
      scale_linetype_manual(values = rep("solid", nlevs), guide = FALSE) + 
      geom_point(data = plot_data_points, size = pointsize + 1,
                 aes_string(colour = "method"), shape = 19) +
      geom_point(data = plot_data_points, size = pointsize,
                 aes_string(fill = "method2.satis", colour = "method"),
                 shape = 21) +
      scale_fill_manual(values = plotcolors(cobraplot), guide = FALSE,
                        name = "") +
      scale_color_manual(values = plotcolors(cobraplot), name = "") +
      ylim(ifelse(yasp == "TPR", yaxisrange[1], 0),
           ifelse(yasp == "TPR", yaxisrange[2],
                  max(c(0, plot_data_lines[[yasp]][plot_data_lines[[xasp]] <=
                                                     xaxisrange[2]])))) +
      scale_x_continuous(breaks = c(thresholds,
                                    seq(0, xaxisrange[2], 0.1)),
                         labels = c(thresholds, "", 
                                    seq(0, xaxisrange[2], 0.1)[-1]),
                         limits = c(xaxisrange[1], xaxisrange[2])) +
      plot_theme(stripsize = stripsize, titlecol = titlecol) +
      ggtitle(title)
  } else if ("curve" %in% plottype) {
    pp <- ggplot(plot_data_lines,
                 aes_string(x = xasp, y = yasp,
                            group = "method", colour = "method")) +
      geom_path(size = linewidth, aes_string(linetype = "method")) +
      scale_linetype_manual(values = rep("solid", nlevs), guide = FALSE) + 
      xlim(xaxisrange[1], xaxisrange[2]) +
      ylim(ifelse(yasp == "TPR", yaxisrange[1], 0),
           ifelse(yasp == "TPR", yaxisrange[2],
                  max(c(0, plot_data_lines[[yasp]][plot_data_lines[[xasp]] <=
                                                     xaxisrange[2]])))) +
      scale_color_manual(values = plotcolors(cobraplot), name = "") +
      plot_theme(stripsize = stripsize, titlecol = titlecol) +
      ggtitle(title)
  } else if ("points" %in% plottype) {
    pp <- ggplot(plot_data_points, aes_string(x = xasp, y = yasp,
                                              group = "method")) +
      geom_vline(xintercept = seq(0, xaxisrange[2], 0.1),
                 colour = "lightgrey", linetype = "dashed") +
      geom_vline(xintercept = thresholds, linetype = "dashed") +
      geom_path(size = linewidth, aes_string(colour = "method", linetype = "method")) +
      scale_linetype_manual(values = rep("solid", nlevs), guide = FALSE) + 
      geom_point(size = pointsize + 1,
                 aes_string(colour = "method"), shape = 19) +
      geom_point(size = pointsize,
                 aes_string(fill = "method2.satis", colour = "method"),
                 shape = 21) +
      scale_fill_manual(values = plotcolors(cobraplot), guide = FALSE,
                        name = "") +
      scale_color_manual(values = plotcolors(cobraplot), name = "") +
      ylim(ifelse(yasp == "TPR", yaxisrange[1], 0),
           ifelse(yasp == "TPR", yaxisrange[2],
                  max(c(0, plot_data_lines[[yasp]][plot_data_lines[[xasp]] <=
                                                     xaxisrange[2]])))) +
      scale_x_continuous(breaks = c(thresholds,
                                    seq(0, xaxisrange[2], 0.1)),
                         labels = c(thresholds, "", 
                                    seq(0, xaxisrange[2], 0.1)[-1]),
                         limits = c(xaxisrange[1], xaxisrange[2])) +
      plot_theme(stripsize = stripsize, titlecol = titlecol) +
      ggtitle(title)
  }
  if (isTRUE(facetted(cobraplot))) {
    if (!is.finite(maxsplit(cobraplot))) {
      if (length(plot_data_lines) != 0)
        msp <- length(unique(plot_data_lines$splitval))
      else
        msp <- length(unique(plot_data_points$splitval))
    } else {
      msp <- maxsplit(cobraplot)
    }
    pp + facet_wrap(~ splitval, nrow = ceiling((msp + 1)/3))
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
#' @param cobraplot A \code{COBRAPlot} object.
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
#' @param linewidth The line width used for plotting
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#' @author Charlotte Soneson
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = c("fdrtpr", "fdrtprcurve"))
#' cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2",
#'                                    incltruth = TRUE)
#' plot_fdrtprcurve(cobraplot, plottype = c("curve", "points"))
plot_fdrtprcurve <- function(cobraplot, title = "", stripsize = 15,
                             titlecol = "black", pointsize = 5,
                             xaxisrange = c(0, 1), yaxisrange = c(0, 1),
                             plottype = c("curve", "points"),
                             linewidth = 1) {
  plot_fdrcurve(cobraplot = cobraplot, title = title, stripsize = stripsize,
                titlecol = titlecol, pointsize = pointsize,
                xaxisrange = xaxisrange, yaxisrange = yaxisrange,
                plottype = plottype, aspc = "TPR", linewidth = linewidth)
}

#' Plot number of significant features vs FDR
#'
#' Plot the number of features considered significant vs observed false
#' discovery rate (FDR), for given adjusted p-value thresholds and/or as curves
#' traced out by considering all threshold values.
#'
#' @param cobraplot A \code{COBRAPlot} object.
#' @param title A character string giving the title of the plot.
#' @param stripsize A numeric value giving the size of the strip text, when the
#'   results are stratified by an annotation.
#' @param titlecol A character string giving the color of the title.
#' @param pointsize A numeric value giving the size of the plot characters.
#' @param xaxisrange A numeric vector with two elements, giving the lower and
#'   upper boundary of the x-axis, respectively.
#' @param plottype A character vector giving the type of plot to construct. Can
#'   be any combination of the two elements "curve" and "points".
#' @param linewidth The line width used for plotting
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#' @author Charlotte Soneson
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = c("fdrnbr", "fdrnbrcurve"))
#' cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2",
#'                                    incltruth = TRUE)
#' plot_fdrnbrcurve(cobraplot, plottype = c("curve", "points"))
plot_fdrnbrcurve <- function(cobraplot, title = "", stripsize = 15,
                             titlecol = "black", pointsize = 5,
                             xaxisrange = c(0, 1),
                             plottype = c("curve", "points"),
                             linewidth = 1) {
  plot_fdrcurve(cobraplot = cobraplot, title = title, stripsize = stripsize,
                titlecol = titlecol, pointsize = pointsize,
                xaxisrange = xaxisrange, yaxisrange = NULL,
                plottype = plottype, aspc = "NBR",
                linewidth = linewidth)
}

#' Plot number of features with s-value below threshold vs FSR
#'
#' Plot the number of features with an s-value below a threshold vs the observed
#' false sign rate (FSR), for given adjusted p-value thresholds and/or as curves
#' traced out by considering all threshold values.
#'
#' @param cobraplot A \code{COBRAPlot} object.
#' @param title A character string giving the title of the plot.
#' @param stripsize A numeric value giving the size of the strip text, when the
#'   results are stratified by an annotation.
#' @param titlecol A character string giving the color of the title.
#' @param pointsize A numeric value giving the size of the plot characters.
#' @param xaxisrange A numeric vector with two elements, giving the lower and
#'   upper boundary of the x-axis, respectively.
#' @param plottype A character vector giving the type of plot to construct. Can
#'   be any combination of the two elements "curve" and "points".
#' @param linewidth The line width used for plotting
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#' @author Charlotte Soneson
#' @examples
#' data(cobradata_example_sval)
#' cobraperf <- calculate_performance(cobradata_example_sval,
#'                                    cont_truth = "logFC",
#'                                    aspects = c("fsrnbr", "fsrnbrcurve"))
#' cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2",
#'                                    incltruth = TRUE)
#' plot_fsrnbrcurve(cobraplot, plottype = c("curve", "points"))
plot_fsrnbrcurve <- function(cobraplot, title = "", stripsize = 15,
                             titlecol = "black", pointsize = 5,
                             xaxisrange = c(0, 1),
                             plottype = c("curve", "points"),
                             linewidth = 1) {
  plot_fdrcurve(cobraplot = cobraplot, title = title, stripsize = stripsize,
                titlecol = titlecol, pointsize = pointsize,
                xaxisrange = xaxisrange, yaxisrange = NULL,
                plottype = plottype, aspc = "FSR",
                linewidth = linewidth)
}

## ------------------------ OVERLAP ---------------------------------- ##
#' Plot Venn diagram
#'
#' Plot a Venn diagram showing the overlaps among sets of significant feature
#' for a given adjusted p-value threshold. Optionally, the truth can be included
#' as a "perfect" method. Note that maximally five methods (including the truth,
#' if applicable) can be compared.
#'
#' @param cobraplot A \code{COBRAPlot} object.
#' @param ... Additional arguments to \code{limma::vennDiagram}.
#'
#' @return Nothing, displays a graph
#'
#' @export
#' @import limma
#' @author Charlotte Soneson
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "overlap")
#' cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2",
#'                                    incltruth = TRUE)
#' plot_overlap(cobraplot)
plot_overlap <- function(cobraplot, ...) {
  overlap_table <- overlap(cobraplot)
  circle.col <- plotcolors(cobraplot)
  if (length(overlap_table) == 0)
    return(NULL)

  if (class(overlap_table) != "list") {
    if (ncol(overlap_table) < 6) {
      if (ncol(overlap_table) == 5)
        cols <- rep(circle.col[colnames(overlap_table)], 2)[2:6]
      else
        cols <- circle.col[colnames(overlap_table)]
      vennDiagram(overlap_table, circle.col = cols, ...)
    } else {
      NULL
    }
  } else {
    ncl <- ceiling(sqrt(length(overlap_table)))
    nrw <- ceiling(length(overlap_table)/ncl)
    graphics::par(mfrow = c(nrw, ncl), mar = c(4, 1, 1, 1))
    for (i in 1:length(overlap_table)) {
      if (ncol(overlap_table[[i]]) < 6) {
        if (ncol(overlap_table[[i]]) == 5)
          cols <- rep(circle.col[colnames(overlap_table[[i]])], 2)[2:6]
        else
          cols <- circle.col[colnames(overlap_table[[i]])]
        vennDiagram(overlap_table[[i]], circle.col = cols,
                    main = paste0(splv(cobraplot), ":",
                                  names(overlap_table)[i]), ...)
      } else {
        NULL
      }
    }
    graphics::par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
  }
}

#' Create UpSet plots
#'
#' Generate UpSet plots showing the overlaps among sets of significant feature
#' for a given adjusted p-value threshold. Optionally, the truth can be included
#' as a "perfect" method. Note that if the results are stratified, only one 
#' category at a time can be displayed.
#'
#' @param cobraplot A \code{COBRAPlot} object.
#' @param stratum If results are stratified, the category to plot results for.
#'   Can be numeric or categorical (the name of the category).
#' @param nsets The number of methods to include. By default, it is determined
#'   automatically from the \code{cobraplot} object.
#' @param nintersects The number of set intersections to display. By default, it
#'   is determined automatically from the \code{cobraplot} object.
#' @param sets.bar.color The colors to use for the bars in the UpSet plot. By
#'   default, they are extracted from the \code{plotcolors} slot of the
#'   \code{cobraplot} object.
#' @param ... Additional arguments to \code{UpSetR::upset}.
#'
#' @return Nothing, displays a graph
#' @references 
#' Lex and Gehlenborg (2014): Points of view: Sets and intersections. Nature
#' Methods 11, 779.
#' 
#' Lex et al (2014): UpSet: Visualization of intersecting sets. IEEE
#' Transactions on Visualization and Computer Graphics 20(12), 1983-1992.
#' @export
#' @import UpSetR
#' @author Charlotte Soneson
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example,
#'                                    binary_truth = "status",
#'                                    aspects = "overlap")
#' cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2",
#'                                    incltruth = TRUE)
#' plot_upset(cobraplot)
#' plot_upset(cobraplot, order.by = "freq", decreasing = TRUE)
#' 
#' cobraperf <- calculate_performance(cobradata_example, 
#'                                    binary_truth = "status", 
#'                                    aspects = "overlap",
#'                                    splv = "expr_cat")
#' cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2", 
#'                                    incltruth = TRUE)
#' plot_upset(cobraplot, stratum = "[2.85e+00,1.45e+01)")
plot_upset <- function(cobraplot, stratum = NULL, nsets = NULL, 
                       nintersects = NULL, sets.bar.color = NULL, ...) {
  overlap_table <- overlap(cobraplot)
  if (length(overlap_table) == 0)
    return(NULL)
  
  if (class(overlap_table) != "list") {
    plotorder <- colnames(overlap_table)[order(colSums(overlap_table), 
                                               seq(1:ncol(overlap_table)),
                                               decreasing = "true")]
    if (all(colSums(overlap_table) == 0)) return(NULL)
    if (is.null(nsets)) nsets <- ncol(overlap_table)
    if (is.null(nintersects)) nintersects <- 2^(ncol(overlap_table)) - 1
    if (is.null(sets.bar.color)) sets.bar.color <- plotcolors(cobraplot)[plotorder]
    upset(overlap_table, nsets = nsets, nintersects = nintersects, 
          sets.bar.color = sets.bar.color, ...)
  } else {
    if (is.null(stratum)) stop("You must provide a stratum")
    plotorder <- 
      colnames(overlap_table[[stratum]])[order(colSums(overlap_table[[stratum]]), 
                                               seq(1:ncol(overlap_table[[stratum]])),
                                               decreasing = "true")]
    if (all(colSums(overlap_table[[stratum]]) == 0)) return(NULL)
    if (is.null(nsets)) nsets <- ncol(overlap_table[[stratum]])
    if (is.null(nintersects)) nintersects <- 2^(ncol(overlap_table[[stratum]])) - 1
    if (is.null(sets.bar.color)) sets.bar.color <- plotcolors(cobraplot)[plotorder]
    upset(overlap_table[[stratum]], nsets = nsets, nintersects = nintersects,
          sets.bar.color = sets.bar.color, ...)
  }
}

## -------------------------- Deviation ------------------------------ ##
#' Plot deviations
#'
#' Plot the deviations between observed scores and the continuous truth
#' variable.
#'
#' @param cobraplot A \code{COBRAPlot} object.
#' @param title A character string giving the title of the plot.
#' @param stripsize A numeric value giving the size of the strip text, when the
#'   results are stratified by an annotation.
#' @param titlecol A character string giving the color of the title.
#' @param xaxisrange A numeric vector with two elements, giving the lower and
#'   upper boundary of the x-axis, respectively.
#' @param plottype Either "boxplot" or "violinplot", indicating what type of
#'   plot to make.
#' @param dojitter A logical indicating whether to include jittered data points
#'   or not.
#' @param transf A character indicating the transformation to apply to the
#'   deviations before plotting. Must be one of "raw", "absolute" or "squared"
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#' @author Charlotte Soneson
#' @examples
#' data(cobradata_example)
#' cobraperf <- calculate_performance(cobradata_example, cont_truth = "logFC",
#'                                    aspects = "deviation")
#' cobraplot <- prepare_data_for_plot(cobraperf, colorscheme = "Dark2",
#'                                    incltruth = TRUE)
#' plot_deviation(cobraplot)
plot_deviation <- function(cobraplot, title = "", stripsize = 15,
                           titlecol = "black", xaxisrange = NULL,
                           plottype = "boxplot",
                           dojitter = TRUE, transf = "raw") {
  stopifnot(transf %in% c("raw", "absolute", "squared"))
  plot_data <- deviation(cobraplot)
  if (transf == "absolute")
    plot_data$absDEVIATION <- abs(plot_data$DEVIATION)
  else if (transf == "squared")
    plot_data$sqDEVIATION <- plot_data$DEVIATION^2

  if (!(isTRUE(facetted(cobraplot)))) {
    plot_data$method <- plot_data$fullmethod
  }

  pp <- ggplot(
    plot_data, aes_string(x = "method",
                          y = ifelse(transf == "raw", "DEVIATION",
                                     ifelse(transf == "absolute",
                                            "absDEVIATION", "sqDEVIATION")),
                          group = "method", colour = "method")) +
    coord_flip() +
    scale_color_manual(values = plotcolors(cobraplot), name = "") +
    plot_theme(stripsize = stripsize, titlecol = titlecol) +
    ggtitle(title)
  if (plottype == "boxplot") {
    if (isTRUE(dojitter))
      pp <- pp + geom_boxplot(outlier.size = 0)
    else
      pp <- pp + geom_boxplot()
  }
  else if (plottype == "violinplot")
    pp <- pp + geom_violin()
  if (isTRUE(dojitter))
    pp <- pp + geom_jitter(position = position_jitter(width = 0.1, height = 0),
                           size = 1.5)
  if (isTRUE(facetted(cobraplot))) {
    if (!is.finite(maxsplit(cobraplot)))
      msp <- length(unique(plot_data$splitval))
    else
      msp <- maxsplit(cobraplot)
    pp <- pp + facet_wrap(~ splitval,
                          nrow = ceiling((msp + 1)/3))
  }
  if (!is.null(xaxisrange))
    pp <- pp + ylim(xaxisrange[1], xaxisrange[2])
  pp
}

