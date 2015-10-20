## Function to perform some checks on the input (result) files.
## Return TRUE if everything seems ok,
## otherwise print a message and return FALSE.
res_check <- function(dfr) {
  if (ncol(dfr) == 1) {
    print("The data frame contains only one column")
    return(FALSE)
  }
  return(TRUE)
}

get_keepfeatures <- function(truth, df, method, colm, onlyshared) {
  if (!isTRUE(onlyshared)) {
    allg <- rownames(truth[which(!is.na(truth[, colm])), , drop = FALSE])
  } else {
    allg <- intersect(rownames(truth[which(!is.na(truth[, colm])), ,
                                     drop = FALSE]),
                      rownames(df)[which(!is.na(df[method]))])
  }
  allg
}

is_plottable <- function(obj) {
  if (is.null(obj))
    FALSE
  else if (class(obj) == "data.frame" && nrow(obj) == 0)
    FALSE
  else if (length(obj) == 0)
    FALSE
  else
    TRUE
}

#' @import dplyr
get_keeplevels <- function(truth, splv, binary_truth, maxsplit) {
  if (splv != "none") {
    if (!is.null(binary_truth)) {
      nbrtrulydiff <-
        as.data.frame(truth %>% group_by_(splv) %>%
                        summarise_(nbrdiff = paste0("length(which(",
                                                    binary_truth, "== 1))")),
                      stringsAsFactors = FALSE)
      tokeep <- nbrtrulydiff[nbrtrulydiff$nbrdiff > 0, splv]
      tbl <- table(truth[[splv]])
      tbl <- tbl[as.character(tokeep)]
    } else {
      tbl <- table(truth[[splv]])
    }
    tbl <- sort(tbl, decreasing = TRUE)
    keeplevels <- names(tbl)[1:min(maxsplit, length(tbl))]
  } else {
    keeplevels <- "overall"
  }
  keeplevels
}

plot_theme <- function(stripsize, titlecol) {
  theme_grey() +
    theme(legend.position = "right",
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.text = element_text(size = stripsize),
          strip.background = element_rect(fill = NA, colour = "black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5,
                                     hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(colour = titlecol))
}

get_coltype <- function(col_name) {
  if (substr(col_name, nchar(col_name) - 1, nchar(col_name)) == ":P") {
    coltype <- "pval"
  } else if (substr(col_name, nchar(col_name) - 4,
                    nchar(col_name)) == ":adjP") {
    coltype <- "padj"
  } else if (substr(col_name, nchar(col_name) - 5,
                    nchar(col_name)) == ":score") {
    coltype <- "score"
  } else {
    coltype <- NULL
  }
  coltype
}

fix_duplicates <- function(res_df, feature_id, method_name) {
  dp <-
    unique(as.character(res_df[, feature_id][duplicated(res_df[, feature_id])]))
  if (length(dp) > 0) {
    print(paste0("Found duplicate gene name(s) for ", method_name))
    for (j in dp) {
      idx <- which(res_df[, feature_id] == j)
      kp <- which.min(res_df[idx, method_name])[1]
      if (is.na(kp)) kp <- 1
      rem <- idx[-kp]
      res_df <- res_df[-rem, , drop = FALSE]
    }
    if (sum(duplicated(res_df[, feature_id])) == 0)
      print("Fixed the duplication!")
  }
  res_df
}

#' Calculate adjusted p-values
#'
#' Calculate adjusted p-values for methods where only nominal p-values are
#' available in an \code{IBRAData} object.
#'
#' @param ibradata An \code{IBRAData} object.
#' @param method A character string giving the method (selected from
#'   \code{p.adjust.methods()}) that will be used to perform the adjustment.
#'
#' @return An \code{IBRAData} object, extended with the calculated adjusted
#'   p-values.
#'
#' @export
#' @author Charlotte Soneson
#' @examples
#' data(ibradata_example)
#' ibradata_example <- calculate_adjp(ibradata_example, method = "BH")
calculate_adjp <- function(ibradata, method = "BH") {
  sf <- setdiff(colnames(pval(ibradata)), colnames(padj(ibradata)))
  if (length(sf) > 0) {
      pa <- as.data.frame(apply(pval(ibradata)[, sf, drop = FALSE], 2,
                                stats::p.adjust, method = method))
      missing_genes <- setdiff(rownames(pa), rownames(padj(ibradata)))
      if (length(padj(ibradata)) != 0) {
        tmpadd <-
          as.data.frame(matrix(NA, length(missing_genes),
                               ncol(padj(ibradata)),
                               dimnames = list(missing_genes,
                                               colnames(padj(ibradata)))))
        padj(ibradata) <- rbind(padj(ibradata), tmpadd)
        padj(ibradata) <- cbind(padj(ibradata),
                                pa[match(rownames(padj(ibradata)),
                                         rownames(pa)), , drop = FALSE])
      } else {
        padj(ibradata) <- pa
      }
  }
  ibradata
}

fixcolname <- function(df, prevv, newv) {
  idx <- which(colnames(df) == prevv)
  if (length(idx) > 0)
    colnames(df)[idx] <- newv
  df
}

define_colors <- function(ibraperf, palette, facetted, incloverall) {
  levs <- basemethod <- NULL

  basem <- basemethods(ibraperf)
  strl <- paste0("_", stratiflevels(ibraperf))
  inp_methods <- expand.grid(basemethod = basem, levs = c("", strl))
  inp_methods$fullmethod <- paste0(inp_methods$basemethod, inp_methods$levs)

  if (!isTRUE(incloverall))
    inp_methods <- subset(inp_methods, levs != "overall")

  ## Calculate number of required colors
  if (facetted == TRUE) {
    tmp_methods <- unique(inp_methods$basemethod)
    ncolors <- length(unique(tmp_methods))
  } else {
    tmp_methods <- unique(subset(inp_methods, levs != "" &
                                   basemethod != "truth")$fullmethod)
    if ("truth" %in% inp_methods$basemethod)
      tmp_methods <- c(tmp_methods, "truth")
    ncolors <- length(tmp_methods)
  }

  ## Define colors
  if (length(palette) > 1) {
    ## User-specified color vector
    if (length(palette) > ncolors) {
      warning(paste0("Too many colors supplied. Only the first ", ncolors,
                     " will be used."))
      use_colors1 <- palette[1:ncolors]
    } else if (length(palette) < ncolors) {
      warning(paste0("Too few colors provided. ", ncolors - length(palette),
                     " random colors will be added."))
      use_colors1 <- c(palette,
                       setdiff(sample(grDevices::colors(),
                                      length(grDevices::colors())),
                               c("white",
                                 palette))[1:(ncolors - length(palette))])
    } else {
      use_colors1 <- palette
    }
  } else {
    bnm <- strsplit(palette, " \\(")[[1]][1]
    ## Check that the palette is valid, otherwise consider it a color
    if (!(bnm %in% c("hue_pal", "rainbow", "heat", "terrain", "topo",
                     "cm", "Accent", "Dark2", "Paired", "Pastel1",
                     "Pastel2", "Set1", "Set2", "Set3"))) {
      warning("Invalid palette, will consider it a color.")
      use_colors1 <- c(palette,
                       setdiff(sample(grDevices::colors(),
                                      length(grDevices::colors())),
                               c("white",
                                 palette))[1:(ncolors - length(palette))])
    } else {
      ## Pre-defined palette
      maxnbr <- c(Accent = 8, Dark2 = 8, Paired = 12, Pastel1 = 9,
                  Pastel2 = 8, Set1 = 9, Set2 = 8, Set3 = 12)
      if (palette == "hue_pal") use_colors1 <- scales::hue_pal()(ncolors)
      else if (palette == "rainbow") use_colors1 <- grDevices::rainbow(ncolors)
      else if (palette == "heat") use_colors1 <- grDevices::heat.colors(ncolors)
      else if (palette == "terrain") use_colors1 <-
          grDevices::terrain.colors(ncolors)
      else if (palette == "topo") use_colors1 <- grDevices::topo.colors(ncolors)
      else if (palette == "cm") use_colors1 <- grDevices::cm.colors(ncolors)
      else {
        if (ncolors <= maxnbr[bnm])
          use_colors1 <- scales::brewer_pal(palette = bnm)(ncolors)
        else
          use_colors1 <- scales::hue_pal()(ncolors)
      }
    }
  }
  names(use_colors1) <- tmp_methods

  if (facetted == TRUE) {
    inp_methods$color <- use_colors1[match(inp_methods$basemethod,
                                           names(use_colors1))]
  } else {
    inp_methods$color <- use_colors1[match(inp_methods$fullmethod,
                                           names(use_colors1))]
    inp_methods$color[inp_methods$basemethod == "truth"] <-
      use_colors1["truth"]
    if ("_overall" %in% inp_methods$levs) {
      for (m in unique(inp_methods$basemethod)) {
        inp_methods$color[inp_methods$fullmethod == m] <-
          inp_methods$color[inp_methods$fullmethod == paste0(m, "_overall")]
      }
    }
  }

  use_colors1 <- inp_methods$color
  names(use_colors1) <- inp_methods$fullmethod

  ## Get colors for "FDR satisfied/not satisfied" (white or method color)
  use_colors2 <- c(use_colors1, rep("white", length(use_colors1)))
  names(use_colors2) <- c(paste0(names(use_colors1), "yes"),
                          paste0(names(use_colors1), "no"))

  use_colors <- c(use_colors1, use_colors2)
  return(use_colors)
}


select_measure <- function(ibradata, method, asp) {
  ret <- NULL
  if (asp %in% c("nbr", "fpr", "fdr", "tpr")) {
    if (method %in% names(padj(ibradata))) ret <- "padj"
    else ret <- NULL
  } else if (asp %in% c("fdrtpr", "fdrnbr")) {
    if (method %in% names(score(ibradata))) {
      if (method %in% names(padj(ibradata))) {
        tmp1 <- padj(ibradata)[method]
        tmp2 <- score(ibradata)[method]
        nm <- intersect(rownames(tmp1), rownames(tmp2))
        tmp1 <- tmp1[match(nm, rownames(tmp1)), method]
        tmp2 <- tmp2[match(nm, rownames(tmp2)), method]
        sgn <- sign(diff(tmp1[order(tmp2)]))
        if (length(unique(sgn[sgn != 0 & !is.na(sgn)])) == 1) ret <- "score"
        else ret <- "padj"
      } else ret <- "score"
    } else if (method %in% names(pval(ibradata))) {
      if (method %in% names(padj(ibradata))) {
        tmp1 <- padj(ibradata)[method]
        tmp2 <- pval(ibradata)[method]
        nm <- intersect(rownames(tmp1), rownames(tmp2))
        tmp1 <- tmp1[match(nm, rownames(tmp1)), method]
        tmp2 <- tmp2[match(nm, rownames(tmp2)), method]
        sgn <- sign(diff(tmp1[order(tmp2)]))
        if (length(unique(sgn[sgn != 0 & !is.na(sgn)])) == 1) ret <- "pval"
        else ret <- "padj"
      } else ret <- "pval"
    } else if (method %in% names(padj(ibradata))) {
      ret <- "padj"
    } else ret <- NULL
  } else if (asp %in% c("roc", "fpc")) {
    if (method %in% names(score(ibradata))) ret <- "score"
    else if (method %in% names(pval(ibradata))) ret <- "pval"
    else if (method %in% names(padj(ibradata))) ret <- "padj"
    else ret <- NULL
  } else if (asp %in% c("corr", "scatter", "deviation")) {
    if (method %in% names(score(ibradata))) ret <- "score"
    else ret <- NULL
  }
  ret
}

extend_resulttable <- function(df, splv, keeplevels, valuename,
                               basemethod, domelt = TRUE) {
  if (isTRUE(domelt)) {
    df <- reshape2::melt(df, varnames = c("thr", "method"),
                         value.name = valuename)
    df <- fixcolname(df, prevv = "value", newv = valuename)
  }
  df$basemethod <- basemethod[match(df$method, names(basemethod))]
  df$meas <- sapply(df$method, function(w) {
    a <- strsplit(as.character(w), "__")[[1]]
    paste0("__", a[length(a)])
  })
  df$method <- gsub("__padj$", "", gsub("__pval$", "",
                                        gsub("__score", "", df$method)))

  df$fullmethod <- df$method
  if (splv == "none") {
    df <- droplevels(df[grep("_overall", df$method), ])
    df$splitval <- "overall"
    df$method <- sapply(as.character(df$fullmethod), function(w) {
      gsub("_overall", "", w)
    })
  } else {
    df$splitval <-
      factor(sapply(as.character(df$fullmethod),
                    function(w) {
                      if (length(grep("overall", w)) > 0) {
                        "overall"
                      } else {
                        a <- unlist(strsplit(w, paste0("_", splv, ":")))
                        paste0(splv, ":", a[2])
                      }
                    }), levels = c("overall", paste0(splv, ":", keeplevels)))

    df$method <-
      sapply(as.character(df$fullmethod),
             function(w) {
               if (length(grep("_overall$", w)) > 0) {
                 unlist(strsplit(w, "_overall$")[1])
               } else {
                 a <- unlist(strsplit(w, paste0("_", splv, ":")))
                 a[1]
               }
             })
  }
  df
}

fix_res <- function(res, methodcol, aspcts, tabtype = "large") {
  res$dist_ <- round(res$dist_, 1)
  if (tabtype != "scatter") {
    if (!("thr" %in% colnames(res))) {
      um <- unique(res[, methodcol])
      res <- res[match(um, res[, methodcol]), ]
    }
  }
  idx <- match(c("thr", methodcol, "dist_", "NBR", "CUTOFF", "FPC_CUTOFF",
                 "ROC_CUTOFF", "topN", "OBSERVATION", "TRUTH"), colnames(res))
  colnames(res)[idx[!is.na(idx)]] <-
    c("Threshold", "Method", "Distance from cursor", "Number of detections",
      "Cutoff", "Cutoff", "Cutoff", "Number of detections", "observation",
      "truth")[!is.na(idx)]

  if ("Threshold" %in% colnames(res)) {
    res$Threshold <- gsub("thr", "", res$Threshold)
  }
  res$Method_m <- paste0(res$Method, res$meas)
  if (tabtype == "large") {
    res[, c("TP", "FP", "FN", "TN", "TOT_CALLED", "DIFF", "NONDIFF")] <-
      round(res[, c("TP", "FP", "FN", "TN", "TOT_CALLED", "DIFF", "NONDIFF")])
    DT::datatable(res[, c(ifelse("Threshold" %in% colnames(res),
                                 "Threshold", "Cutoff"),
                          "Method_m", aspcts, "TP", "FP",
                          "FN", "TN", "TOT_CALLED", "DIFF", "NONDIFF",
                          "Distance from cursor")])
  } else if (tabtype == "corr") {
    DT::datatable(res[, c("Method_m", aspcts, "Distance from cursor")])
  } else if (tabtype == "scatter") {
    DT::datatable(res[, c("Method_m", "feature", aspcts,
                          "Distance from cursor")])
  } else if (tabtype == "deviation") {
    res[, "Horizontal distance from cursor"] <-
      res[, "Distance from cursor"]
    DT::datatable(res[, c("Method_m", "feature", aspcts,
                          "Horizontal distance from cursor")])
  } else {
    DT::datatable(res[, c("Method_m", aspcts, "Cutoff",
                          "Distance from cursor")])
  }
}
