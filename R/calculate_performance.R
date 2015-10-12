gen_thr_vec <- function(thresholds) {
  v <- rep(0, length(thresholds))
  names(v) <- paste0("thr", thresholds)
  v
}

get_curve <- function(bintruth, vals, revr, aspc) {
  kg <- intersect(names(bintruth), names(vals[!is.na(vals)]))
  if (any(bintruth[match(kg, names(bintruth))] == 0) &
      any(bintruth[match(kg, names(bintruth))] == 1)) {
    if (aspc %in% c("roc", "fdrtpr")) {
      tpcorr <- length(which(bintruth == 1))/
        length(intersect(names(bintruth)[which(bintruth == 1)], kg))
      fpcorr <- length(which(bintruth == 0))/
        length(intersect(names(bintruth)[which(bintruth == 0)], kg))
    }
    if (isTRUE(revr))
      inpvals <- 1 - vals[match(kg, names(vals))]
    else
      inpvals <- vals[match(kg, names(vals))]
    preds <- ROCR::prediction(predictions = inpvals,
                              labels = bintruth[match(kg, names(bintruth))],
                              label.ordering = c(0, 1))
    if (aspc == "roc")
      mes <- ROCR::performance(preds, measure = "tpr", x.measure = "fpr")
    else if (aspc == "fpc")
      mes <- ROCR::performance(preds, measure = "fpr", x.measure = "rpp")
    else if (aspc == "fdrtpr") {
      mes <- ROCR::performance(preds, measure = "tpr", x.measure = "ppv")
      mes2 <- ROCR::performance(preds, measure = "fpr", x.measure = "rpp")
      stopifnot(all(unlist(mes@alpha.values) == unlist(mes2@alpha.values)))
    }

    if (isTRUE(revr))
      alphas <- 1 - unlist(mes@alpha.values)
    else
      alphas <- unlist(mes@alpha.values)

    if (aspc == "roc") {
      roc = cbind(FPR = unlist(mes@x.values) / fpcorr,
                  TPR = unlist(mes@y.values) / tpcorr,
                  ROC_CUTOFF = alphas)
      return(roc)
    } else if (aspc == "fpc") {
      fpc = cbind(topN = unlist(mes@x.values) * length(kg),
                  FP = unlist(mes@y.values) * length(which(bintruth[match(kg, names(bintruth))] == 0)),
                  FPC_CUTOFF = alphas)
      return(fpc)
    } else if (aspc == "fdrtpr") {
      fdrtpr = cbind(FDR = 1 - unlist(mes@x.values),
                     TPR = unlist(mes@y.values) / tpcorr,
                     NBR = unlist(mes2@x.values) * length(kg),
                     CUTOFF = alphas,
                     FP = unlist(mes2@y.values) * length(which(bintruth[match(kg, names(bintruth))] == 0)),
                     TOT_CALLED = length(kg))
      fdrtpr = cbind(fdrtpr, TP = fdrtpr[, "TPR"] * length(which(bintruth == 1)))
      fdrtpr = cbind(fdrtpr, FN = length(which(bintruth[match(kg, names(bintruth))] == 1)) - fdrtpr[, "TP"])
      fdrtpr = cbind(fdrtpr, TN = fdrtpr[, "TOT_CALLED"] - fdrtpr[, "TP"] - fdrtpr[, "FN"] - fdrtpr[, "FP"])
      fdrtpr = cbind(fdrtpr, DIFF = length(which(bintruth == 1)))
      fdrtpr = cbind(fdrtpr, NONDIFF = length(which(bintruth == 0)))
      return(fdrtpr)
    }
  } else {
    return(NULL)
  }
}

#' Calculate performance measures
#'
#' Calculate performance measures from a given collection of p-values, adjusted
#' p-values and scores provided in an \code{IBRAData} object.
#'
#' Depending on the collection of observations that are available for a given
#' method, the appropriate one will be chosen for each performance measure. For
#' \code{fpr}, \code{tpr}, \code{fdrtpr}, \code{fdrnbr}, \code{overlap} aspects,
#' results will only be calculated for methods where adjusted p-values are
#' included in the \code{IBRAData} object, since these calculations make use of
#' specific adjusted p-value cutoffs. For \code{fdrtprcurve},
#' \code{fdrnbrcurve}, \code{roc}, \code{fpc}, \code{corr}, \code{scatter}
#' aspects, the \code{score} observations will be preferentially used, given
#' that they are monotonically associated with the adjusted p-values (if
#' provided). If the \code{score} is not provided, the nominal p-values will be
#' used, given that they are monotonically associated with the adjusted p-values
#' (if provided). In other cases, the adjusted p-values will be used also for
#' these aspects.
#'
#' @param ibradata An IBRAData object.
#' @param binary_truth A character string giving the name of the column of
#'   truth(ibradata) that contains the binary truth (true assignment of
#'   variables into two classes, represented by 0/1).
#' @param cont_truth A character string giving the name of the column of
#'   truth(ibradata) that contains the continuous truth (a continuous value that
#'   the observations can be compared to).
#' @param aspects A character vector giving the types of performance measures to
#'   calculate. Must be a subset of c("fdrtpr", "fdrtprcurve", "fdrnbr",
#'   "fdrnbrcurve", "tpr", "fpr", "roc", "fpc", "overlap", "corr", "scatter").
#' @param thrs A numeric vector of adjusted p-value thresholds for which to
#'   calculate the performance measures. Affects "fdrtpr", "fdrnbr", "tpr" and
#'   "fpr".
#' @param splv A character string giving the name of the column of
#'   truth(ibradata) that will be used to stratify the results. The default
#'   value is "none", indicating no stratification.
#' @param maxsplit A numeric value giving the maximal number of categories to
#'   keep in the stratification. The largest categories containing both positive
#'   and negative features will be retained.
#' @param onlyshared A logical, indicating whether to only consider features for
#'   which both the true assignment and a result (p-value, adjusted p-value or
#'   score) is given. If FALSE, all features contained in the truth table are
#'   used.
#' @param thr_venn A numeric value giving the adjusted p-value threshold to use
#'   to create Venn diagrams.
#'
#' @return An IBRAPerformance object
#'
#' @export
#' @author Charlotte Soneson
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100), row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)), row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#' ibraperf <- calculate_performance(ibradata, binary_truth = "status",
#'                                   aspects = c("fdrtpr", "fdrtprcurve",
#'                                               "tpr", "roc"),
#'                                   thrs = c(0.01, 0.05, 0.1), splv = "none")
calculate_performance <- function(ibradata, binary_truth, cont_truth,
                                  aspects = c("fdrtpr", "fdrtprcurve", "fdrnbr",
                                              "fdrnbrcurve", "tpr", "fpr",
                                              "roc", "fpc", "overlap", "corr", "scatter"),
                                  thrs = c(0.01, 0.05, 0.1), splv = "none",
                                  maxsplit = 3, onlyshared = FALSE, thr_venn = 0.05) {

  ## Get all methods represented in the test result object (with at least one type of result)
  all_methods <- unique(c(colnames(pval(ibradata)), colnames(padj(ibradata)),
                          colnames(score(ibradata))))

  ## ------------------- NBR, TP, FP etc (always calculated) ------------ ##
  if (any(c("tpr", "fdr", "fdrtpr", "fpr", "fdrnbr") %in% aspects)) {
    outNBR <- outFP <- outTP <- outFN <- outTN <- outTOT_CALLED <- outDS <- outNONDS <- list()
    for (i in all_methods) {
      inpcol <- select_measure(ibradata, i, asp = "fpr")
      if (!is.null(inpcol)) {
        tmp <- slot(ibradata, inpcol)[i]

        if (onlyshared == FALSE) {
          allg <- rownames(truth(ibradata))
        } else {
          allg <- intersect(rownames(truth(ibradata)),
                            rownames(tmp)[which(!is.na(tmp[i]))])
        }
        tmp <- tmp[match(allg, rownames(tmp)), , drop = FALSE]
        truth <- truth(ibradata)[match(allg, rownames(truth(ibradata))), , drop = FALSE]
        keeplevels <- get_keeplevels(truth = truth, splv = splv, binary_truth = binary_truth, maxsplit = maxsplit)

        if (inpcol == "padj") {
          NBR <- list()
          nbr <- gen_thr_vec(thrs)
          for (thr in thrs) {
            nbr[paste0("thr", thr)] <- length(which(tmp[i] <= thr))
          }
          NBR[[paste0(i, "_overall", "__", inpcol)]] <-
            list(basemethod = i, meas_type = inpcol, nbr = nbr)
          if (splv != "none") {
            for (j in keeplevels) {
              nbr <- gen_thr_vec(thrs)
              for (thr in thrs) {
                g <- rownames(truth)[which(truth[[splv]] == j)]
                nbr[paste0("thr", thr)] <- length(intersect(g, rownames(tmp)[which(tmp[i] <= thr)]))
              }
              NBR[[paste0(i, "_", splv, ":", j, "__",
                          inpcol)]] <- list(basemethod = i,
                                            meas_type = inpcol, nbr = nbr)
            }
          }

          FP <- TP <- FN <- TN <- TOT_CALLED <- DS <- NONDS <- list()
          fp <- tp <- fn <- tn <- tot_called <- ds <- nonds <- gen_thr_vec(thrs)

          for (thr in thrs) {
            signif <- rownames(tmp)[which(tmp[i] <= thr)]
            nonsignif <- rownames(tmp)[which(tmp[i] > thr)]
            fp[paste0("thr", thr)] <-
              length(intersect(signif,
                               rownames(truth)[which(truth[, binary_truth] == 0)]))
            tp[paste0("thr", thr)] <-
              length(intersect(signif,
                               rownames(truth)[which(truth[, binary_truth] == 1)]))
            fn[paste0("thr", thr)] <-
              length(intersect(nonsignif,
                               rownames(truth)[which(truth[, binary_truth] == 1)]))
            tn[paste0("thr", thr)] <-
              length(intersect(nonsignif,
                               rownames(truth)[which(truth[, binary_truth] == 0)]))
            tot_called[paste0("thr", thr)] <-
              length(intersect(rownames(tmp)[which(!is.na(tmp[i]))], rownames(truth)))
            ds[paste0("thr", thr)] <- length(which(truth[, binary_truth] == 1))
            nonds[paste0("thr", thr)] <- length(which(truth[, binary_truth] == 0))
          }
          idx <- paste0(i, "_overall", "__", inpcol)
          FP[[idx]] <- list(basemethod = i, meas_type = inpcol, fp = fp)
          TP[[idx]] <- list(basemethod = i, meas_type = inpcol, tp = tp)
          FN[[idx]] <- list(basemethod = i, meas_type = inpcol, fn = fn)
          TN[[idx]] <- list(basemethod = i, meas_type = inpcol, tn = tn)
          TOT_CALLED[[idx]] <- list(basemethod = i, meas_type = inpcol,
                                    tot_called = tot_called)
          DS[[idx]] <- list(basemethod = i, meas_type = inpcol, ds = ds)
          NONDS[[idx]] <- list(basemethod = i, meas_type = inpcol, nonds = nonds)

          if (splv != "none" & any(truth[, binary_truth] == 0)) {
            for (j in keeplevels) {
              fp <- tp <- fn <- tn <- tot_called <- ds <- nonds <- gen_thr_vec(thrs)
              for (thr in thrs) {
                signif <- rownames(tmp)[which(tmp[i] <= thr)]
                nonsignif <- rownames(tmp)[which(tmp[i] > thr)]
                g <- rownames(truth)[which(truth[[splv]] == j)]
                gt <- intersect(g, rownames(truth)[which(truth[, binary_truth] == 0)])
                gtt <- intersect(g, rownames(truth)[which(truth[, binary_truth] == 1)])
                gf <- intersect(g, signif)
                gnf <- intersect(g, nonsignif)
                fp[paste0("thr", thr)] <- length(intersect(gf, gt))
                tp[paste0("thr", thr)] <- length(intersect(gf, gtt))
                fn[paste0("thr", thr)] <- length(intersect(gnf, gtt))
                tn[paste0("thr", thr)] <- length(intersect(gnf, gt))
                tot_called[paste0("thr", thr)] <-
                  length(intersect(rownames(tmp)[which(!is.na(tmp[i]))], g))
                ds[paste0("thr", thr)] <- length(gtt)
                nonds[paste0("thr", thr)] <- length(gt)
              }
              idx <- paste0(i, "_", splv, ":", j, "__", inpcol)
              FP[[idx]] <- list(basemethod = i, meas_type = inpcol, fp = fp)
              TP[[idx]] <- list(basemethod = i, meas_type = inpcol, tp = tp)
              FN[[idx]] <- list(basemethod = i, meas_type = inpcol, fn = fn)
              TN[[idx]] <- list(basemethod = i, meas_type = inpcol, tn = tn)
              TOT_CALLED[[idx]] <- list(basemethod = i, meas_type = inpcol,
                                        tot_called = tot_called)
              DS[[idx]] <- list(basemethod = i, meas_type = inpcol, ds = ds)
              NONDS[[idx]] <- list(basemethod = i, meas_type = inpcol, nonds = nonds)
            }
          }

          outNBR <- c(outNBR, NBR)
          outFP <- c(outFP, FP)
          outTP <- c(outTP, TP)
          outFN <- c(outFN, FN)
          outTN <- c(outTN, TN)
          outTOT_CALLED <- c(outTOT_CALLED, TOT_CALLED)
          outDS <- c(outDS, DS)
          outNONDS <- c(outNONDS, NONDS)
        } else {
          message(paste0("Column ", i, " is being ignored for NBRS calculations."))
        }
      }
    }
    nbrs <- t(do.call(rbind, lapply(outNBR, function(w) w$nbr)))
    vn <- sapply(outNBR, function(w) w$basemethod)
    tps <- t(do.call(rbind, lapply(outTP, function(w) w$tp)))
    vtp <- sapply(outTP, function(w) w$basemethod)
    fps <- t(do.call(rbind, lapply(outFP, function(w) w$fp)))
    vfp <- sapply(outFP, function(w) w$basemethod)
    tns <- t(do.call(rbind, lapply(outTN, function(w) w$tn)))
    vtn <- sapply(outTN, function(w) w$basemethod)
    fns <- t(do.call(rbind, lapply(outFN, function(w) w$fn)))
    vfn <- sapply(outFN, function(w) w$basemethod)
    tcs <- t(do.call(rbind, lapply(outTOT_CALLED, function(w) w$tot_called)))
    vtc <- sapply(outTOT_CALLED, function(w) w$basemethod)
    dss <- t(do.call(rbind, lapply(outDS, function(w) w$ds)))
    vds <- sapply(outDS, function(w) w$basemethod)
    nds <- t(do.call(rbind, lapply(outNONDS, function(w) w$nonds)))
    vnds <- sapply(outNONDS, function(w) w$basemethod)

    nbrs <- extend_resulttable(nbrs, splv, keeplevels, "NBR", vn, domelt = TRUE)
    tps <- extend_resulttable(tps, splv, keeplevels, "TP", vtp, domelt = TRUE)
    fps <- extend_resulttable(fps, splv, keeplevels, "FP", vfp, domelt = TRUE)
    fns <- extend_resulttable(fns, splv, keeplevels, "FN", vfn, domelt = TRUE)
    tns <- extend_resulttable(tns, splv, keeplevels, "TN", vtn, domelt = TRUE)
    tcs <- extend_resulttable(tcs, splv, keeplevels, "TOT_CALLED", vtc, domelt = TRUE)
    dss <- extend_resulttable(dss, splv, keeplevels, "DIFF", vds, domelt = TRUE)
    nds <- extend_resulttable(nds, splv, keeplevels, "NONDIFF", vnds, domelt = TRUE)

    all_nbr <- Reduce(function(...) merge(..., all = TRUE),
                      list(nbrs, tps, fps, tns, fns, tcs, dss, nds))
  }

  ## ----------------------------- CORR --------------------------------- ##
  if ("corr" %in% aspects) {
    outPEARSON <- list()
    outSPEARMAN <- list()
    for (i in all_methods) {
      inpcol <- select_measure(ibradata, i, asp = "corr")
      if (!is.null(inpcol)) {
        tmp <- slot(ibradata, inpcol)[i]

        if (onlyshared == FALSE) {
          allg <- rownames(truth(ibradata))
        } else {
          allg <- intersect(rownames(truth(ibradata)),
                            rownames(tmp)[which(!is.na(tmp[i]))])
        }
        tmp <- tmp[match(allg, rownames(tmp)), , drop = FALSE]
        truth <- truth(ibradata)[match(allg, rownames(truth(ibradata))), , drop = FALSE]
        keeplevels <- get_keeplevels(truth = truth, splv = splv, binary_truth = NULL, maxsplit = maxsplit)

        PEARSON <- list()
        SPEARMAN <- list()
        isf <- intersect(which(is.finite(tmp[[i]])), which(is.finite(truth[, cont_truth])))
        pearson <- cor(tmp[[i]][isf], truth[, cont_truth][isf], use = "complete.obs",
                       method = "pearson")
        spearman <- cor(tmp[[i]][isf], truth[, cont_truth][isf], use = "complete.obs",
                        method = "spearman")
        PEARSON[[paste0(i, "_overall", "__", inpcol)]] <- list(basemethod = i,
                                                               meas_type = inpcol,
                                                               pearson = pearson)
        SPEARMAN[[paste0(i, "_overall", "__", inpcol)]] <- list(basemethod = i,
                                                                meas_type = inpcol,
                                                                spearman = spearman)
        if (splv != "none") {
          for (j in keeplevels) {
            g <- rownames(truth)[which(truth[[splv]] == j)]
            isf <- intersect(which(is.finite(tmp[match(g, rownames(tmp)), i])),
                             which(is.finite(truth[match(g, rownames(truth)), cont_truth])))
            pearson <- cor(tmp[match(g, rownames(tmp)), i][isf],
                           truth[match(g, rownames(truth)), cont_truth][isf],
                           use = "complete.obs", method = "pearson")
            spearman <- cor(tmp[match(g, rownames(tmp)), i][isf],
                            truth[match(g, rownames(truth)), cont_truth][isf],
                            use = "complete.obs", method = "spearman")
            PEARSON[[paste0(i, "_", splv, ":", j, "__", inpcol)]] <-
              list(basemethod = i, meas_type = inpcol, pearson = pearson)
            SPEARMAN[[paste0(i, "_", splv, ":", j, "__", inpcol)]] <-
              list(basemethod = i, meas_type = inpcol, spearman = spearman)
          }
        }
        outPEARSON <- c(outPEARSON, PEARSON)
        outSPEARMAN <- c(outSPEARMAN, SPEARMAN)
      } else {
        message(paste0("Column ", i, " is being ignored for correlation calculations."))
      }
    }
    pearsons <- t(do.call(rbind, lapply(outPEARSON, function(w) w$pearson)))
    spearmans <- t(do.call(rbind, lapply(outSPEARMAN, function(w) w$spearman)))
    vp <- sapply(outPEARSON, function(w) w$basemethod)
    vs <- sapply(outSPEARMAN, function(w) w$basemethod)
    pearsons <- extend_resulttable(pearsons, splv, keeplevels, "PEARSON", vp, domelt = TRUE)
    spearmans <- extend_resulttable(spearmans, splv, keeplevels, "SPEARMAN", vs, domelt = TRUE)
    corrs <- merge(pearsons, spearmans)
  } else {
    corrs <- data.frame()
  }

  ## ----------------------------- TPR ---------------------------------- ##
  if (any(c("tpr", "fdrtpr", "fdrnbr") %in% aspects)) {
    outTPR <- list()
    for (i in all_methods) {
      inpcol <- select_measure(ibradata, i, asp = "tpr")
      if (!is.null(inpcol)) {
        tmp <- slot(ibradata, inpcol)[i]

        if (onlyshared == FALSE) {
          allg <- rownames(truth(ibradata))
        } else {
          allg <- intersect(rownames(truth(ibradata)),
                            rownames(tmp)[which(!is.na(tmp[i]))])
        }
        tmp <- tmp[match(allg, rownames(tmp)), , drop = FALSE]
        truth <- truth(ibradata)[match(allg, rownames(truth(ibradata))), , drop = FALSE]
        keeplevels <- get_keeplevels(truth = truth, splv = splv, binary_truth = binary_truth, maxsplit = maxsplit)

        if (inpcol == "padj") {
          TPR <- list()
          tpr <- gen_thr_vec(thrs)
          for (thr in thrs) {
            signif <- rownames(tmp)[which(tmp[i] <= thr)]
            tpr[paste0("thr", thr)] <-
              length(intersect(signif,
                               rownames(truth)[which(truth[, binary_truth] == 1)]))/
              length(which(truth[, binary_truth] == 1))
          }
          TPR[[paste0(i, "_overall", "__", inpcol)]] <- list(basemethod = i,
                                                             meas_type = inpcol, tpr = tpr)

          if (splv != "none") {
            for (j in keeplevels) {
              tpr <- gen_thr_vec(thrs)
              for (thr in thrs) {
                g <- rownames(truth)[which(truth[[splv]] == j)]
                signif <- intersect(g, rownames(tmp)[which(tmp[i] <= thr)])
                gt <- intersect(g, rownames(truth)[which(truth[, binary_truth] == 1)])
                gf <- intersect(g, signif)
                tpr[paste0("thr", thr)] <- length(intersect(gf, gt))/length(gt)
              }
              TPR[[paste0(i, "_", splv, ":", j, "__", inpcol)]] <-
                list(basemethod = i, meas_type = inpcol, tpr = tpr)
            }
          }
          outTPR <- c(outTPR, TPR)
        } else {
          message(paste0("Column ", i, " is being ignored for TPR calculations."))
        }
      }
    }
    tprs <- t(do.call(rbind, lapply(outTPR, function(w) w$tpr)))
    vt <- sapply(outTPR, function(w) w$basemethod)
    tprs <- extend_resulttable(tprs, splv, keeplevels, "TPR", vt, domelt = TRUE)
  } else {
    tprs <- data.frame()
  }

  ## ----------------------------- FDR ---------------------------------- ##
  if (any(c("fdrtpr", "fdrnbr") %in% aspects)) {
    outFDR <- list()
    for (i in all_methods) {
      inpcol <- select_measure(ibradata, i, asp = "fdr")
      if (!is.null(inpcol)) {
        tmp <- slot(ibradata, inpcol)[i]

        if (onlyshared == FALSE) {
          allg <- rownames(truth(ibradata))
        } else {
          allg <- intersect(rownames(truth(ibradata)),
                            rownames(tmp)[which(!is.na(tmp[i]))])
        }
        tmp <- tmp[match(allg, rownames(tmp)), , drop = FALSE]
        truth <- truth(ibradata)[match(allg, rownames(truth(ibradata))), , drop = FALSE]
        keeplevels <- get_keeplevels(truth = truth, splv = splv, binary_truth = binary_truth, maxsplit = maxsplit)

        if (inpcol == "padj") {
          FDR <- list()
          fdr <- gen_thr_vec(thrs)
          for (thr in thrs) {
            signif <- rownames(tmp)[which(tmp[i] <= thr)]
            if (length(signif) == 0) {
              fdr[paste0("thr", thr)] <- 0
            } else {
              fdr[paste0("thr", thr)] <-
                length(setdiff(signif,
                               rownames(truth)[which(truth[, binary_truth] == 1)]))/
                length(signif)
            }
          }
          FDR[[paste0(i, "_overall", "__", inpcol)]] <- list(basemethod = i,
                                                             meas_type = inpcol, fdr = fdr)

          if (splv != "none" & any(truth[, binary_truth] == 0)) {
            for (j in keeplevels) {
              fdr <- gen_thr_vec(thrs)
              for (thr in thrs) {
                g <- rownames(truth)[which(truth[[splv]] == j)]
                signif <- intersect(g, rownames(tmp)[which(tmp[i] <= thr)])
                gt <- intersect(g, rownames(truth)[which(truth[, binary_truth] == 0)])
                gf <- intersect(g, signif)
                if (length(gf) == 0) {
                  fdr[paste0("thr", thr)] <- 0
                } else {
                  fdr[paste0("thr", thr)] <- length(intersect(gf, gt))/length(gf)
                }
              }
              FDR[[paste0(i, "_", splv, ":", j, "__", inpcol)]] <-
                list(basemethod = i, meas_type = inpcol, fdr = fdr)
            }
          }
          outFDR <- c(outFDR, FDR)
        } else {
          message(paste0("Column ", i, " is being ignored for FDR calculations."))
        }
      }
    }
    fdrs <- t(do.call(rbind, lapply(outFDR, function(w) w$fdr)))
    vf <- sapply(outFDR, function(w) w$basemethod)

    fdrs <- extend_resulttable(fdrs, splv, keeplevels, "FDR", vf, domelt = TRUE)
    fdrs$satis <- ifelse(fdrs$FDR < as.numeric(gsub("thr", "", fdrs$thr)),
                         "yes", "no")
    fdrs$method.satis <- paste0(fdrs$fullmethod, fdrs$satis)
  } else {
    fdrs <- data.frame()
  }

  ## ----------------------------- FPR ---------------------------------- ##
  if ("fpr" %in% aspects) {
    outFPR <- list()
    for (i in all_methods) {
      inpcol <- select_measure(ibradata, i, asp = "fpr")
      if (!is.null(inpcol)) {
        tmp <- slot(ibradata, inpcol)[i]

        if (onlyshared == FALSE) {
          allg <- rownames(truth(ibradata))
        } else {
          allg <- intersect(rownames(truth(ibradata)),
                            rownames(tmp)[which(!is.na(tmp[i]))])
        }
        tmp <- tmp[match(allg, rownames(tmp)), , drop = FALSE]
        truth <- truth(ibradata)[match(allg, rownames(truth(ibradata))), , drop = FALSE]
        keeplevels <- get_keeplevels(truth = truth, splv = splv, binary_truth = binary_truth, maxsplit = maxsplit)

        if (inpcol == "padj") {
          FPR <- list()
          fpr <- gen_thr_vec(thrs)
          for (thr in thrs) {
            signif <- rownames(tmp)[which(tmp[i] <= thr)]
            fpr[paste0("thr", thr)] <-
              length(intersect(signif,
                               rownames(truth)[which(truth[, binary_truth] == 0)]))/
              length(which(truth[, binary_truth] == 0))
          }
          FPR[[paste0(i, "_overall", "__", inpcol)]] <- list(basemethod = i,
                                                             meas_type = inpcol, fpr = fpr)

          if (splv != "none" & any(truth[, binary_truth] == 0)) {
            for (j in keeplevels) {
              fpr <- gen_thr_vec(thrs)
              for (thr in thrs) {
                g <- rownames(truth)[which(truth[[splv]] == j)]
                signif <- intersect(g, rownames(tmp)[which(tmp[i] <= thr)])
                gt <- intersect(g, rownames(truth)[which(truth[, binary_truth] == 0)])
                gf <- intersect(g, signif)
                fpr[paste0("thr", thr)] <- length(intersect(gf, gt))/length(gt)
              }
              FPR[[paste0(i, "_", splv, ":", j, "__", inpcol)]] <-
                list(basemethod = i, meas_type = inpcol, fpr = fpr)
            }
          }
          outFPR <- c(outFPR, FPR)
        } else {
          message(paste0("Column ", i, " is being ignored for FPR calculations."))
        }
      }
    }
    fprs <- t(do.call(rbind, lapply(outFPR, function(w) w$fpr)))
    vff <- sapply(outFPR, function(w) w$basemethod)

    fprs <- extend_resulttable(fprs, splv, keeplevels, "FPR", vff, domelt = TRUE)
  } else {
    fprs <- data.frame()
  }

  ## ------------------------- FDRTPRcurve ------------------------------ ##
  if (any(c("fdrtprcurve", "fdrnbrcurve") %in% aspects)) {
    outFDRTPR <- list()
    for (i in all_methods) {
      inpcol <- select_measure(ibradata, i, asp = "fdrtpr")
      if (!is.null(inpcol)) {
        tmp <- slot(ibradata, inpcol)[i]

        if (onlyshared == FALSE) {
          allg <- rownames(truth(ibradata))
        } else {
          allg <- intersect(rownames(truth(ibradata)),
                            rownames(tmp)[which(!is.na(tmp[i]))])
        }
        tmp <- tmp[match(allg, rownames(tmp)), , drop = FALSE]
        truth <- truth(ibradata)[match(allg, rownames(truth(ibradata))), , drop = FALSE]
        keeplevels <- get_keeplevels(truth = truth, splv = splv, binary_truth = binary_truth, maxsplit = maxsplit)

        if (!is.null(inpcol)) {
          if (any(truth[, binary_truth] == 0) & any(truth[, binary_truth] == 1)) {
            FDRTPR <- list()
            bintruth <- truth[, binary_truth]
            names(bintruth) <- rownames(truth)
            vals <- tmp[[i]]
            names(vals) <- rownames(tmp)
            fdrtpr <- get_curve(bintruth = bintruth, vals = vals,
                                revr = ifelse(inpcol == "score", FALSE, TRUE),
                                aspc = "fdrtpr")
            FDRTPR[[paste0(i, "_overall", "__", inpcol)]] <-
              list(basemethod = i, meas_type = inpcol, fdrtpr = fdrtpr)
            if (splv != "none") {
              for (j in keeplevels) {
                bintruth <- truth[which(truth[[splv]] == j), binary_truth]
                names(bintruth) <- rownames(truth)[which(truth[[splv]] ==  j)]
                fdrtpr <- get_curve(bintruth = bintruth, vals = vals,
                                    revr = ifelse(inpcol == "score", FALSE, TRUE),
                                    aspc = "fdrtpr")
                if (!is.null(fdrtpr)) {
                  FDRTPR[[paste0(i, "_", splv, ":", j, "__", inpcol)]] <-
                    list(basemethod = i, meas_type = inpcol, fdrtpr = fdrtpr)
                }
              }
            }
          } else {
            FDRTPR <- NULL
          }
          outFDRTPR <- c(outFDRTPR, FDRTPR)
        } else {
          message(paste0("Column ", i, " is being ignored for FDRTPR calculations."))
        }
      }
    }
    if (any(sapply(lapply(outFDRTPR, function(w) w$fdrtpr),
                   length) > 0)) {
      vftp <- sapply(outFDRTPR, function(w) w$basemethod)
      fdrtprs <- do.call(rbind, lapply(names(outFDRTPR), function(s) {
        data.frame(FDR = outFDRTPR[[s]]$fdrtpr[, "FDR"],
                   TPR = outFDRTPR[[s]]$fdrtpr[, "TPR"],
                   NBR = outFDRTPR[[s]]$fdrtpr[, "NBR"],
                   CUTOFF = outFDRTPR[[s]]$fdrtpr[, "CUTOFF"],
                   TP = outFDRTPR[[s]]$fdrtpr[, "TP"],
                   FP = outFDRTPR[[s]]$fdrtpr[, "FP"],
                   TN = outFDRTPR[[s]]$fdrtpr[, "TN"],
                   FN = outFDRTPR[[s]]$fdrtpr[, "FN"],
                   TOT_CALLED = outFDRTPR[[s]]$fdrtpr[, "TOT_CALLED"],
                   DIFF = outFDRTPR[[s]]$fdrtpr[, "DIFF"],
                   NONDIFF = outFDRTPR[[s]]$fdrtpr[, "NONDIFF"],
                   method = s)
      }))
      fdrtprs <- extend_resulttable(fdrtprs, splv, keeplevels, NULL, vftp, domelt = FALSE)
    }
  } else {
    fdrtprs <- data.frame()
  }
  fdrnbrs <- fdrtprs

  ## ----------------------------- ROC ---------------------------------- ##
  if ("roc" %in% aspects) {
    outROC <- list()

    for (i in all_methods) {
      inpcol <- select_measure(ibradata, i, asp = "roc")
      if (!is.null(inpcol)) {
        tmp <- slot(ibradata, inpcol)[i]
        if (onlyshared == FALSE) {
          allg <- rownames(truth(ibradata))
        } else {
          allg <- intersect(rownames(truth(ibradata)),
                            rownames(tmp)[which(!is.na(tmp[i]))])
        }
        tmp <- tmp[match(allg, rownames(tmp)), , drop = FALSE]
        truth <- truth(ibradata)[match(allg, rownames(truth(ibradata))), , drop = FALSE]
        keeplevels <- get_keeplevels(truth = truth, splv = splv, binary_truth = binary_truth, maxsplit = maxsplit)

        if (!is.null(inpcol)) {
          if (any(truth[, binary_truth] == 0) & any(truth[, binary_truth] == 1)) {
            ROC <- list()
            bintruth <- truth[, binary_truth]
            names(bintruth) <- rownames(truth)
            vals <- tmp[[i]]
            names(vals) <- rownames(tmp)
            roc <- get_curve(bintruth = bintruth, vals = vals,
                             revr = ifelse(inpcol == "score", FALSE, TRUE), aspc = "roc")
            ROC[[paste0(i, "_overall", "__", inpcol)]] <-
              list(basemethod = i, meas_type = inpcol, roc = roc)
            if (splv != "none") {
              for (j in keeplevels) {
                bintruth <- truth[which(truth[[splv]] == j), binary_truth]
                names(bintruth) <- rownames(truth)[which(truth[[splv]] == j)]
                roc <- get_curve(bintruth = bintruth, vals = vals,
                                 revr = ifelse(inpcol == "score", FALSE, TRUE), aspc = "roc")
                if (!is.null(roc)) {
                  ROC[[paste0(i, "_", splv, ":", j, "__", inpcol)]] <-
                    list(basemethod = i, meas_type = inpcol, roc = roc)
                }
              }
            }
          }
        } else {
          ROC <- NULL
        }
        outROC <- c(outROC, ROC)
      } else {
        message(paste0("Column ", i, " is being ignored."))
      }
    }

    if (any(sapply(lapply(outROC, function(w) w$roc), length) > 0)) {
      vr <- sapply(outROC, function(w) w$basemethod)
      rocs <- do.call(rbind, lapply(names(outROC), function(s) {
        data.frame(FPR = outROC[[s]]$roc[, "FPR"],
                   TPR = outROC[[s]]$roc[, "TPR"],
                   ROC_CUTOFF = outROC[[s]]$roc[, "ROC_CUTOFF"],
                   method = s)
      }))
      rocs <- extend_resulttable(rocs, splv, keeplevels, NULL, vr, domelt = FALSE)
    } else {
      rocs <- data.frame()
    }
  } else {
    rocs <- data.frame()
  }

  ## ---------------------------- SCATTER -------------------------------- ##
  if ("scatter" %in% aspects) {
    outSCATTER <- list()

    for (i in all_methods) {
      inpcol <- select_measure(ibradata, i, asp = "scatter")
      if (!is.null(inpcol)) {
        tmp <- slot(ibradata, inpcol)[i]
        if (onlyshared == FALSE) {
          allg <- rownames(truth(ibradata))
        } else {
          allg <- intersect(rownames(truth(ibradata)),
                            rownames(tmp)[which(!is.na(tmp[i]))])
        }
        tmp <- tmp[match(allg, rownames(tmp)), , drop = FALSE]
        truth <- truth(ibradata)[match(allg, rownames(truth(ibradata))), , drop = FALSE]
        keeplevels <- get_keeplevels(truth = truth, splv = splv, binary_truth = NULL, maxsplit = maxsplit)

        SCATTER <- list()
        SCATTER[[paste0(i, "_overall", "__", inpcol)]] <-
          list(basemethod = i, meas_type = inpcol,
               scatter = data.frame(vals = tmp[[i]], truth = truth[, cont_truth],
                                    row.names = rownames(tmp),
                                    stringsAsFactors = FALSE))
        if (splv != "none") {
          for (j in keeplevels) {
            s <- which(truth[[splv]] == j)
            SCATTER[[paste0(i, "_", splv, ":", j, "__", inpcol)]] <-
              list(basemethod = i, meas_type = inpcol,
                   scatter = data.frame(vals = tmp[[i]][s], truth = truth[s, cont_truth],
                                        row.names = rownames(tmp)[s],
                                        stringsAsFactors = FALSE))
          }
        }
      } else {
        SCATTER <- NULL
      }
      outSCATTER <- c(outSCATTER, SCATTER)
    }

    if (any(sapply(lapply(outSCATTER, function(w) w$scatter), length) > 0)) {
      vsc <- sapply(outSCATTER, function(w) w$basemethod)
      scatters <- do.call(rbind, lapply(names(outSCATTER), function(s) {
        data.frame(OBSERVATION = outSCATTER[[s]]$scatter[, "vals"],
                   TRUTH = outSCATTER[[s]]$scatter[, "truth"],
                   feature = rownames(outSCATTER[[s]]$scatter),
                   method = s)
      }))
      scatters <- extend_resulttable(scatters, splv, keeplevels, NULL, vsc, domelt = FALSE)
    } else {
      scatters <- data.frame()
    }
  } else {
    scatters <- data.frame()
  }

  ## ----------------------------- FPC ---------------------------------- ##
  if ("fpc" %in% aspects) {
    outFPC <- list()

    for (i in all_methods) {
      inpcol <- select_measure(ibradata, i, asp = "fpc")
      if (!is.null(inpcol)) {
        tmp <- slot(ibradata, inpcol)[i]
        if (onlyshared == FALSE) {
          allg <- rownames(truth(ibradata))
        } else {
          allg <- intersect(rownames(truth(ibradata)),
                            rownames(tmp)[which(!is.na(tmp[i]))])
        }
        tmp <- tmp[match(allg, rownames(tmp)), , drop = FALSE]
        truth <- truth(ibradata)[match(allg, rownames(truth(ibradata))), , drop = FALSE]
        keeplevels <- get_keeplevels(truth = truth, splv = splv, binary_truth = binary_truth, maxsplit = maxsplit)

        if (!is.null(inpcol)) {
          if (any(truth[, binary_truth] == 0) & any(truth[, binary_truth] == 1)) {
            FPC <- list()
            bintruth <- truth[, binary_truth]
            names(bintruth) <- rownames(truth)
            vals <- tmp[[i]]
            names(vals) <- rownames(tmp)
            fpc <- get_curve(bintruth = bintruth, vals = vals,
                             revr = ifelse(inpcol == "score", FALSE, TRUE), aspc = "fpc")
            FPC[[paste0(i, "_overall", "__", inpcol)]] <-
              list(basemethod = i, meas_type = inpcol, fpc = fpc)
            if (splv != "none") {
              for (j in keeplevels) {
                bintruth <- truth[which(truth[[splv]] == j), binary_truth]
                names(bintruth) <- rownames(truth)[which(truth[[splv]] == j)]
                fpc <- get_curve(bintruth = bintruth, vals = vals,
                                 revr = ifelse(inpcol == "score", FALSE, TRUE), aspc = "fpc")
                if (!is.null(fpc)) {
                  FPC[[paste0(i, "_", splv, ":", j, "__", inpcol)]] <-
                    list(basemethod = i, meas_type = inpcol, fpc = fpc)
                }
              }
            }
          }
        } else {
          FPC <- NULL
        }
        outFPC <- c(outFPC, FPC)
      } else {
        message(paste0("Column ", i, " is being ignored."))
      }
    }

    if (any(sapply(lapply(outFPC, function(w) w$fpc), length) > 0)) {
      vfc <- sapply(outFPC, function(w) w$basemethod)
      fpcs <- do.call(rbind, lapply(names(outFPC), function(s) {
        data.frame(topN = outFPC[[s]]$fpc[, "topN"],
                   FP = outFPC[[s]]$fpc[, "FP"],
                   FPC_CUTOFF = outFPC[[s]]$fpc[, "FPC_CUTOFF"],
                   method = s)
      }))
      fpcs <- extend_resulttable(fpcs, splv, keeplevels, NULL, vfc, domelt = FALSE)
    } else {
      fpcs <- data.frame()
    }
  } else {
    fpcs <- data.frame()
  }

  ## --------------------------- OVERLAP -------------------------------- ##
  if ("overlap" %in% aspects) {
    tmplist <- padj(ibradata)
    ## Add 'truth' to list of results
    tmp2 <- 1 - truth(ibradata)[, binary_truth, drop = FALSE]
    colnames(tmp2) <- "truth"
    missing_genes <- setdiff(rownames(tmp2), rownames(tmplist))
    tmpadd <- as.data.frame(matrix(NA, length(missing_genes), ncol(tmplist),
                                   dimnames = list(missing_genes, colnames(tmplist))))
    tmplist <- rbind(tmplist, tmpadd)
    tmplist$truth <- tmp2$truth[match(rownames(tmplist), rownames(tmp2))]

    overlap <- apply(tmplist, 2, function(w) {
      as.numeric(w <= thr_venn)
    })
    common_genes <- rownames(tmplist)
    rownames(overlap) <- common_genes
    overlap <- overlap[, which(colSums(is.na(overlap)) != nrow(overlap))]

    ## Assume that if we don't have information about a gene, it is "negative"
    overlap[is.na(overlap)] <- 0
    overlap <- data.frame(overlap, stringsAsFactors = FALSE)

    ## If splv is not "none", split overlap matrix
    if (splv != "none") {
      keep <- intersect(rownames(overlap), rownames(truth(ibradata)))
      tth <- truth(ibradata)[match(keep, rownames(truth(ibradata))), ]
      overlap <- overlap[match(keep, rownames(overlap)), ]
      overlap <- split(overlap, tth[, splv])
    }
  } else {
    overlap <- data.frame()
  }

  ## --------------------------- RETURNS -------------------------------- ##
  ## Put data together
  if ("fdrtpr" %in% aspects) {
    fdrtpr <- Reduce(function(...) merge(..., all = TRUE),
                     list(all_nbr, tprs, fdrs))
  } else {
    fdrtpr <- data.frame()
  }
  if ("fdrnbr" %in% aspects) {
    fdrnbr <- Reduce(function(...) merge(..., all = TRUE),
                     list(all_nbr, tprs, fdrs))
  } else {
    fdrnbr <- data.frame()
  }
  if ("tpr" %in% aspects) {
    tprs <- Reduce(function(...) merge(..., all = TRUE),
                   list(all_nbr, tprs))
  }
  if ("fpr" %in% aspects) {
    fprs <- Reduce(function(...) merge(..., all = TRUE),
                   list(all_nbr, fprs))
  }

  if (!("tpr" %in% aspects)) tprs <- data.frame()
  if (!("fdrtprcurve" %in% aspects)) fdrtprcurve <- data.frame()
  if (!("fdrnbrcurve" %in% aspects)) fdrnbrcurve <- data.frame()

  IBRAPerformance(tpr = tprs, fpr = fprs, fdrtprcurve = fdrtprs, fdrnbrcurve = fdrnbrs,
                  roc = rocs, fpc = fpcs, fdrtpr = fdrtpr, fdrnbr = fdrnbr,
                  maxsplit = maxsplit, overlap = overlap, splv = splv,
                  corr = corrs, scatter = scatters)

}

#' Prepare data for plotting
#'
#' Prepare performance data provided in an \code{IBRAPerformance} object
#' (obtained by \code{\link{calculate_performance}}) for plotting.
#'
#' @param ibraperf An \code{IBRAPerformance} object.
#' @param keepmethods A character vector consisting of methods to retain for
#'   plotting (these should be a subset of \code{basemethods(ibraperf)}), or
#'   NULL (indicating that all methods represented in ibraperf should be
#'   retained).
#' @param incloverall A logical indicating whether the "overall" results should
#'   be included if the results are stratified by an annotation.
#' @param colorscheme Either a character string giving the color palette to use
#'   to define colors for the different methods, or a character vector with
#'   colors to use. The available pre-defined palettes depend on the number of
#'   different methods to distinguish. The choices are:
#'   \describe{
#'   \item{- \code{Accent}}{(max 8 methods)}
#'   \item{- \code{Dark2}}{(max 8 methods)}
#'   \item{- \code{Paired}}{(max 12 methods)}
#'   \item{- \code{Pastel1}}{(max 9 methods)}
#'   \item{- \code{Pastel2}}{(max 8 methods)}
#'   \item{- \code{Set1}}{(max 9 methods)}
#'   \item{- \code{Set2}}{(max 8 methods)}
#'   \item{- \code{Set3}}{(max 12 methods)}
#'   \item{- \code{hue_pal}}{}
#'   \item{- \code{rainbow}}{}
#'   \item{- \code{heat}}{}
#'   \item{- \code{terrain}}{}
#'   \item{- \code{topo}}{}
#'   \item{- \code{cm}}{}
#'   }
#'   If the number of allowed methods is exceeded, the colorscheme defaults to
#'   \code{hue_pal}.
#' @param facetted A logical indicating whether the results should be split into
#'   subpanels when stratified by an annotation (\code{TRUE}), or kept in the same
#'   panel but shown with different colors (\code{FALSE}).
#' @param incltruth A logical indicating whether the truth should be included in
#'   Venn diagrams.
#'
#' @return An \code{IBRAPlot} object
#'
#' @export
#' @author Charlotte Soneson
#' @examples
#' set.seed(123)
#' padj <- data.frame(m1 = runif(100), m2 = runif(100), row.names = paste0("G", 1:100))
#' truth <- data.frame(status = round(runif(100)), row.names = paste0("G", 1:100))
#' ibradata <- IBRAData(padj = padj, truth = truth)
#' ibraperf <- calculate_performance(ibradata, binary_truth = "status", cont_truth = "none",
#'                                   aspects = c("fdrtpr", "fdrtprcurve",
#'                                               "tpr", "roc"),
#'                                   thrs = c(0.01, 0.05, 0.1), splv = "none")
#' ibraplot <- prepare_data_for_plot(ibraperf, keepmethods = NULL,
#'                                   colorscheme = "Dark2")
#'
#' ## User-specified colors
#' ibraplot2 <- prepare_data_for_plot(ibraperf, keepmethods = NULL,
#'                                    colorscheme = c("blue", "red"))
prepare_data_for_plot <- function(ibraperf, keepmethods = NULL, incloverall = TRUE,
                                  colorscheme = "hue_pal", facetted = TRUE, incltruth = TRUE) {
  splitval <- NULL

  if (is.null(keepmethods)) {
    keepmethods <- basemethods(ibraperf)
  }

  ## Subset ibraperf object
  ibraperf <- ibraperf[, keepmethods]

  ## Exclude truth from overlap matrix
  if (!isTRUE(incltruth)) {
    if (length(overlap(ibraperf)) != 0) {
      if (class(overlap(ibraperf)) == "list") {
        overlap(ibraperf) <- lapply(overlap(ibraperf), function(w) {
          w <- w[, setdiff(colnames(w), "truth"), drop = FALSE]
        })
      } else {
        overlap(ibraperf) <-
          overlap(ibraperf)[, setdiff(colnames(overlap(ibraperf)), "truth"), drop = FALSE]
      }
    }
  }

  ## Define colors
  use_colors <- define_colors(ibraperf = ibraperf, palette = colorscheme,
                              facetted = facetted, incloverall = incloverall)

  ## Exclude overall level
  for (sl in c("tpr", "fpr", "corr", "roc", "fpc", "scatter",
               "fdrtprcurve", "fdrtpr", "fdrnbrcurve", "fdrnbr")) {
    if (splv(ibraperf) != "none") {
      if (!(isTRUE(incloverall))) {
        if (length(slot(ibraperf, sl)) != 0)
          slot(ibraperf, sl) <- subset(slot(ibraperf, sl), splitval != "overall")
      }
    }
    if (!is.null(slot(ibraperf, sl))) {
      if (isTRUE(facetted))
        slot(ibraperf, sl)$num_method <- as.numeric(as.factor(slot(ibraperf, sl)$method))
      else
        slot(ibraperf, sl)$num_method <- as.numeric(as.factor(slot(ibraperf, sl)$fullmethod))
    }
  }

  ibraperf <- as(ibraperf, "IBRAPlot")
  facetted(ibraperf) <- facetted
  plotcolors(ibraperf) <- use_colors
  ibraperf
}

