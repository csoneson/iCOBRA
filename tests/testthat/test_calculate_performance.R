## Test that calculate_performance is doing the right thing.

library(iCOBRA)
context("Check calculation of performance scores")

local({
  ## Remove some genes from the truth
  cobradata <- cobradata_example_sval
  truth(cobradata) <- truth(cobradata)[1:3000, , drop = FALSE]

  set.seed(123)
  ## Set some adjusted p-values to NA
  tmp <- padj(cobradata)$Method1
  tmp[sample(1:length(tmp), 100)] <- NA
  padj(cobradata)$Method1 <- tmp
  
  ## Set some s-values to NA
  tmp <- sval(cobradata)$Method1
  tmp[sample(1:length(tmp), 100)] <- NA
  sval(cobradata)$Method1 <- tmp

  ## Set some scores to NA
  tmp <- iCOBRA::score(cobradata)$Method1
  tmp[sample(1:length(tmp), 100)] <- NA
  iCOBRA::score(cobradata)$Method1 <- tmp
  
  ## Add a truth column represented with logical values instead of 0/1
  truth(cobradata)$statuslogical <- as.logical(truth(cobradata)$status)
  
  ## No stratification, onlyshared = FALSE
  ib1 <- calculate_performance(cobradata, binary_truth = "status",
                               cont_truth = "logFC",
                               aspects = c("fdrtpr", "fdrtprcurve", "fdrnbr",
                                           "fdrnbrcurve", "tpr", "fpr",
                                           "roc", "fpc", "overlap", "corr",
                                           "scatter", "deviation", 
                                           "fsrnbr", "fsrnbrcurve"),
                               thrs = c(0.05), splv = "none",
                               onlyshared = FALSE, thr_venn = 0.05, 
                               rank_by_abs = TRUE)
  
  ## The same, but with the logical truth
  ib1logical <- calculate_performance(cobradata, binary_truth = "statuslogical",
                                      cont_truth = "logFC",
                                      aspects = c("fdrtpr", "fdrtprcurve", "fdrnbr",
                                                  "fdrnbrcurve", "tpr", "fpr",
                                                  "roc", "fpc", "overlap", "corr",
                                                  "scatter", "deviation", 
                                                  "fsrnbr", "fsrnbrcurve"),
                                      thrs = c(0.05), splv = "none",
                                      onlyshared = FALSE, thr_venn = 0.05, 
                                      rank_by_abs = TRUE)

  ## Stratification, onlyshared = FALSE
  ib2 <- calculate_performance(cobradata, binary_truth = "status",
                               cont_truth = "logFC",
                               aspects = c("fdrtpr", "fdrtprcurve", "fdrnbr",
                                           "fdrnbrcurve", "tpr", "fpr",
                                           "roc", "fpc", "overlap", "corr",
                                           "scatter", "deviation", 
                                           "fsrnbr", "fsrnbrcurve"),
                               thrs = c(0.05), splv = "expr_cat",
                               onlyshared = FALSE, thr_venn = 0.05,
                               rank_by_abs = TRUE)

  ## No stratification, onlyshared = TRUE
  ib3 <- calculate_performance(cobradata, binary_truth = "status",
                               cont_truth = "logFC",
                               aspects = c("fdrtpr", "fdrtprcurve", "fdrnbr",
                                           "fdrnbrcurve", "tpr", "fpr",
                                           "roc", "fpc", "overlap", "corr",
                                           "scatter", "deviation", 
                                           "fsrnbr", "fsrnbrcurve"),
                               thrs = c(0.05), splv = "none", onlyshared = TRUE,
                               thr_venn = 0.05,
                               rank_by_abs = TRUE)

  pval <- pval(cobradata)
  padj <- padj(cobradata)
  sval <- sval(cobradata)
  score <- score(cobradata)
  truth <- truth(cobradata)

  pvalsub <- pval[rownames(subset(truth, expr_cat == "[   2.628,  17.148)")), ]
  padjsub <- padj[rownames(subset(truth, expr_cat == "[   2.628,  17.148)")), ]
  svalsub <- sval[rownames(subset(truth, expr_cat == "[   2.628,  17.148)")), ]
  scoresub <- score[rownames(subset(truth, expr_cat == "[   2.628,  17.148)")), ]
  truthsub <- truth[rownames(subset(truth, expr_cat == "[   2.628,  17.148)")), ]

  tp1 <- length(intersect(rownames(subset(padj, Method1 <= 0.05)),
                          rownames(subset(truth, status == 1))))
  fp1 <- length(intersect(rownames(subset(padj, Method1 <= 0.05)),
                          rownames(subset(truth, status == 0))))
  fn1 <- length(intersect(rownames(subset(padj, Method1 > 0.05)),
                          rownames(subset(truth, status == 1))))
  tn1 <- length(intersect(rownames(subset(padj, Method1 > 0.05)),
                          rownames(subset(truth, status == 0))))
  cc1 <- length(setdiff(rownames(subset(truth, status == 1)),
                        rownames(subset(padj, !is.na(Method1)))))
  dd1 <- length(setdiff(rownames(subset(truth, status == 0)),
                        rownames(subset(padj, !is.na(Method1)))))
  aa1 <- length(setdiff(rownames(subset(padj, Method1 <= 0.05)),
                        rownames(subset(truth, status %in% c(0, 1)))))
  bb1 <- length(setdiff(rownames(subset(padj, Method1 > 0.05)),
                        rownames(subset(truth, status %in% c(0, 1)))))
  ## True/false sign
  av <- sign(score[rownames(subset(sval, Method1 <= 0.05)), "Method1"])
  bv <- sign(truth[rownames(subset(sval, Method1 <= 0.05)), "logFC"])
  kp <- which(!is.na(bv))
  av <- av[kp]
  bv <- bv[kp]
  av[is.na(av)] <- -bv[is.na(av)]
  ts1 <- sum(av == bv, na.rm = TRUE)
  fs1 <- sum(abs(av - bv) == 2, na.rm = TRUE)
  fsr1 <- fs1/length(av)
  fnbr1 <- length(av)

  tp2 <- length(intersect(rownames(subset(padjsub, Method1 <= 0.05)),
                          rownames(subset(truthsub, status == 1))))
  fp2 <- length(intersect(rownames(subset(padjsub, Method1 <= 0.05)),
                          rownames(subset(truthsub, status == 0))))
  fn2 <- length(intersect(rownames(subset(padjsub, Method1 > 0.05)),
                          rownames(subset(truthsub, status == 1))))
  tn2 <- length(intersect(rownames(subset(padjsub, Method1 > 0.05)),
                          rownames(subset(truthsub, status == 0))))
  cc2 <- length(setdiff(rownames(subset(truthsub, status == 1)),
                        rownames(subset(padjsub, !is.na(Method1)))))
  dd2 <- length(setdiff(rownames(subset(truthsub, status == 0)),
                        rownames(subset(padjsub, !is.na(Method1)))))
  aa2 <- length(setdiff(rownames(subset(padjsub, Method1 <= 0.05)),
                        rownames(subset(truthsub, status %in% c(0, 1)))))
  bb2 <- length(setdiff(rownames(subset(padjsub, Method1 > 0.05)),
                        rownames(subset(truthsub, status %in% c(0, 1)))))
  ## True/false sign
  av <- sign(scoresub[rownames(subset(svalsub, Method1 <= 0.05)), "Method1"])
  bv <- sign(truthsub[rownames(subset(svalsub, Method1 <= 0.05)), "logFC"])
  kp <- which(!is.na(bv))
  av <- av[kp]
  bv <- bv[kp]
  av[is.na(av)] <- -bv[is.na(av)]
  ts2 <- sum(av == bv, na.rm = TRUE)
  fs2 <- sum(abs(av - bv) == 2, na.rm = TRUE)
  fsr2 <- fs2/length(av)
  fnbr2 <- length(av)
  
  test_that("Shared values in output objects are equal", {
    ibp1 <- prepare_data_for_plot(ib1, keepmethod = NULL, incloverall = TRUE,
                                  incltruth = TRUE)
    ibp1logical <- prepare_data_for_plot(ib1logical, keepmethod = NULL, 
                                         incloverall = TRUE, incltruth = TRUE)
    expect_equivalent(colSums(overlap(ibp1))["Method1"],
                      (subset(tpr(ib1), fullmethod == "Method1_overall" &
                                thr == "thr0.05"))[, "NBR"])
    expect_equivalent(colSums(overlap(ibp1logical))["Method1"],
                      (subset(tpr(ib1logical), fullmethod == "Method1_overall" &
                                thr == "thr0.05"))[, "NBR"])
    
    ## TP (fdrtpr, fdrnbr, tpr, fpr)
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"],
                 (subset(fdrnbr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"],
                 (subset(tpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"],
                 (subset(fpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"])
    
    expect_equal((subset(fdrtpr(ib1logical), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"],
                 (subset(fdrnbr(ib1logical), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"])
    expect_equal((subset(fdrtpr(ib1logical), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"],
                 (subset(tpr(ib1logical), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"])
    expect_equal((subset(fdrtpr(ib1logical), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"],
                 (subset(fpr(ib1logical), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"])
    
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TP"],
                 (subset(fdrnbr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TP"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TP"],
                 (subset(tpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TP"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TP"],
                 (subset(fpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TP"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"],
                 (subset(fdrnbr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"],
                 (subset(tpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"],
                 (subset(fpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"],
                 (subset(fdrtpr(ib2), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TP"])

    ## FP (fdrtpr, fdrnbr, tpr, fpr)
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FP"],
                 (subset(fdrnbr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FP"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FP"],
                 (subset(tpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FP"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FP"],
                 (subset(fpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FP"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "FP"],
                 (subset(fdrnbr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "FP"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "FP"],
                 (subset(tpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "FP"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "FP"],
                 (subset(fpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "FP"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FP"],
                 (subset(fdrnbr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FP"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FP"],
                 (subset(tpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FP"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FP"],
                 (subset(fpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FP"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FP"],
                 (subset(fdrtpr(ib2), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FP"])

    ## NBR (fdrtpr, fdrnbr, tpr, fpr, overlap)
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NBR"],
                 (subset(fdrnbr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NBR"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NBR"],
                 (subset(tpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NBR"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NBR"],
                 (subset(fpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NBR"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "NBR"],
                 (subset(fdrnbr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "NBR"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "NBR"],
                 (subset(tpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "NBR"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "NBR"],
                 (subset(fpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "NBR"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NBR"],
                 (subset(fdrnbr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NBR"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NBR"],
                 (subset(tpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NBR"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NBR"],
                 (subset(fpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NBR"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NBR"],
                 (subset(fdrtpr(ib2), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NBR"])

    ## TN (fdrtpr, fdrnbr, tpr, fpr)
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TN"],
                 (subset(fdrnbr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TN"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TN"],
                 (subset(tpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TN"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TN"],
                 (subset(fpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TN"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TN"],
                 (subset(fdrnbr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TN"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TN"],
                 (subset(tpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TN"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TN"],
                 (subset(fpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TN"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TN"],
                 (subset(fdrnbr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TN"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TN"],
                 (subset(tpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TN"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TN"],
                 (subset(fpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TN"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TN"],
                 (subset(fdrtpr(ib2), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TN"])

    ## FN (fdrtpr, fdrnbr, tpr, fpr)
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FN"],
                 (subset(fdrnbr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FN"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FN"],
                 (subset(tpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FN"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FN"],
                 (subset(fpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FN"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "FN"],
                 (subset(fdrnbr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "FN"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "FN"],
                 (subset(tpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "FN"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "FN"],
                 (subset(fpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "FN"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FN"],
                 (subset(fdrnbr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FN"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FN"],
                 (subset(tpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FN"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FN"],
                 (subset(fpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FN"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FN"],
                 (subset(fdrtpr(ib2), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FN"])

    ## TOT_CALLED (fdrtpr, fdrnbr, tpr, fpr)
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TOT_CALLED"],
                 (subset(fdrnbr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TOT_CALLED"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TOT_CALLED"],
                 (subset(tpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TOT_CALLED"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TOT_CALLED"],
                 (subset(fpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TOT_CALLED"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TOT_CALLED"],
                 (subset(fdrnbr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TOT_CALLED"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TOT_CALLED"],
                 (subset(tpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TOT_CALLED"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TOT_CALLED"],
                 (subset(fpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TOT_CALLED"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TOT_CALLED"],
                 (subset(fdrnbr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TOT_CALLED"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TOT_CALLED"],
                 (subset(tpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TOT_CALLED"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TOT_CALLED"],
                 (subset(fpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TOT_CALLED"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TOT_CALLED"],
                 (subset(fdrtpr(ib2), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TOT_CALLED"])

    ## DIFF (fdrtpr, fdrnbr, tpr, fpr)
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "DIFF"],
                 (subset(fdrnbr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "DIFF"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "DIFF"],
                 (subset(tpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "DIFF"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "DIFF"],
                 (subset(fpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "DIFF"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "DIFF"],
                 (subset(fdrnbr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "DIFF"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "DIFF"],
                 (subset(tpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "DIFF"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "DIFF"],
                 (subset(fpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "DIFF"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "DIFF"],
                 (subset(fdrnbr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "DIFF"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "DIFF"],
                 (subset(tpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "DIFF"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "DIFF"],
                 (subset(fpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "DIFF"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "DIFF"],
                 (subset(fdrtpr(ib2), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "DIFF"])

    ## NONDIFF (fdrtpr, fdrnbr, tpr, fpr)
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NONDIFF"],
                 (subset(fdrnbr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NONDIFF"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NONDIFF"],
                 (subset(tpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NONDIFF"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NONDIFF"],
                 (subset(fpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NONDIFF"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "NONDIFF"],
                 (subset(fdrnbr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "NONDIFF"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "NONDIFF"],
                 (subset(tpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "NONDIFF"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "NONDIFF"],
                 (subset(fpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "NONDIFF"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NONDIFF"],
                 (subset(fdrnbr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NONDIFF"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NONDIFF"],
                 (subset(tpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NONDIFF"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NONDIFF"],
                 (subset(fpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NONDIFF"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NONDIFF"],
                 (subset(fdrtpr(ib2), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "NONDIFF"])

    ## TPR (fdrtpr, fdrnbr, tpr)
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TPR"],
                 (subset(fdrnbr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TPR"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TPR"],
                 (subset(tpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TPR"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TPR"],
                 (subset(fdrnbr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TPR"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TPR"],
                 (subset(tpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "TPR"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TPR"],
                 (subset(fdrnbr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TPR"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TPR"],
                 (subset(tpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TPR"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TPR"],
                 (subset(fdrtpr(ib2), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "TPR"])

    ## FDR (fdrtpr, fdrnbr)
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FDR"],
                 (subset(fdrnbr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FDR"])
    expect_equal((subset(fdrtpr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "FDR"],
                 (subset(fdrnbr(ib2), fullmethod == "Method2_expr_cat:[   0.000,   0.362)" &
                           thr == "thr0.05"))[, "FDR"])
    expect_equal((subset(fdrtpr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FDR"],
                 (subset(fdrnbr(ib3), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FDR"])
    expect_equal((subset(fdrtpr(ib1), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FDR"],
                 (subset(fdrtpr(ib2), fullmethod == "Method2_overall" & thr == "thr0.05"))[, "FDR"])

    ## fdrtpr vs fdrtprcurve
    nbr1 <- (subset(fdrtpr(ib1), fullmethod == "Method1_overall" & thr == "thr0.05"))[, "NBR"]
    expect_equivalent((subset(fdrtpr(ib1), fullmethod == "Method1_overall" &
                                NBR == nbr1))[, c("NBR", "TP", "FP", "TN", "FN",
                                                  "TOT_CALLED", "DIFF", "NONDIFF", "TPR", "FDR")],
                      (subset(fdrtprcurve(ib1), fullmethod == "Method1_overall" &
                                NBR == nbr1))[, c("NBR", "TP", "FP", "TN", "FN",
                                                  "TOT_CALLED", "DIFF", "NONDIFF", "TPR", "FDR")])

    nbr1 <- (subset(fdrtpr(ib2), fullmethod == "Method1_expr_cat:[   0.000,   0.362)" &
                      thr == "thr0.05"))[, "NBR"]
    fdrtprcurve(ib2)$NBR <- round(fdrtprcurve(ib2)$NBR)
    expect_equivalent((subset(fdrtpr(ib2), fullmethod == "Method1_expr_cat:[   0.000,   0.362)" &
                                NBR == nbr1))[, c("NBR", "TP", "FP", "TN", "FN",
                                                  "TOT_CALLED", "DIFF", "NONDIFF", "TPR", "FDR")],
                      (subset(fdrtprcurve(ib2), fullmethod == "Method1_expr_cat:[   0.000,   0.362)" &
                                NBR == nbr1))[, c("NBR", "TP", "FP", "TN", "FN",
                                                  "TOT_CALLED", "DIFF", "NONDIFF", "TPR", "FDR")])

    nbr1 <- (subset(fdrtpr(ib3), fullmethod == "Method1_overall" & thr == "thr0.05"))[, "NBR"]
    expect_equivalent((subset(fdrtpr(ib3), fullmethod == "Method1_overall" &
                                NBR == nbr1))[, c("NBR", "TP", "FP", "TN", "FN",
                                                  "TOT_CALLED", "DIFF", "NONDIFF", "TPR", "FDR")],
                      (subset(fdrtprcurve(ib3), fullmethod == "Method1_overall" &
                                NBR == nbr1))[, c("NBR", "TP", "FP", "TN", "FN",
                                                  "TOT_CALLED", "DIFF", "NONDIFF", "TPR", "FDR")])

    ## fdrtprcurve and fdrnbrcurve data frames should be equal
    expect_equal(fdrtprcurve(ib1), fdrnbrcurve(ib1))
    expect_equal(fdrtprcurve(ib2), fdrnbrcurve(ib2))
    expect_equal(fdrtprcurve(ib3), fdrnbrcurve(ib3))

    ## fsrnbr vs fsrnbrcurve
    nbr1 <- (subset(fsrnbr(ib1), fullmethod == "Method1_overall" & thr == "thr0.05"))[, "NBR"]
    expect_equivalent((subset(fsrnbr(ib1), fullmethod == "Method1_overall" &
                                NBR == nbr1))[, c("NBR", "FSR", "FS", "TS", "POSSIGN",
                                                  "NEGSIGN", "ZEROSIGN", "TOT_CALLED")],
                      (subset(fsrnbrcurve(ib1), fullmethod == "Method1_overall" &
                                NBR == nbr1))[, c("NBR", "FSR", "FS", "TS", "POSSIGN",
                                                  "NEGSIGN", "ZEROSIGN", "TOT_CALLED")])
    
    nbr1 <- (subset(fsrnbr(ib2), fullmethod == "Method1_expr_cat:[   0.000,   0.362)" &
                      thr == "thr0.05"))[, "NBR"]
    fsrnbrcurve(ib2)$NBR <- round(fsrnbrcurve(ib2)$NBR)
    expect_equivalent((subset(fsrnbr(ib2), fullmethod == "Method1_expr_cat:[   0.000,   0.362)" &
                                NBR == nbr1))[, c("NBR", "FSR", "FS", "TS", "POSSIGN",
                                                  "NEGSIGN", "ZEROSIGN", "TOT_CALLED")],
                      (subset(fsrnbrcurve(ib2), fullmethod == "Method1_expr_cat:[   0.000,   0.362)" &
                                NBR == nbr1))[, c("NBR", "FSR", "FS", "TS", "POSSIGN",
                                                  "NEGSIGN", "ZEROSIGN", "TOT_CALLED")])
    
    nbr1 <- (subset(fsrnbr(ib3), fullmethod == "Method1_overall" & thr == "thr0.05"))[, "NBR"]
    expect_equivalent((subset(fsrnbr(ib3), fullmethod == "Method1_overall" &
                                NBR == nbr1))[, c("NBR", "FSR", "FS", "TS", "POSSIGN",
                                                  "NEGSIGN", "ZEROSIGN", "TOT_CALLED")],
                      (subset(fsrnbrcurve(ib3), fullmethod == "Method1_overall" &
                                NBR == nbr1))[, c("NBR", "FSR", "FS", "TS", "POSSIGN",
                                                  "NEGSIGN", "ZEROSIGN", "TOT_CALLED")])

    ## FPC vs FP
    nbr1 <- (subset(fpc(ib1), fullmethod == "Method1_overall"))[, "topN"][150]
    cut1 <- (subset(fpc(ib1), fullmethod == "Method1_overall" & topN == nbr1))[, "FPC_CUTOFF"]
    fp4 <- length(intersect(rownames(subset(pval, abs(Method1) <= cut1)), 
                            rownames(subset(truth, status == 0))))
    expect_equal(fp4, (subset(fpc(ib1), fullmethod == "Method1_overall" & topN == nbr1))[, "FP"])

    ## FPC vs FP
    nbr1 <- (subset(fpc(ib2), fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "topN"][150]
    cut1 <- (subset(fpc(ib2), fullmethod == "Method1_expr_cat:[   2.628,  17.148)" &
                      topN == nbr1))[, "FPC_CUTOFF"]
    fp4 <- length(intersect(rownames(subset(pvalsub, abs(Method1) <= cut1)),
                            rownames(subset(truthsub, status == 0))))
    expect_equal(fp4, (subset(fpc(ib2), fullmethod == "Method1_expr_cat:[   2.628,  17.148)" &
                                topN == nbr1))[, "FP"])

    ## FPC vs FP
    nbr1 <- (subset(fpc(ib3), fullmethod == "Method1_overall"))[, "topN"][150]
    cut1 <- (subset(fpc(ib3), fullmethod == "Method1_overall" & topN == nbr1))[, "FPC_CUTOFF"]
    fp4 <- length(intersect(rownames(subset(pval, abs(Method1) <= cut1)), 
                            rownames(subset(truth, status == 0))))
    expect_equal(fp4, (subset(fpc(ib3), fullmethod == "Method1_overall" & topN == nbr1))[, "FP"])

    ## ROC vs TPR/FPR
    tpr1 <- (subset(roc(ib1), fullmethod == "Method1_overall"))[, "TPR"][155]
    fpr1 <- (subset(roc(ib1), fullmethod == "Method1_overall" & TPR == tpr1))[, "FPR"]
    cut1 <- (subset(roc(ib1), fullmethod == "Method1_overall" & TPR == tpr1))[, "ROC_CUTOFF"]
    tp4 <- length(intersect(rownames(subset(pval, abs(Method1) <= cut1)), 
                            rownames(subset(truth, status == 1))))
    fp4 <- length(intersect(rownames(subset(pval, abs(Method1) <= cut1)), 
                            rownames(subset(truth, status == 0))))
    fn4 <- length(intersect(rownames(subset(pval, abs(Method1) > cut1)), 
                            rownames(subset(truth, status == 1))))
    tn4 <- length(intersect(rownames(subset(pval, abs(Method1) > cut1)), 
                            rownames(subset(truth, status == 0))))
    cc4 <- length(setdiff(rownames(subset(truth, status == 1)), rownames(subset(pval, !is.na(Method1)))))
    dd4 <- length(setdiff(rownames(subset(truth, status == 0)), rownames(subset(pval, !is.na(Method1)))))
    expect_equal(fpr1, fp4/(fp4 + tn4 + dd4))
    expect_equal(tpr1, tp4/(tp4 + fn4 + cc4))

    ## ROC vs TPR/FPR
    tpr1 <- (subset(roc(ib2), fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "TPR"][465]
    fpr1 <- (subset(roc(ib2), fullmethod == "Method1_expr_cat:[   2.628,  17.148)" & TPR == tpr1))[, "FPR"]
    cut1 <- (subset(roc(ib2), fullmethod == "Method1_expr_cat:[   2.628,  17.148)" & TPR == tpr1))[, "ROC_CUTOFF"]
    tp4 <- length(intersect(rownames(subset(pvalsub, abs(Method1) <= cut1)), 
                            rownames(subset(truthsub, status == 1))))
    fp4 <- length(intersect(rownames(subset(pvalsub, abs(Method1) <= cut1)), 
                            rownames(subset(truthsub, status == 0))))
    fn4 <- length(intersect(rownames(subset(pvalsub, abs(Method1) > cut1)), 
                            rownames(subset(truthsub, status == 1))))
    tn4 <- length(intersect(rownames(subset(pvalsub, abs(Method1) > cut1)), 
                            rownames(subset(truthsub, status == 0))))
    cc4 <- length(setdiff(rownames(subset(truthsub, status == 1)), rownames(subset(pvalsub, !is.na(Method1)))))
    dd4 <- length(setdiff(rownames(subset(truthsub, status == 0)), rownames(subset(pvalsub, !is.na(Method1)))))
    expect_equal(fpr1, fp4/(fp4 + tn4 + dd4))
    expect_equal(tpr1, tp4/(tp4 + fn4 + cc4))

    ## ROC vs TPR/FPR
    tpr1 <- (subset(roc(ib3), fullmethod == "Method1_overall"))[, "TPR"][155]
    fpr1 <- (subset(roc(ib3), fullmethod == "Method1_overall" & TPR == tpr1))[, "FPR"]
    cut1 <- (subset(roc(ib3), fullmethod == "Method1_overall" & TPR == tpr1))[, "ROC_CUTOFF"]
    tp4 <- length(intersect(rownames(subset(pval, abs(Method1) <= cut1)), 
                            rownames(subset(truth, status == 1))))
    fp4 <- length(intersect(rownames(subset(pval, abs(Method1) <= cut1)), 
                            rownames(subset(truth, status == 0))))
    fn4 <- length(intersect(rownames(subset(pval, abs(Method1) > cut1)), 
                            rownames(subset(truth, status == 1))))
    tn4 <- length(intersect(rownames(subset(pval, abs(Method1) > cut1)), 
                            rownames(subset(truth, status == 0))))
    cc4 <- length(setdiff(rownames(subset(truth, status == 1)), rownames(subset(pval, !is.na(Method1)))))
    dd4 <- length(setdiff(rownames(subset(truth, status == 0)), rownames(subset(pval, !is.na(Method1)))))
    expect_equal(fpr1, fp4/(fp4 + tn4))
    expect_equal(tpr1, tp4/(tp4 + fn4))
  })

  test_that("FSR values are correct", {
    ## No stratification, onlyshared = FALSE
    expect_equal(fsr1, (subset(fsrnbr(ib1), thr == "thr0.05" & basemethod == "Method1"))[, "FSR"])
    expect_equal(fnbr1,  (subset(fsrnbr(ib1), thr == "thr0.05" &
                                                   basemethod == "Method1"))[, "NBR"])
    
    ## Stratification, onlyshared = FALSE
    expect_equal(fsr2, (subset(fsrnbr(ib2), thr == "thr0.05" &
                                fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "FSR"])
    expect_equal(fnbr2,
                 (subset(fsrnbr(ib2), thr == "thr0.05" &
                           fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "NBR"])
    
    ## No stratification, onlyshared = TRUE
    expect_equal(fsr1, (subset(fsrnbr(ib3), thr == "thr0.05" & basemethod == "Method1"))[, "FSR"])
    expect_equal(fnbr1,
                 (subset(fsrnbr(ib3), thr == "thr0.05" & basemethod == "Method1"))[, "NBR"])
  })
  
  test_that("TPR values are correct", {
    ## No stratification, onlyshared = FALSE
    expect_equal(tp1, (subset(tpr(ib1), thr == "thr0.05" & basemethod == "Method1"))[, "TP"])
    expect_equal(tp1/(tp1 + fn1 + cc1),  (subset(tpr(ib1), thr == "thr0.05" &
                                                   basemethod == "Method1"))[, "TPR"])

    ## Stratification, onlyshared = FALSE
    expect_equal(tp2, (subset(tpr(ib2), thr == "thr0.05" &
                                fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "TP"])
    expect_equal(tp2/(tp2 + fn2 + cc2),
                 (subset(tpr(ib2), thr == "thr0.05" &
                           fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "TPR"])

    ## No stratification, onlyshared = TRUE
    expect_equal(tp1, (subset(tpr(ib3), thr == "thr0.05" & basemethod == "Method1"))[, "TP"])
    expect_equal(tp1/(tp1 + fn1),
                 (subset(tpr(ib3), thr == "thr0.05" & basemethod == "Method1"))[, "TPR"])
  })

  test_that("FPR values are correct", {
    ## No stratification, onlyshared = FALSE
    expect_equal(fp1, (subset(fpr(ib1), thr == "thr0.05" & basemethod == "Method1"))[, "FP"])
    expect_equal(fp1/(fp1 + tn1 + dd1),
                 (subset(fpr(ib1), thr == "thr0.05" & basemethod == "Method1"))[, "FPR"])

    ## Stratification, onlyshared = FALSE
    expect_equal(fp2, (subset(fpr(ib2), thr == "thr0.05" &
                                fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "FP"])
    expect_equal(fp2/(fp2 + tn2 + dd2),
                 (subset(fpr(ib2), thr == "thr0.05" &
                           fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "FPR"])

    ## No stratification, onlyshared = TRUE
    expect_equal(fp1, (subset(fpr(ib3), thr == "thr0.05" & basemethod == "Method1"))[, "FP"])
    expect_equal(fp1/(fp1 + tn1),
                 (subset(fpr(ib3), thr == "thr0.05" & basemethod == "Method1"))[, "FPR"])
  })

  test_that("FDR values are correct", {
    ## No stratification, onlyshared = FALSE
    expect_equal(fp1/(tp1 + fp1),
                 (subset(fdrtpr(ib1), thr == "thr0.05" & basemethod == "Method1"))[, "FDR"])

    ## Stratification, onlyshared = FALSE
    expect_equal(fp2/(tp2 + fp2),
                 (subset(fdrtpr(ib2), thr == "thr0.05" &
                           fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "FDR"])

    ## No stratification, onlyshared = TRUE
    expect_equal(fp1/(tp1 + fp1),
                 (subset(fdrtpr(ib3), thr == "thr0.05" & basemethod == "Method1"))[, "FDR"])
  })

  test_that("NBR values are correct", {
    ## No stratification, onlyshared = FALSE
    expect_equal(tp1 + fp1,
                 (subset(fdrnbr(ib1), thr == "thr0.05" & basemethod == "Method1"))[, "NBR"])

    ## Stratification, onlyshared = FALSE
    expect_equal(tp2 + fp2,
                 (subset(fdrnbr(ib2), thr == "thr0.05" &
                           fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "NBR"])

    ## No stratification, onlyshared = TRUE
    expect_equal(tp1 + fp1,
                 (subset(fdrtpr(ib3), thr == "thr0.05" & basemethod == "Method1"))[, "NBR"])
  })

  test_that("TN values are correct", {
    ## No stratification, onlyshared = FALSE
    expect_equal(tn1,  (subset(fdrtpr(ib1), thr == "thr0.05" & basemethod == "Method1"))[, "TN"])

    ## Stratification, onlyshared = FALSE
    expect_equal(tn2,  (subset(fdrtpr(ib2), thr == "thr0.05" &
                                 fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "TN"])

    ## No stratification, onlyshared = TRUE
    expect_equal(tn1,  (subset(fdrtpr(ib3), thr == "thr0.05" & basemethod == "Method1"))[, "TN"])
  })

  test_that("FN values are correct", {
    ## No stratification, onlyshared = FALSE
    expect_equal(fn1,  (subset(fdrtpr(ib1), thr == "thr0.05" & basemethod == "Method1"))[, "FN"])

    ## Stratification, onlyshared = FALSE
    expect_equal(fn2,  (subset(fdrtpr(ib2), thr == "thr0.05" &
                                 fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "FN"])

    ## No stratification, onlyshared = TRUE
    expect_equal(fn1,  (subset(fdrtpr(ib3), thr == "thr0.05" & basemethod == "Method1"))[, "FN"])
  })

  test_that("TOT_CALLED values are correct", {
    ## No stratification, onlyshared = FALSE
    expect_equal(tp1 + fp1 + fn1 + tn1,
                 (subset(fdrtpr(ib1), thr == "thr0.05" & basemethod == "Method1"))[, "TOT_CALLED"])

    ## Stratification, onlyshared = FALSE
    expect_equal(tp2 + fp2 + fn2 + tn2,
                 (subset(fdrtpr(ib2), thr == "thr0.05" &
                           fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "TOT_CALLED"])

    ## No stratification, onlyshared = TRUE
    expect_equal(tp1 + fp1 + fn1 + tn1,
                 (subset(fdrtpr(ib3), thr == "thr0.05" & basemethod == "Method1"))[, "TOT_CALLED"])
  })

  test_that("DIFF values are correct", {
    ## No stratification, onlyshared = FALSE
    expect_equal(tp1 + fn1 + cc1,
                 (subset(fdrtpr(ib1), thr == "thr0.05" & basemethod == "Method1"))[, "DIFF"])

    ## Stratification, onlyshared = FALSE
    expect_equal(tp2 + fn2 + cc2,
                 (subset(fdrtpr(ib2), thr == "thr0.05" &
                           fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "DIFF"])

    ## No stratification, onlyshared = TRUE
    expect_equal(tp1 + fn1,
                 (subset(fdrtpr(ib3), thr == "thr0.05" & basemethod == "Method1"))[, "DIFF"])
  })

  test_that("NONDIFF values are correct", {
    ## No stratification, onlyshared = FALSE
    expect_equal(fp1 + tn1 + dd1,
                 (subset(fdrtpr(ib1), thr == "thr0.05" & basemethod == "Method1"))[, "NONDIFF"])

    ## Stratification, onlyshared = FALSE
    expect_equal(fp2 + tn2 + dd2,
                 (subset(fdrtpr(ib2), thr == "thr0.05" &
                           fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "NONDIFF"])

    ## No stratification, onlyshared = TRUE
    expect_equal(fp1 + tn1,
                 (subset(fdrtpr(ib3), thr == "thr0.05" & basemethod == "Method1"))[, "NONDIFF"])
  })

  test_that("CORR values are correct", {
    ## No stratification, onlyshared = FALSE
    s1 <- score$Method1[match(rownames(truth), rownames(score))]
    s2 <- truth$logFC
    expect_equal(cor(s1, s2, use = "complete.obs", method = "pearson"),
                 (subset(corr(ib1), fullmethod == "Method1_overall"))[, "PEARSON"])
    expect_equal(cor(s1, s2, use = "complete.obs", method = "spearman"),
                 (subset(corr(ib1), fullmethod == "Method1_overall"))[, "SPEARMAN"])

    ## Stratification, onlyshared = FALSE
    s1 <- scoresub$Method1[match(rownames(truthsub), rownames(scoresub))]
    s2 <- truthsub$logFC
    expect_equal(cor(s1, s2, use = "complete.obs", method = "pearson"),
                 (subset(corr(ib2), fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "PEARSON"])
    expect_equal(cor(s1, s2, use = "complete.obs", method = "spearman"),
                 (subset(corr(ib2), fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "SPEARMAN"])

    ## No stratification, onlyshared = TRUE
    kg <- intersect(rownames(truth), rownames(score))
    s1 <- score$Method1[match(kg, rownames(score))]
    s2 <- truth$logFC[match(kg, rownames(truth))]
    expect_equal(cor(s1, s2, use = "complete.obs", method = "pearson"),
                 (subset(corr(ib3), fullmethod == "Method1_overall"))[, "PEARSON"])
    expect_equal(cor(s1, s2, use = "complete.obs", method = "spearman"),
                 (subset(corr(ib3), fullmethod == "Method1_overall"))[, "SPEARMAN"])
  })

  test_that("DEVIATION values are correct", {
    ## No stratification, onlyshared = FALSE
    feat <- "ENSG00000001460"
    expect_equal(score[feat, "Method1"] - truth[feat, "logFC"],
                 (subset(deviation(ib1), feature == feat & fullmethod == "Method1_overall"))[, "DEVIATION"])

    ## Stratification, onlyshared = FALSE
    expect_equal(scoresub[feat, "Method1"] - truthsub[feat, "logFC"],
                 (subset(deviation(ib2), feature == feat &
                           fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "DEVIATION"])

    ## No stratification, onlyshared = TRUE
    expect_equal(score[feat, "Method1"] - truth[feat, "logFC"],
                 (subset(deviation(ib3), feature == feat & fullmethod == "Method1_overall"))[, "DEVIATION"])
  })

  test_that("SCATTER values are correct", {
    ## No stratification, onlyshared = FALSE
    feat <- "ENSG00000001460"
    expect_equal(score[feat, "Method1"],
                 (subset(scatter(ib1), feature == feat & fullmethod == "Method1_overall"))[, "OBSERVATION"])
    expect_equal(truth[feat, "logFC"],
                 (subset(scatter(ib1), feature == feat & fullmethod == "Method1_overall"))[, "TRUTH"])

    ## Stratification, onlyshared = FALSE
    expect_equal(scoresub[feat, "Method1"],
                 (subset(scatter(ib2), feature == feat &
                           fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "OBSERVATION"])
    expect_equal(truthsub[feat, "logFC"],
                 (subset(scatter(ib2), feature == feat &
                           fullmethod == "Method1_expr_cat:[   2.628,  17.148)"))[, "TRUTH"])

    ## No stratification, onlyshared = TRUE
    expect_equal(score[feat, "Method1"],
                 (subset(scatter(ib3), feature == feat & fullmethod == "Method1_overall"))[, "OBSERVATION"])
    expect_equal(truth[feat, "logFC"],
                 (subset(scatter(ib3), feature == feat & fullmethod == "Method1_overall"))[, "TRUTH"])
  })

  test_that("DEVIATION and SCATTER values correspond to each other", {
    ## No stratification, onlyshared = FALSE
    m <- merge(deviation(ib1), scatter(ib1))
    expect_equal(m$DEVIATION, m$OBSERVATION - m$TRUTH)

    ## Stratification, onlyshared = FALSE
    m <- merge(deviation(ib2), scatter(ib2))
    expect_equal(m$DEVIATION, m$OBSERVATION - m$TRUTH)

    ## No stratification, onlyshared = TRUE
    m <- merge(deviation(ib3), scatter(ib3))
    expect_equal(m$DEVIATION, m$OBSERVATION - m$TRUTH)
  })
})

test_that("getcurve returns NULL if not both positive and negative instances are present", {
  cobradata <- cobradata_example_sval
  truth(cobradata)$status <- 0
  cobraperf <- calculate_performance(cobradata, binary_truth <- "status",
                                     aspects = "fdrtprcurve")
  expect_equal(length(fdrtprcurve(cobraperf)), 0)
  cobraperf2 <- calculate_performance(cobradata, binary_truth = "status",
                                      aspects = "fdrtprcurve", splv = "expr_cat")
  expect_equal(length(fdrtprcurve(cobraperf2)), 0)
})

test_that("calculate_performance without score works", {
  cobradata <- cobradata_example_sval
  score(cobradata) <- data.frame()
  ib1 <- calculate_performance(cobradata, binary_truth = "status",
                               cont_truth = "logFC",
                               thrs = c(0.05), splv = "none", onlyshared = FALSE,
                               thr_venn = 0.05)
  expect_is(ib1, "COBRAPerformance")
})

test_that("calculate_performance without adj.p works", {
  cobradata <- cobradata_example_sval
  padj(cobradata) <- data.frame()
  ib1 <- calculate_performance(cobradata, binary_truth = "status",
                               cont_truth = "logFC",
                               thrs = c(0.05), splv = "none", onlyshared = FALSE,
                               thr_venn = 0.05)
  expect_is(ib1, "COBRAPerformance")
})

test_that("calculate_performance with only adj.p works", {
  cobradata <- cobradata_example_sval
  pval(cobradata) <- data.frame()
  score(cobradata) <- data.frame()
  ib1 <- calculate_performance(cobradata, binary_truth = "status",
                               cont_truth = "logFC", aspects = "fdrtprcurve",
                               thrs = c(0.05), splv = "none", onlyshared = FALSE,
                               thr_venn = 0.05)
  expect_is(ib1, "COBRAPerformance")
})

test_that("calculate_performance with empty input works", {
  cobradata <- cobradata_example_sval
  padj(cobradata) <- data.frame()
  pval(cobradata) <- data.frame()
  score(cobradata) <- data.frame()
  ib1 <- calculate_performance(cobradata, binary_truth = "status",
                               cont_truth = "logFC",
                               thrs = c(0.05), splv = "none", onlyshared = FALSE,
                               thr_venn = 0.05)
  expect_is(ib1, "COBRAPerformance")
  expect_equal(basemethods(ib1), character(0))
})

test_that("calculate_performance without significant features works", {
  cobradata <- cobradata_example_sval
  padj(cobradata) <- data.frame(Method1 = rep(1, nrow(padj(cobradata))),
                                Method2 = rep(1, nrow(padj(cobradata))),
                                Method3 = rep(1, nrow(padj(cobradata))),
                                row.names = rownames(padj(cobradata)))
  ib1 <- calculate_performance(cobradata, binary_truth = "status",
                               cont_truth = "logFC",
                               aspects = c("fdrtpr"),
                               thrs = c(0.05), splv = "none", onlyshared = FALSE,
                               thr_venn = 0.05)

  expect_is(ib1, "COBRAPerformance")
  expect_equal(fdrtpr(ib1)$FDR, rep(0, 3))

  ib1 <- calculate_performance(cobradata, binary_truth = "status",
                               cont_truth = "logFC",
                               aspects = c("fdrtpr"),
                               thrs = c(0.05), splv = "expr_cat",
                               onlyshared = FALSE,
                               thr_venn = 0.05)

  expect_is(ib1, "COBRAPerformance")
  expect_equal(fdrtpr(ib1)$FDR, rep(0, 12))
})

test_that("overlap calculations are correct", {
  cobradata <- cobradata_example_sval
  truth(cobradata) <- truth(cobradata)[1:3000, , drop = FALSE]
  ## Set some adjusted p-values to NA
  tmp <- padj(cobradata)$Method1
  tmp[sample(1:length(tmp), 5)] <- NA
  padj(cobradata)$Method1 <- tmp
  padj <- padj(cobradata)
  truth <- truth(cobradata)

  ## onlyshared = FALSE, incltruth = FALSE
  ib1 <- calculate_performance(cobradata, binary_truth = "status",
                               aspects = "overlap", splv = "none",
                               onlyshared = FALSE, thr_venn = 0.05)
  ib1 <- prepare_data_for_plot(ib1, incltruth = FALSE)
  expect_equal(length(intersect(which(padj$Method1 <= 0.05),
                                which(padj$Method2 <= 0.05))),
               sum(overlap(ib1)$Method1 * overlap(ib1)$Method2))

  ## onlyshared = TRUE, incltruth = FALSE
  ib1 <- calculate_performance(cobradata, binary_truth = "status",
                               aspects = "overlap", splv = "none",
                               onlyshared = TRUE, thr_venn = 0.05)
  ib1 <- prepare_data_for_plot(ib1, incltruth = FALSE)
  expect_equal(length(intersect(which(padj$Method1 <= 0.05),
                                which(padj$Method2 <= 0.05))),
               sum(overlap(ib1)$Method1 * overlap(ib1)$Method2))

  ## onlyshared = FALSE, incltruth = TRUE
  ib1 <- calculate_performance(cobradata, binary_truth = "status",
                               aspects = "overlap", splv = "none",
                               onlyshared = FALSE, thr_venn = 0.05)
  ib1 <- prepare_data_for_plot(ib1, incltruth = TRUE)
  padj1 <- padj[match(rownames(truth)[which(!is.na(truth$status))],
                      rownames(padj)), ]
  rownames(padj1) <- rownames(truth)
  expect_equal(length(intersect(which(padj1$Method1 <= 0.05),
                                which(padj1$Method2 <= 0.05))),
               sum(overlap(ib1)$Method1 * overlap(ib1)$Method2))
  expect_equal(length(setdiff(which(padj1$Method2 <= 0.05), which(padj1$Method1 <= 0.05))),
               length(setdiff(which(overlap(ib1)$Method2 == 1), which(overlap(ib1)$Method1 == 1))))

  ## onlyshared = TRUE, incltruth = TRUE
  ib1 <- calculate_performance(cobradata, binary_truth = "status",
                               aspects = "overlap", splv = "none",
                               onlyshared = TRUE, thr_venn = 0.05)
  ib1 <- prepare_data_for_plot(ib1, incltruth = TRUE)
  kp <- intersect(rownames(truth)[which(!is.na(truth$status))],
                  rownames(padj)[which(rowSums(is.na(padj)) == 0)])
  padj1 <- padj[match(kp, rownames(padj)), ]
  expect_equal(length(intersect(which(padj1$Method1 <= 0.05),
                                which(padj1$Method2 <= 0.05))),
               sum(overlap(ib1)$Method1 * overlap(ib1)$Method2))
  expect_equal(length(setdiff(which(padj1$Method2 <= 0.05), which(padj1$Method1 <= 0.05))),
               length(setdiff(which(overlap(ib1)$Method2 == 1), which(overlap(ib1)$Method1 == 1))))

  ## onlyshared = TRUE, incltruth = TRUE, stratify
  ib1 <- calculate_performance(cobradata, binary_truth = "status",
                               aspects = c("tpr", "overlap"), splv = "expr_cat",
                               onlyshared = TRUE, thr_venn = 0.05)
  ib1 <- prepare_data_for_plot(ib1, incltruth = TRUE)
  kp <- intersect(rownames(truth)[which(!is.na(truth$status))],
                  rownames(padj)[which(rowSums(is.na(padj)) == 0)])
  padj1 <- padj[match(kp, rownames(padj)), ]
  expect_equal(length(intersect(which(padj1$Method1 <= 0.05),
                                which(padj1$Method2 <= 0.05))),
               sum(overlap(ib1)$overall$Method1 * overlap(ib1)$overall$Method2))
  expect_equal(length(setdiff(which(padj1$Method2 <= 0.05), which(padj1$Method1 <= 0.05))),
               length(setdiff(which(overlap(ib1)$overall$Method2 == 1), which(overlap(ib1)$overall$Method1 == 1))))

  ib2 <- prepare_data_for_plot(ib1, incltruth = TRUE, incloverall = FALSE)
  expect_is(ib2, "COBRAPlot")
})

test_that("overlap calculations based on rank are correct", {
  cobradata <- cobradata_example_sval
  truth(cobradata) <- truth(cobradata)[1:3000, , drop = FALSE]
  ## Set some adjusted p-values to NA
  tmp <- padj(cobradata)$Method1
  tmp[sample(1:length(tmp), 5)] <- NA
  padj(cobradata)$Method1 <- tmp
  padj <- padj(cobradata)
  truth <- truth(cobradata)
  
  ## onlyshared = FALSE, incltruth = FALSE
  ## all features used. missing adjp considered nonsignificant
  ib1 <- calculate_performance(cobradata, binary_truth = "status",
                               aspects = "overlap", splv = "none",
                               onlyshared = FALSE, topn_venn = 100, 
                               type_venn = "rank")
  ib1 <- prepare_data_for_plot(ib1, incltruth = FALSE)
  expect_equal(length(intersect(order(padj$Method1)[1:100],
                                order(padj$Method2)[1:100])),
               sum(overlap(ib1)$Method1 * overlap(ib1)$Method2))
  
  ## onlyshared = TRUE, incltruth = FALSE
  ## all features used. missing adjp considered nonsignificant
  ib1 <- calculate_performance(cobradata, binary_truth = "status",
                               aspects = "overlap", splv = "none",
                               onlyshared = TRUE, topn_venn = 100,
                               type_venn = "rank")
  ib1 <- prepare_data_for_plot(ib1, incltruth = FALSE)
  expect_equal(length(intersect(order(padj$Method1)[1:100],
                                order(padj$Method2)[1:100])),
               sum(overlap(ib1)$Method1 * overlap(ib1)$Method2))
  
  ib2 <- prepare_data_for_plot(ib1, incltruth = FALSE, incloverall = FALSE)
  expect_is(ib2, "COBRAPlot")
})
