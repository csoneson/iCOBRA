## Test that the constructors generate objects of the correct type

library(IBRA)
context("Check that constructors generate objects of the correct type")

test_that("IBRAData constructors generate IBRAData objects", {
  expect_is(IBRAData(), "IBRAData")
  expect_is(IBRAData_from_text(
    truth_file = system.file("extdata", "ibradata_example_truth.txt",
                             package = "IBRA"),
    result_files = system.file("extdata", "ibradata_example_results.txt",
                               package = "IBRA"),
    feature_id = "feature"), "IBRAData")

  load(system.file("extdata", "ibradata_example_simres.Rdata",
                   package = "IBRA"))
  expect_is(IBRAData_from_simresults(simres), "IBRAData")

  rownames(simres@padj) <- NULL
  expect_is(IBRAData_from_simresults(simres), "IBRAData")

  load(system.file("extdata", "ibradata_example_simres.Rdata",
                   package = "IBRA"))
  rownames(simres@pval) <- NULL
  expect_is(IBRAData_from_simresults(simres), "IBRAData")

  rownames(simres@padj) <- NULL
  expect_is(IBRAData_from_simresults(simres), "IBRAData")

  expect_is(IBRAData_from_text(
    truth_file = system.file("extdata", "ibradata_example_truth.txt",
                             package = "IBRA"),
    result_files = system.file("extdata", "ibradata_example_results_2.txt",
                               package = "IBRA"),
    feature_id = "feature"), "IBRAData")

  ibt <- IBRAData_from_text(
    truth_file = system.file("extdata", "ibradata_example_truth.txt",
                             package = "IBRA"),
    result_files = system.file("extdata", "ibradata_example_results_3.txt",
                               package = "IBRA"),
    feature_id = "feature")
  expect_is(ibt, "IBRAData")
  expect_equal(length(pval(ibt)), 0)
  expect_equal(length(padj(ibt)), 0)
  expect_equal(length(score(ibt)), 0)

  pval <- c(0.0058, 0.771, 0.024, 0.741, 0.247)
  padj <- p.adjust(pval, method = "BH")
  score <- 1:5
  ib <- IBRAData(pval = data.frame(m1 = pval, row.names = paste0("F", 1:5)),
                 padj = data.frame(m1 = padj, row.names = paste0("F", 1:5)),
                 score = data.frame(m1 = score, row.names = paste0("F", 1:5)),
                 truth = data.frame(status = c(0, 1, 0, 1, 0),
                                    row.names = paste0("F", 1:5)))
  ib2 <- IBRAData(pval = data.frame(m2 = pval, row.names = paste0("F", 1:5)),
                  padj = data.frame(m2 = padj, row.names = paste0("F", 1:5)),
                  score = data.frame(m2 = score, row.names = paste0("F", 1:5)),
                  object_to_extend = ib)
  expect_equal(pval(ib2)$m1, pval(ib2)$m2)
  expect_equal(padj(ib2)$m1, padj(ib2)$m2)
  expect_equal(score(ib2)$m1, score(ib2)$m2)
})

test_that("export functions return correct class", {
  expect_is(IBRAData_to_text(ibradata_example, feature_id = "feature",
                             truth_file = "test_truth.txt",
                             result_files = "test_results.txt"), "NULL")
  expect_is(IBRAData_to_simresults(ibradata_example, binary_truth = "status",
                                   strat = NULL), "SimResults")
  ibradata <- ibradata_example
  ibradata <- calculate_adjp(ibradata)
  expect_is(IBRAData_to_simresults(ibradata, binary_truth = "status",
                                   strat = "expr_cat"), "SimResults")
})

test_that(paste0("IBRAPerformance constructor and calculate_performance ",
                 "generate IBRAPerformance objects"), {
                   expect_is(IBRAPerformance(), "IBRAPerformance")
                   expect_is(calculate_performance(ibradata_example,
                                                   binary_truth = "status",
                                                   aspects = "fpr"),
                             "IBRAPerformance")
})

test_that(paste0("IBRAPlot constructor and prepare_data_for_plot ",
                 "generate IBRAPlot objects"), {
                   expect_is(IBRAPlot(), "IBRAPlot")
                   expect_is(
                     prepare_data_for_plot(
                       calculate_performance(ibradata_example,
                                             binary_truth = "status",
                                             aspects = "fpr")), "IBRAPlot")
})

test_that("replacement still returns valid objects", {
  ibradata <- ibradata_example
  pval(ibradata) <- pval(ibradata)[1:500, ]
  padj(ibradata) <- padj(ibradata)[1:1000, ]
  score(ibradata) <- score(ibradata)[1:750, ]
  truth(ibradata) <- truth(ibradata)[1:500, ]
  expect_is(ibradata, "IBRAData")

  ibraperf <- calculate_performance(ibradata, binary_truth = "status",
                                    cont_truth = "logFC")
  fdrtpr(ibraperf) <- fdrtpr(ibraperf)[1:2, ]
  fdrtprcurve(ibraperf) <- fdrtprcurve(ibraperf)[1:2, ]
  fdrnbr(ibraperf) <- fdrnbr(ibraperf)[1:2, ]
  fdrnbrcurve(ibraperf) <- fdrnbrcurve(ibraperf)[1:2, ]
  fpr(ibraperf) <- fpr(ibraperf)[1:2, ]
  tpr(ibraperf) <- tpr(ibraperf)[1:2, ]
  roc(ibraperf) <- roc(ibraperf)[1:2, ]
  fpc(ibraperf) <- fpc(ibraperf)[1:2, ]
  deviation(ibraperf) <- deviation(ibraperf)[1:2, ]
  scatter(ibraperf) <- scatter(ibraperf)[1:2, ]
  fdrtpr(ibraperf) <- fdrtpr(ibraperf)[1:2, ]
  overlap(ibraperf) <- overlap(ibraperf)[1:2, ]
  corr(ibraperf) <- corr(ibraperf)[1:2, ]
  maxsplit(ibraperf) <- 4
  splv(ibraperf) <- "none"
  expect_is(ibraperf, "IBRAPerformance")

  ibraplot <- prepare_data_for_plot(ibraperf)
  plotcolors(ibraplot) <- plotcolors(ibraplot)[1:5]
  facetted(ibraplot) <- FALSE
  expect_true(IBRA:::is_plottable(tpr(ibraplot)))
})

test_that("extending an object still returns valid objects", {
  ibradata <- ibradata_example
  ibradata <- IBRAData(pval = pval(ibradata), object_to_extend = ibradata)
  expect_is(ibradata, "IBRAData")

  ibraperf <- calculate_performance(ibradata, binary_truth = "status",
                                    cont_truth = "logFC")
  ibraperf <- IBRAPerformance(maxsplit = 4, object_to_extend = ibraperf)
  expect_is(ibraperf, "IBRAPerformance")

  ibraplot <- prepare_data_for_plot(ibraperf)
  ibraplot <- IBRAPlot(maxsplit = 4, object_to_extend = ibraplot)
  expect_is(ibraplot, "IBRAPlot")
})

test_that("extending an object of the wrong class doesn't work", {
  ibradata <- ibradata_example
  ibraperf <- calculate_performance(ibradata, binary_truth = "status",
                                    cont_truth = "logFC", aspects = "tpr")
  ibraplot <- prepare_data_for_plot(ibraperf)
  expect_error(IBRAData(pval = pval(ibradata), object_to_extend = ibraplot))
  expect_error(IBRAPerformance(maxsplit = 4, object_to_extend = ibradata))
  expect_error(IBRAPlot(maxsplit = 4, object_to_extend = ibraperf))
})

test_that("show returns NULL", {
  ibradata <- ibradata_example
  ibraperf <- calculate_performance(ibradata, binary_truth = "status",
                                    cont_truth = "logFC", aspects = "tpr")
  ibraperf2 <- calculate_performance(ibradata, binary_truth = "status",
                                     cont_truth = "logFC",
                                     aspects = c("tpr", "overlap"),
                                     splv = "expr_cat", maxsplit = 4)
  ibraplot <- prepare_data_for_plot(ibraperf2, colorscheme = "Set3",
                                    facetted = FALSE)
  expect_is(show(ibradata), "NULL")
  expect_is(show(ibraperf), "NULL")
  expect_is(show(ibraperf2), "NULL")
  expect_is(show(ibraplot), "NULL")
})

test_that("subsetting of objects works", {
  ibradata <- ibradata_example
  ibraperf <- calculate_performance(ibradata, binary_truth = "status",
                                    cont_truth = "logFC", aspects = "tpr")
  ibraplot <- prepare_data_for_plot(ibraperf)
  expect_error(ibraplot[, "nonexisting_method"])
  expect_is(ibraplot[, "voom"], "IBRAPlot")
  expect_equal(tpr(ibraplot[, "voom"])$basemethod, rep("voom", 3))

  ibraperf <- calculate_performance(ibradata, binary_truth = "status",
                                    cont_truth = "logFC", aspects = "overlap",
                                    splv = "none")
  ibraplot <- prepare_data_for_plot(ibraperf)
  expect_error(ibraperf[, "nonexisting_method"])
  expect_is(overlap(ibraperf[, "voom"]), "data.frame")
  expect_is(overlap(ibraplot[, "voom"]), "data.frame")

  ibraperf <- calculate_performance(ibradata, binary_truth = "status",
                                    cont_truth = "logFC", aspects = "overlap",
                                    splv = "expr_cat")
  ibraplot <- prepare_data_for_plot(ibraperf)
  expect_error(ibraperf[, "nonexisting_method"])
  expect_is(ibraperf[, "voom"], "IBRAPerformance")
  expect_is(overlap(ibraperf[, "voom"]), "list")
  expect_equal(colnames(overlap(ibraperf[, "voom"])[[1]]), c("voom", "truth"))

  expect_error(ibraplot[, "nonexisting_method"])
  expect_is(ibraplot[, "voom"], "IBRAPlot")
  expect_is(overlap(ibraplot[, "voom"]), "list")
  expect_equal(colnames(overlap(ibraplot[, "voom"])[[1]]), c("voom", "truth"))

  kp <- rownames(pval(ibradata))[1:50]
  expect_error(ibradata["nonexisting_variable", ])
  expect_is(ibradata[kp, ], "IBRAData")
  expect_error(ibraplot[kp, ])
  expect_equal(nrow(pval(ibradata[kp, ])), length(kp))
})
