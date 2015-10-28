## Test that the constructors generate objects of the correct type

library(COBRA)
context("Check that constructors generate objects of the correct type")

test_that("COBRAData constructors generate COBRAData objects", {
  expect_is(COBRAData(), "COBRAData")
  expect_is(COBRAData_from_text(
    truth_file = system.file("extdata", "cobradata_example_truth.txt",
                             package = "COBRA"),
    result_files = system.file("extdata", "cobradata_example_results.txt",
                               package = "COBRA"),
    feature_id = "feature"), "COBRAData")

  load(system.file("extdata", "cobradata_example_simres.Rdata",
                   package = "COBRA"))
  expect_is(COBRAData_from_simresults(simres), "COBRAData")

  rownames(simres@padj) <- NULL
  expect_is(COBRAData_from_simresults(simres), "COBRAData")

  load(system.file("extdata", "cobradata_example_simres.Rdata",
                   package = "COBRA"))
  rownames(simres@pval) <- NULL
  expect_is(COBRAData_from_simresults(simres), "COBRAData")

  rownames(simres@padj) <- NULL
  expect_is(COBRAData_from_simresults(simres), "COBRAData")

  expect_is(COBRAData_from_text(
    truth_file = system.file("extdata", "cobradata_example_truth.txt",
                             package = "COBRA"),
    result_files = system.file("extdata", "cobradata_example_results_2.txt",
                               package = "COBRA"),
    feature_id = "feature"), "COBRAData")

  ibt <- COBRAData_from_text(
    truth_file = system.file("extdata", "cobradata_example_truth.txt",
                             package = "COBRA"),
    result_files = system.file("extdata", "cobradata_example_results_3.txt",
                               package = "COBRA"),
    feature_id = "feature")
  expect_is(ibt, "COBRAData")
  expect_equal(length(pval(ibt)), 0)
  expect_equal(length(padj(ibt)), 0)
  expect_equal(length(score(ibt)), 0)

  pval <- c(0.0058, 0.771, 0.024, 0.741, 0.247)
  padj <- p.adjust(pval, method = "BH")
  score <- 1:5
  ib <- COBRAData(pval = data.frame(m1 = pval, row.names = paste0("F", 1:5)),
                 padj = data.frame(m1 = padj, row.names = paste0("F", 1:5)),
                 score = data.frame(m1 = score, row.names = paste0("F", 1:5)),
                 truth = data.frame(status = c(0, 1, 0, 1, 0),
                                    row.names = paste0("F", 1:5)))
  ib2 <- COBRAData(pval = data.frame(m2 = pval, row.names = paste0("F", 1:5)),
                  padj = data.frame(m2 = padj, row.names = paste0("F", 1:5)),
                  score = data.frame(m2 = score, row.names = paste0("F", 1:5)),
                  object_to_extend = ib)
  expect_equal(pval(ib2)$m1, pval(ib2)$m2)
  expect_equal(padj(ib2)$m1, padj(ib2)$m2)
  expect_equal(score(ib2)$m1, score(ib2)$m2)
})

test_that("extending COBRAData objects works as expected", {
  ibA <- COBRAData(pval = data.frame(mA = c(0.1, 0.2, 0.3, 0.4, 0.5),
                                    row.names = paste0("F", 1:5)),
                  padj = data.frame(mA = c(0.1, 0.2, 0.3, 0.4, 0.5),
                                    row.names = paste0("F", 1:5)),
                  score = data.frame(mA = c(0.1, 0.2, 0.3, 0.4, 0.5),
                                     row.names = paste0("F", 1:5)),
                  truth = data.frame(status = c(1, 0, 1, 0, 1),
                                     row.names = paste0("F", 1:5)))
  ibAp <- ibA
  truth(ibAp)$expr <- 1:5
  truth(ibAp)$status <- c(0, 1, 0, 1, 0)
  ibB <- COBRAData(pval = data.frame(mB = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                                    row.names = paste0("F", 1:6)),
                  padj = data.frame(mA = c(0.6, 0.7, 0.8, 0.9, 1.0),
                                    row.names = paste0("F", 1:5)),
                  score = data.frame(mB = c(0.6, 0.7, 0.8),
                                     row.names = paste0("F", 1:3)),
                  truth = data.frame(status = c(1, 0, 1, 0, 1, 0),
                                     expr = 1:6,
                                     row.names = paste0("F", 1:6)))
  ibBp <- ibB
  truth(ibBp)$expr <- NULL
  truth(ibBp)$status <- c(0, 1, 0, 1, 0, 1)

  ibtest <- COBRAData(pval = pval(ibA), padj = padj(ibA), score = score(ibA),
                     truth = truth(ibA), object_to_extend = ibB)
  expect_equal(nrow(pval(ibtest)), 6)
  expect_equal(pval(ibtest)[paste0("F", 1:6), "mB"], c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
  expect_equal(nrow(padj(ibtest)), 5)
  expect_equal(ncol(padj(ibtest)), 1)
  expect_equal(padj(ibtest)[paste0("F", 1:5), "mA"], c(0.6, 0.7, 0.8, 0.9, 1.0))
  expect_equal(score(ibtest)[paste0("F", 1:5), "mB"], c(0.6, 0.7, 0.8, NA, NA))
  expect_equal(truth(ibtest)[paste0("F", 1:6), "expr"], 1:6)
  expect_equal(ncol(truth(ibtest)), 2)

  expect_error(COBRAData(pval = pval(ibA), padj = padj(ibA), score = score(ibA),
                        truth = truth(ibA), object_to_extend = ibBp))

  ibtestp <- COBRAData(pval = pval(ibBp), padj = padj(ibBp), score = score(ibBp),
                      truth = truth(ibBp), object_to_extend = ibAp)
  expect_equal(pval(ibtest)[paste0("F", 1:6), c("mA", "mB")],
               pval(ibtestp)[paste0("F", 1:6), c("mA", "mB")])
  expect_equal(padj(ibtestp)[paste0("F", 1:5), "mA"], c(0.1, 0.2, 0.3, 0.4, 0.5))
  expect_equal(score(ibtest)[paste0("F", 1:5), c("mA", "mB")],
               score(ibtestp)[paste0("F", 1:5), c("mA", "mB")])
  expect_equal(truth(ibtestp)[paste0("F", 1:6), "expr"], c(1:5, NA))
})

test_that("export functions return correct class", {
  expect_is(COBRAData_to_text(cobradata_example, feature_id = "feature",
                             truth_file = "test_truth.txt",
                             result_files = "test_results.txt"), "NULL")
  expect_is(COBRAData_to_simresults(cobradata_example, binary_truth = "status",
                                   strat = NULL), "SimResults")
  cobradata <- cobradata_example
  cobradata <- calculate_adjp(cobradata)
  expect_is(COBRAData_to_simresults(cobradata, binary_truth = "status",
                                   strat = "expr_cat"), "SimResults")
})

test_that(paste0("COBRAPerformance constructor and calculate_performance ",
                 "generate COBRAPerformance objects"), {
                   expect_is(COBRAPerformance(), "COBRAPerformance")
                   expect_is(calculate_performance(cobradata_example,
                                                   binary_truth = "status",
                                                   aspects = "fpr"),
                             "COBRAPerformance")
                   ip <- COBRAPerformance()
                   onlyshared(ip) <- TRUE
                   expect_is(ip, "COBRAPerformance")
})

test_that(paste0("COBRAPlot constructor and prepare_data_for_plot ",
                 "generate COBRAPlot objects"), {
                   expect_is(COBRAPlot(), "COBRAPlot")
                   expect_is(
                     prepare_data_for_plot(
                       calculate_performance(cobradata_example,
                                             binary_truth = "status",
                                             aspects = "fpr")), "COBRAPlot")
})

test_that("replacement still returns valid objects", {
  cobradata <- cobradata_example
  pval(cobradata) <- pval(cobradata)[1:500, ]
  padj(cobradata) <- padj(cobradata)[1:1000, ]
  score(cobradata) <- score(cobradata)[1:750, ]
  truth(cobradata) <- truth(cobradata)[1:500, ]
  expect_is(cobradata, "COBRAData")

  cobraperf <- calculate_performance(cobradata, binary_truth = "status",
                                    cont_truth = "logFC")
  fdrtpr(cobraperf) <- fdrtpr(cobraperf)[1:2, ]
  fdrtprcurve(cobraperf) <- fdrtprcurve(cobraperf)[1:2, ]
  fdrnbr(cobraperf) <- fdrnbr(cobraperf)[1:2, ]
  fdrnbrcurve(cobraperf) <- fdrnbrcurve(cobraperf)[1:2, ]
  fpr(cobraperf) <- fpr(cobraperf)[1:2, ]
  tpr(cobraperf) <- tpr(cobraperf)[1:2, ]
  roc(cobraperf) <- roc(cobraperf)[1:2, ]
  fpc(cobraperf) <- fpc(cobraperf)[1:2, ]
  deviation(cobraperf) <- deviation(cobraperf)[1:2, ]
  scatter(cobraperf) <- scatter(cobraperf)[1:2, ]
  fdrtpr(cobraperf) <- fdrtpr(cobraperf)[1:2, ]
  overlap(cobraperf) <- overlap(cobraperf)[1:2, ]
  corr(cobraperf) <- corr(cobraperf)[1:2, ]
  maxsplit(cobraperf) <- 4
  splv(cobraperf) <- "none"
  expect_is(cobraperf, "COBRAPerformance")

  cobraplot <- prepare_data_for_plot(cobraperf)
  plotcolors(cobraplot) <- plotcolors(cobraplot)[1:5]
  facetted(cobraplot) <- FALSE
  expect_true(COBRA:::is_plottable(tpr(cobraplot)))
})

test_that("show returns NULL", {
  cobradata <- cobradata_example
  cobraperf <- calculate_performance(cobradata, binary_truth = "status",
                                    cont_truth = "logFC", aspects = "tpr")
  cobraperf2 <- calculate_performance(cobradata, binary_truth = "status",
                                     cont_truth = "logFC",
                                     aspects = c("tpr", "overlap"),
                                     splv = "expr_cat", maxsplit = 4)
  cobraplot <- prepare_data_for_plot(cobraperf2, colorscheme = "Set3",
                                    facetted = FALSE)
  expect_is(show(cobradata), "NULL")
  expect_is(show(cobraperf), "NULL")
  expect_is(show(cobraperf2), "NULL")
  expect_is(show(cobraplot), "NULL")
})

test_that("subsetting of objects works", {
  cobradata <- cobradata_example
  cobraperf <- calculate_performance(cobradata, binary_truth = "status",
                                    cont_truth = "logFC", aspects = "tpr")
  cobraplot <- prepare_data_for_plot(cobraperf)
  expect_error(cobraplot[, "nonexisting_method"])
  expect_is(cobraplot[, "voom"], "COBRAPlot")
  expect_equal(tpr(cobraplot[, "voom"])$basemethod, rep("voom", 3))

  cobraperf <- calculate_performance(cobradata, binary_truth = "status",
                                    cont_truth = "logFC", aspects = "overlap",
                                    splv = "none")
  cobraplot <- prepare_data_for_plot(cobraperf)
  expect_error(cobraperf[, "nonexisting_method"])
  expect_is(overlap(cobraperf[, "voom"]), "data.frame")
  expect_is(overlap(cobraplot[, "voom"]), "data.frame")

  cobraperf <- calculate_performance(cobradata, binary_truth = "status",
                                    cont_truth = "logFC", aspects = "overlap",
                                    splv = "expr_cat")
  cobraplot <- prepare_data_for_plot(cobraperf)
  expect_error(cobraperf[, "nonexisting_method"])
  expect_is(cobraperf[, "voom"], "COBRAPerformance")
  expect_is(overlap(cobraperf[, "voom"]), "list")
  expect_equal(colnames(overlap(cobraperf[, "voom"])[[1]]), c("voom", "truth"))

  expect_error(cobraplot[, "nonexisting_method"])
  expect_is(cobraplot[, "voom"], "COBRAPlot")
  expect_is(overlap(cobraplot[, "voom"]), "list")
  expect_equal(colnames(overlap(cobraplot[, "voom"])[[1]]), c("voom", "truth"))

  kp <- rownames(pval(cobradata))[1:50]
  expect_error(cobradata["nonexisting_variable", ])
  expect_is(cobradata[kp, ], "COBRAData")
  expect_error(cobraplot[kp, ])
  expect_equal(nrow(pval(cobradata[kp, ])), length(kp))
})

test_that("extending an object still returns valid objects", {
  cobradata <- cobradata_example
  cobradata <- COBRAData(pval = pval(cobradata), object_to_extend = cobradata)
  expect_is(cobradata, "COBRAData")
})

test_that("extending an object of the wrong class doesn't work", {
    cobradata <- cobradata_example
    cobraperf <- calculate_performance(cobradata, binary_truth = "status",
                                      cont_truth = "logFC", aspects = "tpr")
    expect_error(COBRAData(pval = pval(cobradata), object_to_extend = cobraperf))
})
