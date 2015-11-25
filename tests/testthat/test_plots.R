## Test that plot functions do the right thing

library(iCOBRA)
context("Check plot functions")

local({
  ib1 <- calculate_performance(cobradata_example, binary_truth = "status",
                               cont_truth = "logFC",
                               aspects = c("fdrtpr", "fdrtprcurve", "fdrnbr",
                                           "fdrnbrcurve", "tpr", "fpr",
                                           "roc", "fpc", "overlap", "corr",
                                           "scatter", "deviation"),
                               thrs = c(0.05), splv = "none", onlyshared = FALSE,
                               thr_venn = 0.05)

  ibp1 <- prepare_data_for_plot(ib1, keepmethods = NULL, incloverall = TRUE,
                                colorscheme = "hue_pal", facetted = TRUE,
                                incltruth = TRUE)

  test_that("Plot functions return ggplot objects", {
    expect_is(plot_tpr(ibp1), "ggplot")
    expect_is(plot_corr(ibp1), "ggplot")
    expect_is(plot_deviation(ibp1), "ggplot")
    expect_is(plot_fdrnbrcurve(ibp1), "ggplot")
    expect_is(plot_fdrtprcurve(ibp1), "ggplot")
    expect_is(plot_fpc(ibp1), "ggplot")
    expect_is(plot_fpr(ibp1), "ggplot")
    expect_is(plot_corr(ibp1), "ggplot")
    expect_is(plot_roc(ibp1), "ggplot")
    expect_is(plot_scatter(ibp1), "ggplot")
    expect_is(plot_overlap(ibp1), "NULL")
  })

  ibp1 <- prepare_data_for_plot(ib1, keepmethods = NULL, incloverall = FALSE,
                                colorscheme = "hue_pal", facetted = FALSE,
                                incltruth = TRUE)

  test_that("Plot functions return ggplot objects if not facetted", {
    expect_is(plot_tpr(ibp1), "ggplot")
    expect_is(plot_corr(ibp1), "ggplot")
    expect_is(plot_deviation(ibp1), "ggplot")
    expect_is(plot_fdrnbrcurve(ibp1), "ggplot")
    expect_is(plot_fdrtprcurve(ibp1), "ggplot")
    expect_is(plot_fpc(ibp1), "ggplot")
    expect_is(plot_fpr(ibp1), "ggplot")
    expect_is(plot_corr(ibp1), "ggplot")
    expect_is(plot_roc(ibp1), "ggplot")
    expect_is(plot_scatter(ibp1), "ggplot")
    expect_is(plot_overlap(ibp1), "NULL")
  })
})

test_that("Plot functions return ggplot objects for empty input", {
  ib1 <- calculate_performance(cobradata_example, binary_truth = "status",
                               cont_truth = "logFC",
                               aspects = c("overlap", "tpr"),
                               thrs = c(0.05), splv = "none", onlyshared = FALSE,
                               thr_venn = 0.05)

  ibp1 <- prepare_data_for_plot(ib1, keepmethods = NULL, incloverall = TRUE,
                                colorscheme = c("blue", "green"), facetted = TRUE,
                                incltruth = FALSE)

  expect_is(plot_tpr(ibp1), "ggplot")
  expect_is(plot_corr(ibp1), "ggplot")
  expect_is(plot_deviation(ibp1), "ggplot")
  expect_is(plot_fdrtprcurve(ibp1), "ggplot")
  expect_is(plot_fdrtprcurve(ibp1, plottype = "curve"), "ggplot")
  expect_is(plot_fdrtprcurve(ibp1, plottype = "points"), "ggplot")
  expect_is(plot_fpr(ibp1), "ggplot")
  expect_is(plot_corr(ibp1), "ggplot")
  expect_is(plot_roc(ibp1), "ggplot")
  expect_is(plot_scatter(ibp1), "ggplot")
  expect_is(plot_overlap(ibp1), "NULL")
})

local({
  ib1 <- calculate_performance(cobradata_example, binary_truth = "status",
                               cont_truth = "logFC",
                               aspects = c("tpr"),
                               thrs = c(0.05), splv = "none", onlyshared = FALSE,
                               thr_venn = 0.05)

  ibp1 <- prepare_data_for_plot(ib1, keepmethods = NULL, incloverall = TRUE,
                                colorscheme = c("blue"), facetted = TRUE,
                                incltruth = TRUE)

  test_that("Plot functions return ggplot objects with only one color", {
    expect_is(plot_tpr(ibp1), "ggplot")
    expect_equal(length(plotcolors(ibp1)), 12)
    expect_equal(length(unique(plotcolors(ibp1))), 3)
  })

  ibp1 <- prepare_data_for_plot(ib1, keepmethods = NULL, incloverall = TRUE,
                                colorscheme = c("blue", "green", "yellow", "red"),
                                facetted = TRUE, incltruth = TRUE)

  test_that("Plot functions return ggplot objects with too many colors", {
    expect_is(plot_tpr(ibp1), "ggplot")
    expect_equal(length(plotcolors(ibp1)), 12)
    expect_equal(length(unique(plotcolors(ibp1))), 3)
  })
})

local({
  ib1 <- calculate_performance(cobradata_example, binary_truth = "status",
                               cont_truth = "logFC",
                               aspects = c("fdrtprcurve"),
                               thrs = c(0.05), splv = "none", onlyshared = FALSE,
                               thr_venn = 0.05)

  ibp1 <- prepare_data_for_plot(ib1, keepmethods = NULL, incloverall = TRUE,
                                colorscheme = c("blue", "green"), facetted = TRUE,
                                incltruth = TRUE)

  test_that("Plot functions return ggplot objects with too few colors", {
    expect_is(plot_tpr(ibp1), "ggplot")
    expect_equal(length(plotcolors(ibp1)), 18)
    expect_equal(length(unique(plotcolors(ibp1))), 4)
  })

  ibp1 <- prepare_data_for_plot(ib1, keepmethods = NULL, incloverall = TRUE,
                                colorscheme = c("blue", "green", "red"), facetted = TRUE,
                                incltruth = TRUE)

  test_that("Plot functions return ggplot objects with right nbr colors", {
    expect_is(plot_tpr(ibp1), "ggplot")
    expect_equal(length(plotcolors(ibp1)), 18)
    expect_equal(length(unique(plotcolors(ibp1))), 4)
  })
})

test_that("Plot functions return ggplot objects after stratification", {
  ## Facetted, stratified
  ib1 <- calculate_performance(cobradata_example, binary_truth = "status",
                               cont_truth = "logFC",
                               aspects = c("fdrtpr", "fdrtprcurve", "fdrnbr",
                                           "fdrnbrcurve", "tpr", "fpr",
                                           "roc", "fpc", "overlap", "corr",
                                           "scatter", "deviation"),
                               thrs = c(0.05), splv = "expr_cat", onlyshared = FALSE,
                               thr_venn = 0.05, maxsplit = 3)

  ibp1 <- prepare_data_for_plot(ib1, keepmethods = NULL, incloverall = TRUE,
                                colorscheme = "rainbow", facetted = TRUE,
                                incltruth = FALSE)

  ibp2 <- prepare_data_for_plot(ib1, keepmethods = NULL, incloverall = TRUE,
                                colorscheme = "rainbow", facetted = TRUE,
                                incltruth = TRUE)

  expect_is(plot_tpr(ibp1), "ggplot")
  expect_is(plot_corr(ibp1), "ggplot")
  expect_is(plot_deviation(ibp1), "ggplot")
  expect_is(plot_deviation(ibp1, transf = "absolute"), "ggplot")
  expect_is(plot_deviation(ibp1, transf = "squared"), "ggplot")
  expect_error(plot_deviation(ibp1, transf = "square"))
  expect_is(plot_fdrnbrcurve(ibp1), "ggplot")
  expect_is(plot_fdrtprcurve(ibp1), "ggplot")
  expect_is(plot_fpc(ibp1), "ggplot")
  expect_is(plot_fpr(ibp1), "ggplot")
  expect_is(plot_corr(ibp1), "ggplot")
  expect_is(plot_roc(ibp1), "ggplot")
  expect_is(plot_scatter(ibp1), "ggplot")
  expect_is(plot_overlap(ibp1), "list")
  expect_is(plot_overlap(ibp2), "list")
})
