## Test helper functions

library(iCOBRA)
context("Check helper functions")

test_that("is_plottable works", {
  expect_false(iCOBRA:::is_plottable(NULL))
  expect_false(iCOBRA:::is_plottable(data.frame()))
  expect_false(iCOBRA:::is_plottable(logical(0)))
})

test_that("get_coltype works", {
  expect_equal(iCOBRA:::get_coltype("Method1:P"), "pval")
  expect_equal(iCOBRA:::get_coltype("Method2:adjP"), "padj")
  expect_equal(iCOBRA:::get_coltype("Method3:score"), "score")
  expect_equal(iCOBRA:::get_coltype("Method3:S"), "sval")
  expect_is(iCOBRA:::get_coltype("Method2"), "NULL")
})

test_that("fix_res works", {
  cobraperf <- calculate_performance(cobradata_example_sval, binary_truth = "status",
                                    cont_truth = "logFC")
  cobraplot <- prepare_data_for_plot(cobraperf)

  res <- fsrnbr(cobraplot)
  res$dist_ <- runif(nrow(res))
  dt <- iCOBRA:::fix_res(res, methodcol = "fullmethod", aspcts = "FSR", tabtype = "fsrnbr")
  expect_is(dt, "datatables")
  
  res <- tpr(cobraplot)
  res$dist_ <- runif(nrow(res))
  dt <- iCOBRA:::fix_res(res, methodcol = "fullmethod", aspcts = "TPR")
  expect_is(dt, "datatables")

  res <- corr(cobraplot)
  res$dist_ <- runif(nrow(res))
  dt <- iCOBRA:::fix_res(res, methodcol = "fullmethod", aspcts = "PEARSON",
                       tabtype = "corr")
  expect_is(dt, "datatables")

  res <- scatter(cobraplot)
  res$dist_ <- runif(nrow(res))
  dt <- iCOBRA:::fix_res(res, methodcol = "fullmethod", aspcts = c("observation", "truth"),
                       tabtype = "scatter")
  expect_is(dt, "datatables")

  res <- deviation(cobraplot)
  res$dist_ <- runif(nrow(res))
  dt <- iCOBRA:::fix_res(res, methodcol = "fullmethod", aspcts = "DEVIATION",
                       tabtype = "deviation")
  expect_is(dt, "datatables")

  res <- roc(cobraplot)
  res$dist_ <- runif(nrow(res))
  dt <- iCOBRA:::fix_res(res, methodcol = "fullmethod", aspcts = c("FPR", "TPR"),
                       tabtype = "small")
  expect_is(dt, "datatables")
})

test_that("res_check works", {
  expect_false(iCOBRA:::res_check(pval(cobradata_example_sval)[, 1, drop = FALSE]))
  expect_true(iCOBRA:::res_check(pval(cobradata_example_sval)))
})

test_that("fix_duplicates works", {
  df <- data.frame(gene = c("A", "A", "B", "C", "D"), "m1:P" = c(0.2, 0.1, 0.5, 0.3, 0.01), check.names = FALSE, stringsAsFactors = FALSE)
  expect_equal(nrow(iCOBRA:::fix_duplicates(df, "gene", "m1:P")), 4)
  expect_equal(iCOBRA:::fix_duplicates(df, "gene", "m1:P")[, "m1:P"], c(0.1, 0.5, 0.3, 0.01))
})
