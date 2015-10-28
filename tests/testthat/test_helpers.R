## Test helper functions

library(COBRA)
context("Check helper functions")

test_that("get_coltype works", {
  expect_equal(COBRA:::get_coltype("edgeR:P"), "pval")
  expect_equal(COBRA:::get_coltype("voom:adjP"), "padj")
  expect_equal(COBRA:::get_coltype("DESeq:2:score"), "score")
  expect_is(COBRA:::get_coltype("voom"), "NULL")
})

test_that("fix_res works", {
  cobraperf <- calculate_performance(cobradata_example, binary_truth = "status",
                                    cont_truth = "logFC")
  cobraplot <- prepare_data_for_plot(cobraperf)

  res <- tpr(cobraplot)
  res$dist_ <- runif(nrow(res))
  dt <- COBRA:::fix_res(res, methodcol = "fullmethod", aspcts = "TPR")
  expect_is(dt, "datatables")

  res <- corr(cobraplot)
  res$dist_ <- runif(nrow(res))
  dt <- COBRA:::fix_res(res, methodcol = "fullmethod", aspcts = "PEARSON",
                       tabtype = "corr")
  expect_is(dt, "datatables")

  res <- scatter(cobraplot)
  res$dist_ <- runif(nrow(res))
  dt <- COBRA:::fix_res(res, methodcol = "fullmethod", aspcts = c("observation", "truth"),
                       tabtype = "scatter")
  expect_is(dt, "datatables")

  res <- deviation(cobraplot)
  res$dist_ <- runif(nrow(res))
  dt <- COBRA:::fix_res(res, methodcol = "fullmethod", aspcts = "DEVIATION",
                       tabtype = "deviation")
  expect_is(dt, "datatables")

  res <- roc(cobraplot)
  res$dist_ <- runif(nrow(res))
  dt <- COBRA:::fix_res(res, methodcol = "fullmethod", aspcts = c("FPR", "TPR"),
                       tabtype = "small")
  expect_is(dt, "datatables")
})

test_that("res_check works", {
  expect_false(COBRA:::res_check(pval(cobradata_example)[, 1, drop = FALSE]))
  expect_true(COBRA:::res_check(pval(cobradata_example)))
})

test_that("fix_duplicates works", {
  df <- data.frame(gene = c("A", "A", "B", "C", "D"), "m1:P" = c(0.2, 0.1, 0.5, 0.3, 0.01), check.names = FALSE, stringsAsFactors = FALSE)
  expect_equal(nrow(COBRA:::fix_duplicates(df, "gene", "m1:P")), 4)
  expect_equal(COBRA:::fix_duplicates(df, "gene", "m1:P")[, "m1:P"], c(0.1, 0.5, 0.3, 0.01))
})
