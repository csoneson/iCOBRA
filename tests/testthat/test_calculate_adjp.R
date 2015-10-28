## Test that calculate_adjp is doing the right thing

library(COBRA)
context("Check that calculate_adjp is working properly")

test_that("calculated adjusted p-values are correct", {
  pval <- c(0.0058, 0.771, 0.024, 0.741, 0.247)
  ib <- COBRAData(pval = data.frame(m1 = pval, row.names = paste0("F", 1:5)),
                 truth = data.frame(status = c(0, 1, 0, 1, 0),
                                    row.names = paste0("F", 1:5)))
  ib2 <- calculate_adjp(ib)
  expect_equal(padj(ib2)[, "m1"], p.adjust(pval, method = "BH"))
  ib2 <- calculate_adjp(ib, method = "holm")
  expect_equal(padj(ib2)[, "m1"], p.adjust(pval, method = "holm"))
})

test_that("adjusted p-values are added correctly", {
  pval <- c(0.0058, 0.771, 0.024, 0.741, 0.247)
  padj <- p.adjust(pval, method = "BH")
  ib <- COBRAData(pval = data.frame(m1 = pval, m2 = pval, row.names = paste0("F", 1:5)),
                 padj = data.frame(m1 = padj, row.names = paste0("F", 1:5)),
                 truth = data.frame(status = c(0, 1, 0, 1, 0),
                                    row.names = paste0("F", 1:5)))
  ib2 <- calculate_adjp(ib, method = "BH")
  expect_equal(padj(ib2)$m1, padj(ib2)$m2)
})
