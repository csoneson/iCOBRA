## Test shiny app

library(COBRA)
context("Check that shiny app is generated")

test_that("Shiny app is generated", {
  expect_is(COBRAapp(), "shiny.appobj")
  expect_is(COBRAapp(cobradata_example), "shiny.appobj")

  expect_is(COBRAapp(autorun = TRUE), "shiny.appobj")
  expect_is(COBRAapp(cobradata_example, autorun = TRUE), "shiny.appobj")
})
