## Test shiny app

library(IBRA)
context("Check that shiny app is generated")

test_that("Shiny app is generated", {
  expect_is(IBRAapp(), "shiny.appobj")
  expect_is(IBRAapp(ibradata_example), "shiny.appobj")

  expect_is(IBRAapp(autorun = TRUE), "shiny.appobj")
  expect_is(IBRAapp(ibradata_example, autorun = TRUE), "shiny.appobj")
})
