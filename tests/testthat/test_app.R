## Test shiny app

library(iCOBRA)
context("Check that shiny app is generated")

test_that("Shiny app is generated", {
  expect_warning({
    expect_is(COBRAapp(), "shiny.appobj")
  }, "expect a collection of")
  expect_warning({
    expect_is(COBRAapp(cobradata_example), "shiny.appobj")
  }, "expect a collection of")

  expect_warning({
    expect_is(COBRAapp(autorun = TRUE), "shiny.appobj")
  }, "expect a collection of")
  expect_warning({
    expect_is(COBRAapp(cobradata_example, autorun = TRUE), "shiny.appobj")
  }, "expect a collection of")
})
