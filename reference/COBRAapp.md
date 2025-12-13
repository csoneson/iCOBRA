# Interactive shiny app to visualize results

Interactive shiny app for visualization of results. The app can be
initialized with a `COBRAData` object. If no object is provided, truth
and results are loaded into the app from text files (see the
Instructions tab of the app for formatting instructions). Properly
formatted text files can also be obtained using the function
[`COBRAData_to_text`](https://csoneson.github.io/iCOBRA/reference/COBRAData.md).

## Usage

``` r
COBRAapp(cobradata = NULL, autorun = FALSE, addStopButton = TRUE)
```

## Arguments

- cobradata:

  An (optional) `COBRAData` object. If not given, the user can load
  results from text files.

- autorun:

  A logical indicating whether the app calculations should start
  automatically on launch, or wait for the user to press the 'Start
  calculation!' button.

- addStopButton:

  Logical scalar. If `TRUE` (default), will add a button to stop the app
  (by calling
  [`shiny::stopApp`](https://rdrr.io/pkg/shiny/man/stopApp.html)).

## Value

Returns (and runs) an object representing the shiny app.

## Author

Charlotte Soneson

## Examples

``` r
data(cobradata_example)
app <- COBRAapp(cobradata_example)
if (interactive()) {
  shiny::runApp(app)
}
```
