% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny.R
\name{COBRAapp}
\alias{COBRAapp}
\title{Interactive shiny app to visualize results}
\usage{
COBRAapp(cobradata = NULL, autorun = FALSE)
}
\arguments{
\item{cobradata}{An (optional) \code{COBRAData} object. If not given, the
user can load results from text files.}

\item{autorun}{A logical indicating whether the app calculations should start
automatically on launch, or wait for the user to press the 'Start
calculation!' button.}
}
\value{
Returns (and runs) an object representing the shiny app.
}
\description{
Interactive shiny app for visualization of results. The app can be
initialized with a \code{COBRAData} object. If no object is provided, truth
and results are loaded into the app from text files (see the Instructions tab
of the app for formatting instructions). Properly formatted text files can
also be obtained using the function \code{\link{COBRAData_to_text}}.
}
\examples{
data(cobradata_example)
app <- COBRAapp(cobradata_example)
if (interactive()) {
  shiny::runApp(app)
}
}
\author{
Charlotte Soneson
}
