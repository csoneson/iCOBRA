% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/COBRAPerformance.R,
%   R/COBRAPlot.R
\docType{methods}
\name{deviation}
\alias{deviation}
\alias{deviation<-}
\alias{deviation,COBRAPerformance-method}
\alias{deviation<-,COBRAPerformance,data.frame-method}
\alias{deviation,COBRAPlot-method}
\alias{deviation<-,COBRAPlot,data.frame-method}
\title{Accessor and replacement functions for \code{deviation} slot}
\usage{
deviation(x, ...)

deviation(x, ...) <- value

\S4method{deviation}{COBRAPerformance}(x)

\S4method{deviation}{COBRAPerformance,data.frame}(x) <- value

\S4method{deviation}{COBRAPlot,data.frame}(x) <- value
}
\arguments{
\item{x}{A \code{COBRAPerformance} or \code{COBRAPlot} object.}

\item{...}{Additional arguments.}

\item{value}{A data frame giving information necessary to plots of deviations
between observed and true scores for each method and each stratification
level.}
}
\value{
The accessor function returns a data frame giving information
  necessary to plots of deviations between observed and true scores for each
  method and each stratification level.
}
\description{
Accessor and replacement functions for the \code{deviation} slot in a
\code{COBRAPerformance} or \code{COBRAPlot} object.
}
\examples{
data(cobradata_example)
cobraperf <- calculate_performance(cobradata_example, cont_truth = "logFC",
                                   aspects = "deviation")
head(deviation(cobraperf))
}
\author{
Charlotte Soneson
}
