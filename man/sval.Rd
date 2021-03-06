% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/COBRAData.R
\docType{methods}
\name{sval}
\alias{sval}
\alias{sval<-}
\alias{sval,COBRAData-method}
\alias{sval<-,COBRAData,data.frame-method}
\title{Accessor and replacement functions for \code{sval} slot}
\usage{
sval(x, ...)

sval(x, ...) <- value

\S4method{sval}{COBRAData}(x)

\S4method{sval}{COBRAData,data.frame}(x) <- value
}
\arguments{
\item{x}{A \code{COBRAData} object.}

\item{...}{Additional arguments.}

\item{value}{A data frame containing s-values for each feature and each 
method. If the object does not have an s-value slot (older versions of the
class did not have this slot), an empty data frame is returned for
simplicity.}
}
\value{
The accessor function returns a data frame containing s-values for 
  each feature and each method.
}
\description{
Accessor and replacement functions for the \code{sval} slot in a
\code{COBRAData} object.
}
\examples{
data(cobradata_example)
head(sval(cobradata_example))
}
\author{
Charlotte Soneson
}
