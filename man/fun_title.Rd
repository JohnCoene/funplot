% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{fun_title}
\alias{fun_title}
\title{Add title}
\usage{
fun_title(p, title)
}
\arguments{
\item{p}{Plot as initialised by \code{\link{funplot}}.}

\item{title}{Plot title.}
}
\description{
Add a title.
}
\examples{
funplot() \%>\%
  fun_add("tan(x)") \%>\%
  fun_title("Infinite tangent")

}
