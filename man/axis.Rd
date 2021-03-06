% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{axis}
\alias{axis}
\alias{fun_x}
\alias{fun_y}
\title{Change size}
\usage{
fun_x(p, type = NULL, label = NULL, domain = NULL)

fun_y(p, type = NULL, label = NULL, domain = NULL)
}
\arguments{
\item{p}{Plot as initialised by \code{\link{funplot}}.}

\item{type}{Scale for this axis, possible values \code{linear} or \code{log}.}

\item{label}{Axis label.}

\item{domain}{Possible axis values.}
}
\description{
Change plot size.
}
\details{
\code{fun_y} \code{domain} parameter defaults to \code{list(-7, 7)}. By default the domain of
\code{fun_x} is computed with: \deqn{yDiff = height * (xDomain[1] - xDomain[0]) / width}
Note: The origin is at the center of the graph by default so \eqn{yDiff} is split in half
and distributed evenly to the \eqn{±y} axis.
}
\examples{
funplot() \%>\%
  fun_add("x ^ 2") \%>\%
  fun_x(label = "x axis", domain = list(-6, 6)) \%>\%
  fun_y(type = "log", "log scale") \%>\%
  fun_title("Custom axis")

}
