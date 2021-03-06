% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{fun_scope}
\alias{fun_scope}
\title{Add scope}
\usage{
fun_scope(p, a, r0, gamma)
}
\arguments{
\item{p}{Plot as initialised by \code{\link{funplot}}.}

\item{a, r0, gamma}{Parameters for \code{r} in \code{\link{fun_add_polar}}.}
}
\description{
Add a scope to a polar equation.
}
\examples{
fn <- "r0 * cos(theta - gamma) + sqrt(a^2 - r0^2 * (sin(theta - gamma))^2)"

funplot() \%>\%
  fun_add_polar(fn) \%>\%
  fun_scope(1, 1, 1) \%>\%
  fun_add_polar(fn) \%>\%
  fun_scope(1, 0, 0)

}
