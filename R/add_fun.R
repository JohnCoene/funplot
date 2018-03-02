#' Add function
#'
#' Add a function to plot.
#'
#' @param p Plot as initialised by \code{\link{funplot}}.
#' @param fun Function to plot.
#' @param x,y Parametric equation.
#' @param r Polar equation.
#' @param samples Determine the number of equally spaced points
#' in which the function will be evaluated in the current domain,
#' increasing it will more accurately represent the function using rectangles
#' at the cost of processing speed.
#' @param closed Set to \code{TRUE} to render a closed path, y0 will always be 0 and y1 will be \eqn{fn(x)}.
#' @param color Color.
#' @param type Rhree representations of functions, default to \code{interval}, see details.
#' @param tip Set to \code{TRUE} to hide the tooltip.
#' @param scope Scope of \code{r}, see \code{\link{fun_scope}}.
#' @param range A \code{list} of length 2, the function will be evaluated only within this range.
#' @param ... Any other parameter.
#'
#' @details
#' Valid \code{type} values:
#' \itemize{
#'   \item{\code{polyline} }{where \eqn{f(x)} is evaluated with some \eqn{x} values,
#'   after the evaluation the points are joined with line segments using \code{<path>}s}
#'   \item{\code{scatter} }{where \eqn{f(x)} is evaluated with some \eqn{x} values,
#'   after the evaluation the points are represented by \code{<circle>}s}
#'   \item{\code{interval} }{where \eqn{f(x)} is evaluated with intervals instead of a single point,
#'   after the evaluation 2d rects are painted on the screen (done using the \code{<path>} svg element)}
#' }
#'
#' \code{fun}:
#'
#' Plotting roots can be a challenging problem, most plotters will actually analyze expression of the type \eqn{x^{a/b}},
#' particularly they will analyze the denominator of the exponent (to plot in the negative x-axis),
#' interval-arithmetic and math.js come bundled with a useful \code{nthRoot} function to solve these issues.
#'
#' @examples
#' # basic equation
#' funplot() %>%
#'   fun_add(fun = "sin(x)")
#'
#' # parametric equation
#' funplot() %>%
#'   fun_add_param(
#'     x = "sin(t) * (exp(cos(t)) - 2 cos(4t) - sin(t/12)^5)",
#'     y = "cos(t) * (exp(cos(t)) - 2 cos(4t) - sin(t/12)^5)"
#'    )
#'
#' # polar equation
#' funplot() %>%
#'   fun_add_polar(r = "2 * sin(4 theta)")
#'
#' # implicit function
#' funplot() %>%
#'   fun_add_imp("cos(PI * x) - cos(PI * y)")
#'
#' # multiple functions
#' funplot() %>%
#'   fun_add("sqrt(1 - x * x)") %>%
#'   fun_add("-sqrt(1 - x * x)")
#'
#' # samples
#' funplot() %>%
#'   fun_add(fun = "sin(x)", samples = 1000)
#'
#' # closed = TRUE
#' funplot() %>%
#'   fun_add("1/x * cos(1/x)", closed = TRUE) %>%
#'   fun_x("log", domain = list(0.01, 1)) %>%
#'   fun_y(domain = list(-100, 100))
#'
#' # color and type
#' funplot() %>%
#'   fun_add("x", color = "black") %>%
#'   fun_add("-x") %>%
#'   fun_add("-sqrt(x)", type = "scatter", samples = 100) %>%
#'   fun_add("sqrt(x)", tip = TRUE)
#'
#' # nthRoot
#' funplot() %>%
#'   fun_add("nthRoot(x, 3)^2")
#'
#' # derivative
#' funplot() %>%
#'   fun_add("x^2") %>%
#'   fun_deriv("2 * x", mouse = TRUE)
#'
#' # secants
#' funplot() %>%
#'   fun_add("x^2") %>%
#'   fun_secants(x0 = 5, mouse = TRUE)
#'
#' @rdname fun
#' @seealso \code{\link{fun_secants}}, \code{\link{fun_deriv}}, \code{\link{fun_scope}}
#' @export
fun_add <- function(p, fun, samples = NULL, closed = FALSE, color = NULL, type = NULL,
                    range = NULL, tip = FALSE, ...){
  if(missing(p)) stop("missing plot.")

  foo <- list(...)
  foo$fn <- if(!missing(fun)) fun
  foo$nSamples <- if(!is.null(samples)) samples
  foo$range <- if(!is.null(range)) range
  foo$closed <- closed
  foo$color <- if(!is.null(color)) color
  foo$graphType <- if(!is.null(type)) type
  foo$skipTip <- tip

  p$x$data <- append(p$x$data, list(foo))
  p
}

#' @rdname fun
#' @export
fun_add_param <- function(p, x, y, samples = NULL, closed = FALSE, color = NULL, type = "polyline",
                          range = NULL, tip = FALSE, ...){
  if(missing(p)) stop("missing plot.")
  if(missing(x) || missing(y)) stop("missing x or y.")


  foo <- list(...)
  foo$x <- x
  foo$y <- y
  foo$fnType <- "parametric"
  foo$nSamples <- if(!is.null(samples)) samples
  foo$range <- if(!is.null(range)) range
  foo$closed <- closed
  foo$color <- if(!is.null(color)) color
  foo$graphType <- if(!is.null(type)) type
  foo$skipTip <- tip

  p$x$data <- append(p$x$data, list(foo))
  p
}

#' @rdname fun
#' @export
fun_add_polar <- function(p, r, scope = NULL, samples = NULL, closed = FALSE, color = NULL, type = "polyline",
                          range = NULL, tip = FALSE, ...){
  if(missing(p)) stop("missing plot.")

  foo <- list(...)
  foo$r <- r
  foo$scope <- if(!is.null(scope)) scope
  foo$range <- if(!is.null(range)) range
  foo$fnType <- "polar"
  foo$nSamples <- if(!is.null(samples)) samples
  foo$closed <- closed
  foo$color <- if(!is.null(color)) color
  foo$graphType <- if(!is.null(type)) type
  foo$skipTip <- tip

  p$x$data <- append(p$x$data, list(foo))
  p
}

#' @rdname fun
#' @export
fun_add_imp <- function(p, fun, samples = NULL, closed = FALSE, color = NULL, type = NULL,
                        range = NULL, tip = FALSE, ...){
  if(missing(p)) stop("missing plot.")

  foo <- list(...)
  foo$fn <- if(!missing(fun)) fun
  foo$range <- if(!is.null(range)) range
  foo$fnType <- "implicit"
  foo$nSamples <- if(!is.null(samples)) samples
  foo$closed <- closed
  foo$color <- if(!is.null(color)) color
  foo$graphType <- if(!is.null(type)) type
  foo$skipTip <- tip

  p$x$data <- append(p$x$data, list(foo))
  p
}

#' Add math.js
#'
#' Add a Add function to be evaluated by math.js.
#'
#' @inheritParams fun_add
#' @param fun Function to plot.
#' @param ... Any other parameter, see \code{\link{fun_add}}.
#'
#' @details \code{funplot} uses interval-arithmetic math by default, unfortunately some functions are not
#' implemented yet because of the underlying complexity, for this reason you can always evaluate a function with
#' \href{mathjs}{http://mathjs.org/}.
#'
#' @examples
#' funplot() %>%
#'  fun_math("gamma(x)")
#'
#' funplot() %>%
#'  fun_math("atan2(x, x ^ 2) / pi", samples = 4000) %>%
#'  fun_y(domain = list(-1, 1))
#'
#' @export
fun_math <- function(p, fun, ...){
  if(missing(p)) stop("missing plot.")
  if(missing(fun)) stop("missing fun.")

  foo <- list(...)
  foo$fn <- fun
  foo$sample <- "builtIn"
  foo$graphType <- "polyline"

  p$x$data <- append(p$x$data, list(foo))
  p
}
