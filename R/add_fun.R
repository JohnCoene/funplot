#' Add function
#'
#' Add a function to plot.
#'
#' @param p Plot as initialised by \code{\link{funplot}}.
#' @param fun Function to plot.
#' @param samples Determine the number of equally spaced points
#' in which the function will be evaluated in the current domain,
#' increasing it will more accurately represent the function using rectangles
#' at the cost of processing speed.
#' @param closed Set to \code{TRUE} to render a closed path, y0 will always be 0 and y1 will be \eqn{fn(x)}.
#' @param color color.
#' @param type three representations of functions, default to \code{interval}, see details.
#' @param tip set to \code{TRUE} to hide the tooltip.
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
#' \code{fun}
#'
#' Plotting roots can be a challenging problem, most plotters will actually analyze expression of the type \eqn{x^{a/b}},
#' particularly they will analyze the denominator of the exponent (to plot in the negative x-axis),
#' interval-arithmetic and math.js come bundled with a useful \code{nthRoot} function to solve these issues.
#'
#' If a data object has a secants \code{list}, then each object will be used to compute secant lines between two
#' points belonging to the function, additionally if \code{updateOnMouseMove} is a property set to true in the object then
#' \eqn{(x_{0},f(x_{1}))} will be used as an anchored point and \eqn{(x_{0},f(x_{1}))} will be computed dynamically
#' based on the mouse abscissa.
#'
#' Values for \code{secants} list:
#' \itemize{
#'   \item{\code{x0} }{the abscissa of the first point}
#'   \item{\code{x1} }{(optional if \code{updateOnMouseMove} is set) the abscissa of the second point}
#'   \item{\code{updateOnMouseMove} }{ (optional) if set to \code{TRUE} \code{x1} will be computed dynamically
#'   based on the current position of the mouse}.
#' }
#'
#' @examples
#' funplot() %>%
#'   fun_add(fun = "sin(x)")
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
#' # secants
#' funplot() %>%
#'   fun_add(
#'     "x^2",
#'     secants = list(
#'       list(x0 = 2, x1 = 3),
#'       list(x0 = -2, updateOnMouseMove = TRUE),
#'       list(x0 = 1, x1 = 2)
#'     )
#'   ) %>%
#'   fun_y(domain = list(-200, 400)) %>%
#'   fun_x(domain = list(-300, 300))
#'
#' # derivative
#' funplot() %>%
#'   fun_add("x^2", derivative = list(fn = "2 * x", updateOnMouseMove = TRUE))
#'
#' @seealso \code{\link{fun_secants}}, \code{\link{fun_deriv}}
#' @export
fun_add <- function(p, fun, samples = NULL, closed = FALSE, color = NULL, type = NULL,
                    tip = FALSE, secants = NULL, derivative = NULL){
  if(missing(p)) stop("missing plot.")
  if(missing(fun)) stop("missing fun.")

  foo <- list()
  foo$fn <- fun
  foo$nSamples <- if(!is.null(samples)) samples
  foo$closed <- closed
  foo$color <- if(!is.null(color)) color
  foo$graphType <- if(!is.null(type)) type
  foo$skipTip <- tip
  foo$secants <- if(!is.null(secants)) secants
  foo$derivative <- if(!is.null(derivative)) derivative

  p$x$data <- append(p$x$data, list(foo))
  p
}

#' Add secants
#'
#' Add secants to the previously added function.
#'
#' @inheritParams fun_add
#' @param x0,x1 see details.
#' @param mouse update secant on mouse move.
#'
#' @details
#' If a data object has a secants, then each object will be used to compute secant lines between two
#' points belonging to the function, additionally if \code{updateOnMouseMove} is a property set to true in the object then
#' \eqn{(x_{0},f(x_{1}))} will be used as an anchored point and \eqn{(x_{0},f(x_{1}))} will be computed dynamically
#' based on the mouse abscissa.
#'
#' Values for \code{secants} list:
#' \itemize{
#'   \item{\code{x0} }{the abscissa of the first point}
#'   \item{\code{x1} }{(optional if \code{updateOnMouseMove} is set) the abscissa of the second point}
#'   \item{\code{mouse} }{ (optional) if set to \code{TRUE} \code{x1} will be computed dynamically
#'   based on the current position of the mouse}.
#' }
#'
#' @examples
#' funplot() %>%
#'   fun_add("x^2") %>%
#'   fun_secants(x0 = 5, mouse = TRUE) %>%
#'   fun_secants(x0 = 2, x1 = 3)
#'
#' @export
fun_secants <- function(p, x0, x1 = NULL, mouse = FALSE){
  if(missing(p)) stop("missing plot.")
  if(missing(x0)) stop("missing x0.")

  previous <- if(!is.null(p$x$data[[length(p$x$data)]]$secants)) p$x$data[[length(p$x$data)]]$secants else list()

  secants <- list()
  secants$x0 <- x0
  secants$x1 <- if(!is.null(x1)) x1
  secants$updateOnMouseMove <- mouse

  secants <- append(previous, list(secants))

  p$x$data[[length(p$x$data)]]$secants <- secants

  p
}

#' Add derivative
#'
#' Add a derivative to the previously added function.
#'
#' @inheritParams fun_add
#' @param fun see details.
#' @param mouse update tip on mouse move.
#'
#' @details
#' If \code{mouse} is set to true then tangent line is computed whenever the mouse is moved inside the canvas
#' (let \code{x_{0}} be the mouse's abscissa then the tangent line to the poin
#' \eqn{(x_{0},f(x_{1}))} is computed whenever the position of the mouse changes).
#'
#' @examples
#' funplot() %>%
#'   fun_add("x^2") %>%
#'   fun_deriv("2 * x", mouse = TRUE)
#'
#' funplot() %>%
#'   fun_add("x * x") %>%
#'   fun_deriv("2 * x", mouse = TRUE) %>%
#'   fun_add("x * x * x") %>%
#'   fun_deriv("3 * x * x", mouse = TRUE)
#'
#' @export
fun_deriv <- function(p, fun, mouse = FALSE){
  if(missing(p)) stop("missing plot.")
  if(missing(fun)) stop("missing fun.")

  derivative <- list(
    fn = fun,
    updateOnMouseMove = mouse
  )

  p$x$data[[length(p$x$data)]]$derivative <- derivative

  p
}
