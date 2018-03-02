#' Add secants
#'
#' Add secants to the previously added function.
#'
#' @inheritParams fun_add
#' @param x0,x1 See details.
#' @param mouse Update secant on mouse move.
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
#' @param fun Derivative, see details.
#' @param mouse Update tip on mouse move.
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

#' Add scope
#'
#' Add a scope to a polar equation.
#'
#' @inheritParams fun_add
#' @param a,r0,gamma Parameters for \code{r} in \code{\link{fun_add_polar}}.
#'
#' @examples
#' fn <- "r0 * cos(theta - gamma) + sqrt(a^2 - r0^2 * (sin(theta - gamma))^2)"
#'
#' funplot() %>%
#'   fun_add_polar(fn) %>%
#'   fun_scope(1, 1, 1) %>%
#'   fun_add_polar(fn) %>%
#'   fun_scope(1, 0, 0)
#'
#' @export
fun_scope <- function(p, a, r0, gamma){

  if(missing(p)) stop("missing plot.")

  scope <- list()
  scope$a <- a
  scope$r0 <- r0
  scope$gamma <- gamma

  p$x$data[[length(p$x$data)]]$scope <- scope

  p
}

#' Add points
#'
#' Add points to plot.
#'
#' @inheritParams fun_add
#' @param data \code{data.frame} or \code{matrix} where the first
#' column is \eqn{x} and the sencond \eqn{y}.
#' @param ... Any other parameter.
#'
#' @examples
#' points <- data.frame(x = rnorm(10), y = rnorm(10))
#' lines <- data.frame(x = seq(-5, 4), y = rnorm(10))
#'
#' funplot() %>%
#'   fun_points(points) %>%
#'   fun_lines(lines)
#'
#' @rdname points
#' @export
fun_points <- function(p, data, ...){

  if(missing(p)) stop("missing plot.")
  if(missing(data)) stop("missing data.")

  data <- apply(unname(data), 1, as.list)

  points <- list(...)
  points$points <- data
  points$fnType = "points"
  points$graphType = "scatter"

  p$x$data <- append(p$x$data, list(points))
  p
}

#' @rdname points
#' @export
fun_lines <- function(p, data, ...){

  if(missing(p)) stop("missing plot.")
  if(missing(data)) stop("missing data.")

  data <- apply(unname(data), 1, as.list)

  points <- list(...)
  points$points <- data
  points$fnType = "points"
  points$graphType = "polyline"

  p$x$data <- append(p$x$data, list(points))
  p
}

#' Add a vector
#'
#' Add a vector to plot.
#'
#' @inheritParams fun_add
#' @param vector A vector of length 2.
#' @param offset A vector of displacement from the origin.
#' @param ... Any other parameter.
#'
#' @examples
#' funplot() %>%
#'   fun_vector(c(2, 1), c(1, 2))
#'
#' @export
fun_vector <- function(p, vector, offset = NULL, ...){

  if(missing(p)) stop("missing plot.")
  if(missing(vector)) stop("missing vector.")

  vect <- list(...)
  vect$vector <- vector
  vect$offset <- if(!is.null(offset)) offset
  vect$fnType = "vector"
  vect$graphType = "polyline"

  p$x$data <- append(p$x$data, list(vect))
  p
}
