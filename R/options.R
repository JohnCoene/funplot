#' Add title
#'
#' Add a title.
#'
#' @inheritParams fun_add
#' @param title Plot title.
#'
#' @examples
#' funplot() %>%
#'   fun_add("tan(x)") %>%
#'   fun_title("Infinite tangent")
#'
#' @export
fun_title <- function(p, title){
  if(missing(p)) stop("missing plot.")
  if(missing(title)) stop("missing title.")

  p$x$title <- title
  p
}

#' Disable zoom
#'
#' Disable zoom.
#'
#' @inheritParams fun_add
#' @param zoom Set to \code{TRUE} to disable (default).
#'
#' @examples
#' funplot() %>%
#'   fun_add("tan(x)") %>%
#'   fun_zoom()
#'
#' @export
fun_zoom <- function(p, zoom = TRUE){
  if(missing(p)) stop("missing plot.")

  p$x$disableZoom <- zoom
  p
}

#' Change size
#'
#' Change plot size.
#'
#' @inheritParams fun_add
#' @param height,width Dimentions of plot.
#'
#' @examples
#' funplot() %>%
#'   fun_add("tan(x)") %>%
#'   fun_size(200, 400) %>%
#'   fun_title("Plot size")
#'
#' @export
fun_size <- function(p, height = NULL, width = NULL){
  if(missing(p)) stop("missing plot.")

  p$x$width <- if(!is.null(width)) width
  p$x$height <- if(!is.null(height)) height

  p
}

#' Change size
#'
#' Change plot size.
#'
#' @inheritParams fun_add
#' @param type Scale for this axis, possible values \code{linear} or \code{log}.
#' @param label Axis label.
#' @param domain Possible axis values.
#'
#' @details
#' \code{fun_y} \code{domain} parameter defaults to \code{list(-7, 7)}. By default the domain of
#' \code{fun_x} is computed with: \deqn{yDiff = height * (xDomain[1] - xDomain[0]) / width}
#' Note: The origin is at the center of the graph by default so \eqn{yDiff} is split in half
#' and distributed evenly to the \eqn{±y} axis.
#'
#' @examples
#' funplot() %>%
#'   fun_add("x ^ 2") %>%
#'   fun_x(label = "x axis", domain = list(-6, 6)) %>%
#'   fun_y(type = "log", "log scale") %>%
#'   fun_title("Custom axis")
#'
#' @name axis
#' @rdname axis
#' @export
fun_x <- function(p, type = NULL, label = NULL, domain = NULL){
  if(missing(p)) stop("missing plot.")

  p$x$xAxis$type <- if(!is.null(type)) type
  p$x$xAxis$label <- if(!is.null(label)) label
  p$x$xAxis$domain <- if(!is.null(domain)) domain

  p
}

#' @rdname axis
#' @export
fun_y <- function(p, type = NULL, label = NULL, domain = NULL){
  if(missing(p)) stop("missing plot.")

  p$x$yAxis$type <- if(!is.null(type)) type
  p$x$yAxis$label <- if(!is.null(label)) label
  p$x$yAxis$domain <- if(!is.null(domain)) domain

  p
}

#' Add grid
#'
#' Add plot grid.
#'
#' @inheritParams fun_add
#' @param grid Set to \code{TRUE} add grid (default).
#'
#' @examples
#' funplot() %>%
#'   fun_add("(x ^ 2) - 3") %>%
#'   fun_grid() %>%
#'   fun_title("Show grid")
#'
#' @export
fun_grid <- function(p, grid = TRUE){
  if(missing(p)) stop("missing plot.")

  p$x$grid <- grid

  p
}

#' Customise tooltip
#'
#' Customise the tooltip.
#'
#' @inheritParams fun_add
#' @param x,y Set to \code{TRUE} to show x and y lines.
#' @param renderer Custom rendering function for the text shown in the tip.
#'
#' @details
#' \code{x = TRUE} shows a dashed line parallel to \eqn{y=0} on the tip position,
#' \code{y = TRUE} shows a dashed line parallel to \eqn{x=0} on the tip position
#'
#' @examples
#' funplot() %>%
#'   fun_add("(x ^ 2) - 3") %>%
#'   fun_tip(TRUE, TRUE)
#'
#' @export
fun_tip <- function(p, x = FALSE, y = FALSE, renderer = NULL){
  if(missing(p)) stop("missing plot.")

  tip <- list()
  tip$xLine <- x
  tip$yLine <- y
  tip$renderer <- renderer

  p$x$tip <- tip

  p
}
