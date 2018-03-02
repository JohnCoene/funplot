#' Initialise
#'
#' Initialise a funplot.
#'
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param elementId id of container (optional).
#'
#' @examples
#' funplot() %>%
#'   fun_add("sin(x)")
#'
#' @import htmlwidgets
#'
#' @export
funplot <- function(width = NULL, height = NULL, elementId = NULL) {

  # forward options using x
  x = list(
    data = NULL
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'function-plot',
    x,
    width = width,
    height = height,
    package = 'funplot',
    elementId = elementId
  )
}

#' Shiny bindings for function-plot
#'
#' Output and render functions for using function-plot within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a function-plot
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name function-plot-shiny
#'
#' @export
funplotOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'function-plot', width, height, package = 'funplot')
}

#' @rdname function-plot-shiny
#' @export
renderFunplot <- function(expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, funplotOutput, env, quoted = TRUE)
}
