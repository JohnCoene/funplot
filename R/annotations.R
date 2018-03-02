#' Add annotation
#'
#' Add annotation.
#'
#' @inheritParams fun_add
#' @param x,y coordinate.
#' @param text annotation.
#'
#' @examples
#' funplot() %>%
#'   fun_add("x^2") %>%
#'   fun_annot(x = 1, text = "x = 1") %>%
#'   fun_annot(y = 2, text = "y = 2")
#'
#' @export
fun_annot <- function(p, x = NULL, y = NULL, text = NULL){

  if(missing(p)) stop("missing plot.")
  if(is.null(x) && is.null(y)) stop("can only specify one of x or y, not both.")

  annot <- list()
  annot$x <- if(!is.null(x)) x
  annot$y <- if(!is.null(y)) y
  annot$text <- if(!is.null(text)) text

  p$x$annotations <- append(p$x$annotations, list(annot))

  p
}
