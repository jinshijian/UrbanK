#' Re-scale and un-scale data based on its range
#'
#' @param x Vector to scale or un-scale
#' @param y Vector whose limits will be used for re-scaling
#' @return Object of same type as `x`
#' @author Alexey Shiklomanov
#' @export
scale_range <- function(x) {
  hi <- max(x)
  lo <- min(x)
  (x - lo) / (hi - lo)
}

#' @rdname scale_range
unscale_range <- function(x, y = x) {
  stopifnot(is.numeric(y))
  hi <- max(y)
  lo <- min(y)
  x * (hi - lo) + lo
}
