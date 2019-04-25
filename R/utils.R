#' Allow a function to be called with a progress bar
#'
#' @param .f Function to be called. Pre-processed with
#'   [rlang::as_function()], so it can be an anonymous "lambda"
#'   function.
#' @param ... Arguments to function `.f`
#' @param .pb Progress bar object. Both [progress::progress_bar()] and
#'   [utils::txtProgressBar()] are supported.
#' @return Function `.f` modified to tick the progress bar
#' @author Alexey Shiklomanov
#' @examples
#' # The progress package has nice
#' f <- function(x) Sys.sleep(1)
#' pb <- progress::progress_bar$new(total = 5)
#' invisible(lapply(1:5, with_pb(f, pb)))
#'
#' # Base R's txtProgressBar is also supported, but note that it does
#' # not close itself
#' pb <- txtProgressBar(max = 5, style = 3)
#' invisible(lapply(1:5, with_pb(~Sys.sleep(0.5), pb)))
#' close(pb)
#' @export
with_pb <- function(.f, .pb, ...) {
  if (!inherits(.pb, c("txtProgressBar", "progress_bar"))) {
    cl <- paste(class(pb), collapse = ", ")
    stop("`.pb` must be of class `txtProgressBar` or `progress_bar`. ",
         "Given class `", cl, "`.")
  }
  .f <- rlang::as_function(.f)
  function(...) {
    if (inherits(.pb, "progress_bar")) {
      .pb$tick()
    } else {
      i <- .pb$getVal()
      setTxtProgressBar(.pb, i + 1)
    }
    .f(...)
  }
}

#' Extract p value from model object
#'
#' @param object Model fit object (e.g. output of [stats::lm()])
#' @return Model p value, based on f-statistic
#' @export
pvalue <- function(object) {
  stopifnot(inherits(object, "lm"))
  s <- summary(object)
  fstat <- as.list(unname(s[["fstatistic"]]))
  do.call(pf, c(fstat, lower.tail = FALSE))
}

#' Shrink the size of a `randomForest` model object by removing pieces
#' not necessary for prediction
#'
#' The large pieces are `proximity` and `terms`. The `predict` method
#' for `randomForest` objects should be robust to these being `NULL`.
#'
#' @param object Fitted [randomForest::randomForest()] model
#' @return `randomForest` object with excess data removed
#' @export
shrink_randomforest <- function(object) {
  out <- object
  out[["proximity"]] <- NULL
  out[["terms"]] <- NULL
  class(out) <- "randomForest"
  out
}
