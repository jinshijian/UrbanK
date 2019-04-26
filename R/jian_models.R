#' Fit models from Jian et al. paper
#'
#' `fit_jian_ann` fits an artificial neural network, and `fit_jian_rf`
#' fits a random forest model (with or without `top_type` included).
#'
#' @param data Input data for fit.
#' @param top_type Logical. If `TRUE`, include the `Top_Type` as a
#'   predictor in the Random Forest model.
#' @return For `fit_jian_ann`, the output of [neuralnet::neuralnet()],
#'   but with the special class attribute `urbankfs_ann` (so we can
#'   define our own `predict` S3 method). For `fit_jian_rf`, the
#'   output of [randomForest::randomForest()].
#' @author Alexey Shiklomanov
#' @export
fit_jian_ann <- function(data) {
  cols <- c(
    "Unsaturated_K2cm_cmhr",
    paste0("Percent_", c("Sand", "Silt", "Clay"))
  )
  sdata_unscaled <- data[, cols]
  out_scale <- range(sdata_unscaled[["Unsaturated_K2cm_cmhr"]])
  sdata <- purrr::map_dfc(sdata_unscaled, scale_range)
  out <- neuralnet::neuralnet(
    Unsaturated_K2cm_cmhr ~ Percent_Sand + Percent_Silt + Percent_Clay,
    data = sdata,
    hidden = c(5, 3),
    linear.output = TRUE,
    stepmax = 1e6
  )
  class(out) <- c("urbankfs_ann", class(out))
  attr(out, "scale_factors") <- out_scale
  out
}

#' @rdname fit_jian_ann
#' @export
fit_jian_rf <- function(data, top_type = FALSE) {
  cols <- c(
    "Unsaturated_K2cm_cmhr",
    paste0("Percent_", c("Sand", "Silt", "Clay"))
  )
  if (top_type) cols <- c(cols, "Top_Type")
  sdata <- data[, cols]
  form_string <- sprintf("%s ~ %s", cols[[1]], paste(
    cols[-1], collapse = " + "
  ))
  form <- as.formula(form_string)
  randomForest::randomForest(
    form,
    data = sdata,
    ntree = 100,
    mtry = 2,
    importance = TRUE,
    proximity = TRUE
  )
}

#' `predict` method for `neuralnet` output
#'
#' @inheritParams stats::predict.lm
#' @param ... Additional arguments to [neuralnet::compute()]
#' @seealso fit_jian_ann
#' @export predict.urbankfs_ann
predict.urbankfs_ann <- function(object, newdata, ...) {
  cols <- paste0("Percent_", c("Sand", "Silt", "Clay"))
  sdata_unscaled <- newdata[, cols]
  sdata <- purrr::map_dfc(sdata_unscaled, scale_range)
  result <- neuralnet::compute(object, sdata, ...)
  out <- result[["net.result"]]
  unscale_range(out, attr(object, "scale_factors"))
}

#' Download full bootstrap (500 fits) of model fits from the Open
#' Science Framework (OSF)
#'
#' The project is hosted at https://osf.io/fzrcq/.
#'
#' @param ... Additional arguments to [utils::download.file()]
#' @inheritParams utils::download.file
#' @return `destfile`, invisibly
#' @export
#' @examples
#' @export
#' \dontrun{
#' load(download_jian_fits("full_model_fits.rda"))
#' }
download_jian_fits <- function(destfile, ...) {
  url <- "https://osf.io/download/ebsym/"
  download.file(url, destfile = destfile, ...)
  invisible(destfile)
}
