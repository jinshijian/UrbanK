#' Fit models from Jian et al. paper
#'
#' `fit_jian_ann` fits an artificial neural network, and `fit_jian_rf`
#' fits a random forest model (with or without `top_type` included).
#'
#' @param data Input data for fit.
#' @param use_rock Logical. If `TRUE`, include the `Percent_Rock` as a predictor.
#' @param top_type Logical. If `TRUE`, include the `Top_Type` as a
#'   predictor in the Random Forest model.
#' @param verbose Logical. If `TRUE`, print the number of attempts.
#' @return For `fit_jian_ann`, the output of [neuralnet::neuralnet()],
#'   but with the special class attribute `urbankfs_ann` (so we can
#'   define our own `predict` S3 method). For `fit_jian_rf`, the
#'   output of [randomForest::randomForest()].
#' @author Alexey Shiklomanov
#' @export
fit_jian_ann <- function(data, use_rock = FALSE, verbose = FALSE) {
  cols <- c(
    "Unsaturated_K2cm_cmhr",
    paste0("Percent_", c("Sand", "Silt", "Clay"))
  )
  if (use_rock) cols <- c(cols, "Percent_Rock_Fragment")
  sdata_unscaled <- prepare_data(data, use_rock = use_rock)
  out_scale <- range(sdata_unscaled[["Unsaturated_K2cm_cmhr"]])
  sdata <- purrr::map_dfc(sdata_unscaled, scale_range)
  form <- as.formula(paste("Unsaturated_K2cm_cmhr ~", paste(cols[-1], collapse = " + ")))
  runmodel <- TRUE
  i <- 0
  while(runmodel) {
    i <- i + 1
    if (verbose) message("Attempt ", i)
    out <- neuralnet::neuralnet(
      form,
      data = sdata,
      hidden = c(5, 3),
      linear.output = TRUE,
      stepmax = 1e6
    )
    test_predict <- neuralnet::compute(out, sdata)[["net.result"]]
    runmodel <- any(test_predict < 0)
  }
  
  class(out) <- c("urbankfs_ann", class(out))
  attr(out, "scale_factors") <- out_scale
  attr(out, "use_rock") <- use_rock
  out
}

#' @rdname fit_jian_ann
#' @export
fit_jian_rf <- function(data, use_rock = FALSE, top_type = FALSE) {
  cols <- c(
    "Unsaturated_K2cm_cmhr",
    paste0("Percent_", c("Sand", "Silt", "Clay"))
  )
  if (use_rock) cols <- c(cols, "Percent_Rock_Fragment")
  if (top_type) cols <- c(cols, "Top_Type")
  sdata <- prepare_data(data, use_rock = use_rock, top_type = top_type)
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
  use_rock <- attr(object, "use_rock")
  if (!is.null(use_rock) && use_rock) cols <- c(cols, "Percent_Rock")
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
#' @examples
#' \dontrun{
#' load(download_jian_fits("full_model_fits.rda"))
#' }
#' @export
download_jian_fits <- function(destfile, ...) {
  url <- "https://osf.io/download/ebsym/"
  download.file(url, destfile = destfile, ...)
  invisible(destfile)
}

#' Nicer factor levels for model type
#'
#' @param pretty Where should the pretty name be? If `"name"`
#'   (default), the pretty label is the name and value is the original
#'   (useful for, e.g., [forcats::fct_recode()]). If `"value"`, the
#'   reverse.
#' @return Character vector with
#' @author Alexey Shiklomanov
#' @export
pretty_model_types <- function(pretty = c("name", "value")) {
  pretty <- match.arg(pretty)
  dict <- c(
    "Neural network (no rock)" = "ann",
    "Neural network (with rock)" = "annr",
    "RandomForest (no rock, no type)" = "rf1",
    "RandomForest (with rock, no type)" = "rf1r",
    "RandomForest (no rock, with type)" = "rf2",
    "RandomForest (with rock, with type)" = "rf2r"
  )
  if (pretty == "name") return(dict)
  # If `value`, swap the names and values
  out <- names(dict)
  names(out) <- dict
  out
}
