#' Prepare data for ANN or random forest models
#'
#' @return `data.frame` with only the necessary columns and no missing data
#' @author Alexey Shiklomanov
#' @inheritParams fit_jian_ann
#' @export
prepare_data <- function(data, use_rock = FALSE, top_type = FALSE) {
  cols <- c(
    "Unsaturated_K2cm_cmhr",
    paste0("Percent_", c("Sand", "Silt", "Clay"))
  )
  if (use_rock) cols <- c(cols, "Percent_Rock_Fragment")
  if (top_type) cols <- c(cols, "Top_Type")
  data %>%
    dplyr::select(cols) %>%
    # Log-transform K2
    dplyr::mutate(log_Unsaturated_K2 = log(Unsaturated_K2cm_cmhr)) %>%
    # Remove NA and infinite values
    # NOTE: When K2 == 0, log(K2) == -Inf, so this removes all 0 values!
    dplyr::filter_all(is.finite) %>%
    normalize_soil_pct_data(add_rock = use_rock)
}
