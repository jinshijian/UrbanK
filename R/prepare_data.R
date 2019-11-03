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
    dplyr::filter_all(purrr::negate(is.na)) %>%
    normalize_soil_pct_data(add_rock = use_rock)
}
