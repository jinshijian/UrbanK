#' Normalize soil sand, silt, and clay percentages to 100%
#'
#' @param sand,silt,clay Percentages (0 - 100%) of sand, silt, and clay in soil
#' @param df `data.frame` containing soil percentages in columns
#'   `soil_cols`
#' @param soil_cols Column names corresponding to sand, silt, and clay
#' @return 
#' @author Alexey Shiklomanov
#' @export
normalize_soil_pct <- function(sand, silt, clay) {
  assertthat::assert_that(
    is_positive(sand),
    is_positive(silt),
    is_positive(clay),
    same_length(sand, silt, clay)
  )
  sum <- sand + silt + clay
  ratio <- 100 / sum
  lapply(list(sand, silt, clay), `*`, ratio)
}

#' @rdname normalize_soil_pct
#' @export
normalize_soil_pct_data <- function(df,
                                    soil_cols = paste0("Percent_", c("Sand", "Silt", "Clay"))) {
  assertthat::assert_that(
    assertthat::has_name(df, soil_cols)
  )
  df[, soil_cols] <- do.call(
    normalize_soil_pct,
    setNames(df[, soil_cols], c("sand", "silt", "clay"))
  )
  df
}
