#' Normalize soil sand, silt, clay, and rock percentages to 100%
#'
#' If no `Percent_Rock_Fragment` is given (`Percent_Rock_Fragment = NULL`, default), renormalize sand, silt, and
#' clay percents so that they sum to 100%. If rock is given, normalize sand,
#' silt, and clay so that their sum = `Percent_Rock_Fragment - 100`.
#'
#' @param Percent_Sand,Percent_Silt,Percent_Clay Percentages (0 - 100%) of sand, silt, and clay in soil
#' @param Percent_Rock_Fragment Percent rock in soil. If `NULL` (default), re-normalize sand,
#'   silt, and clay to 100%
#' @param df `data.frame` containing soil percentages in columns
#'   `soil_cols`
#' @param add_rock (Logical) If `TRUE`, add rock data
#' @return 
#' @author Alexey Shiklomanov
#' @export
normalize_soil_pct <- function(Percent_Sand, Percent_Silt, Percent_Clay, Percent_Rock_Fragment = NULL) {
  assertthat::assert_that(
    is_positive(Percent_Sand),
    is_positive(Percent_Silt),
    is_positive(Percent_Clay),
    same_length(Percent_Sand, Percent_Silt, Percent_Clay),
    is.null(Percent_Rock_Fragment) || is_positive(Percent_Rock_Fragment),
    is.null(Percent_Rock_Fragment) || same_length(Percent_Sand, Percent_Rock_Fragment)
  )
  ssc_sum <- Percent_Sand + Percent_Silt + Percent_Clay
  if (is.null(Percent_Rock_Fragment)) {
    # Re-normalize sand, silt, and clay to 100%
    out <- lapply(list(Percent_Sand, Percent_Silt, Percent_Clay), `*`, 100 / ssc_sum)
    return(out)
  }
  sum <- Percent_Sand + Percent_Silt + Percent_Clay + Percent_Rock_Fragment
  ratio <- (100 - Percent_Rock_Fragment) / ssc_sum
  ssc_list <- lapply(list(Percent_Sand, Percent_Silt, Percent_Clay), `*`, ratio)
  # c(ssc_list, list(Percent_Rock_Fragment)) # use this if chose sscr = 100%
  out <- lapply(list(Percent_Sand, Percent_Silt, Percent_Clay), `*`, 100 / ssc_sum)
  c(out, list(Percent_Rock_Fragment)) # ssc = 100%
}

#' @rdname normalize_soil_pct
#' @export
normalize_soil_pct_data <- function(df, add_rock = TRUE) {
  soil_cols <- paste0("Percent_", c("Sand", "Silt", "Clay"))
  if (add_rock) {
    soil_cols <- c(soil_cols, "Percent_Rock_Fragment")
  }
  assertthat::assert_that(
    all(assertthat::has_name(df, soil_cols))
  )
  df[, soil_cols] <- do.call(normalize_soil_pct, as.list(df[, soil_cols]))
  df
}
