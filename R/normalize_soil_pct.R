#' Normalize soil sand, silt, and clay percentages to 100%
#'
#' @param sand,silt,clay Percentages (0 - 100%) of sand, silt, and clay in soil
#' @param df `data.frame` containing soil percentages in columns
#'   `soil_cols`
#' @param soil_cols Column names corresponding to sand, silt, and clay
#' @return 
#' @author Alexey Shiklomanov
#' @export
normalize_soil_pct <- function(sand, silt, clay, add_rock = FALSE) {
  assertthat::assert_that(
    is_positive(sand),
    is_positive(silt),
    is_positive(clay),
    same_length(sand, silt, clay)
  )
  sum <- sand + silt + clay
  if (sum > 100) {
    ratio2 <- 100 / sum
    sand <- sand * ratio2
    silt <- silt * ratio2
    clay <- clay * ratio2
    sum <- 100
  }
  if (add_rock) {
    rock <- 100 - sum
    list(sand, silt, clay, rock)
  } else {
    ratio <- 100 / sum
    lapply(list(sand, silt, clay), `*`, ratio)
  }
}

#' @rdname normalize_soil_pct
#' @export
normalize_soil_pct_data <- function(df,
                                    soil_cols = paste0("Percent_", c("Sand", "Silt", "Clay")),
                                    add_rock = TRUE) {
  assertthat::assert_that(
    all(assertthat::has_name(df, soil_cols))
  )
  out_names <- c("sand", "silt", "clay")
  argv <- as.list(setNames(df[, soil_cols], out_names))
  argv[["add_rock"]] <- add_rock
  if (add_rock) soil_cols <- c(soil_cols, "Percent_Rock")
  df[, soil_cols] <- do.call(normalize_soil_pct, argv)
  df
}
