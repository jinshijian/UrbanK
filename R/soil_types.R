#' Conversion table between specific and general soil types
#'
#' @param col Name of column to return. If `NULL` (default), return
#'   the full `data.frame`
#' @inheritParams base::data.frame
#' @return If `is.null(col)`, `data.frame` linking specific (`Type`)
#'   and general (`Top_Type`) soils
#' @author Alexey Shiklomanov
#' @export
soil_types <- function(col = NULL, stringsAsFactors = FALSE) {
  type_df <- data.frame(
    Type = c(
      "fine granular structure", "single grain", "medium granular structure",
      "thin and medium plate-like structure", "massive", "medium subangular blocky",
      "medium and fine granular", "coarse granular blocky", "fine subangular blocky",
      "fine and medium granular structure", "medium platy structure",
      "fine and medium subangular blocky", "fine and medium prismatic structure",
      "medium granular and strong", "medium angular blocky", "fine angular structure",
      "medium prismatic parting to moderate medium subangular blocky",
      "medium prismatic structure parting to moderate medium subangular blocky",
      "coarse prismatic", "medium prismatic", "angular blocky",
      "very coarse prismatic structure", "very fine granular structure",
      "coarse subangular blocky" , "fine subangular and angular blocky",
      "very fine and fine subangular blocky", "medium and coarse subangular blocky",
      "subangular blocky", "fine granular structure and weak very fine subangular blocky"
    ),
    Top_Type = c(
      "granular", "single grain", "granular", "platy", "massive",
      "blocky", "granular", "blocky", "blocky", "granular", "platy",
      "blocky", "prismatic", "granular", "blocky", "blocky", "blocky",
      "blocky", "prismatic", "prismatic", "blocky", "prismatic", "granular",
      "blocky", "blocky", "blocky", "blocky", "blocky", "granular"      
    ),
    stringsAsFactors = stringsAsFactors
  )
  if (is.null(col)) return(type_df)
  type_df[[col]]
}
