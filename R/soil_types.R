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
      "fine and medium granular structure", "medium platy structure", "fine and medium subangular blocky",
      "fine and medium prismatic structure", "medium granular and strong coarse angular blocky", "medium angular blocky",
      "fine angular structure", "medium prismatic parting to moderate medium subangular blocky", "medium prismatic structure parting to moderate medium subangular blocky",
      "coarse prismatic", "medium prismatic", "angular blocky",
      "very coarse prismatic structure", "very fine granular structure", "coarse subangular blocky" ,
      "fine subangular and angular blocky", "very fine and fine subangular blocky", "medium and coarse subangular blocky",
      "subangular blocky", "fine granular structure and weak very fine subangular blocky", "coarse granular"    
      ),
    
    Top_Type = c(
      "GR", "SGR", "GR",
      "PL", "MA", "SBK",
      "GR", "GR", "SBK",
      "GR", "PL", "SBK",
      "PR", "ABK", "ABK",
      "ABK", "PR", "PR",
      "PR", "PR", "ABK",
      "PR", "GR",  "SBK",
      "SBK", "SBK", "SBK",
      "SBK", "GR", "GR"      
    ),
    stringsAsFactors = stringsAsFactors
  )
  if (is.null(col)) return(type_df)
  type_df[[col]]
}

#' Alphabetical list of soil types, to be used as factor levels 
#'
#' @return Character vector of soil types, in alphabetical order
#' @export
soil_type_levels <- function() {
  sort(unique(soil_types("Top_Type")))
}
