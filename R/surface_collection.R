# surface_collection.R -- functions related to processing twb2 surface data
# John Bannister
# 


#' Re-factor clod percentage variable
#' 
#' ArcGIS online stores clod percent cover as a factor variable, which is read 
#' through the JSON download as a string variable. This function factors the 
#' variable with labels specifying corresponding % cover.
#' 
#' @param vec Numeric vector. Vector of clod dover factors.
#' @return A vector of labeled factors for clod percent cover.
#' @examples
#' clod_percents(df1$f1.pnt5to3)
clod_percents <- function(vec){
  fac <- factor(vec, levels=c(0:10),
                labels=c(0, seq(5, 95, 10)))
  num <- as.numeric(as.character(fac))
  num
}
