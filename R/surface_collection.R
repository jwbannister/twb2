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
#' factor_clods(df1$f1.pnt5to3)
factor_clods <- function(vec){
  fac <- factor(vec, levels=c(0:10),
                labels=c("0%", "1-10%", "10-20%", "20-30%", "30-40%", 
                         "40-50%", "50-60%", "60-70%", "70-80%", "80-90%",
                         "90-100%"))
  fac
}
