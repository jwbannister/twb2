#' Factor observed clod percentages.
#' 
#' @param clmn A vector. 
#' @return A vector of length clmn factored according to standard clod percentages
#' @examples 
#' factorClods(data$f1.small)
factorClods <- function(clmn){
  clmn[clmn=="100"] <- "90-100"
  clmn <- factor(clmn, levels=c("0", "1-10", "10-20", "20-30", "30-40", "40-50", 
                                "50-60", "60-70", "70-80", "80-90", "90-100"),
                 labels=c("0", "10", "20", "30", "40", "50", "60", "70", "80",
                          "90", "100"))
  clmn
}


#' Create a list of plots for densities of tillage roughness characteristics.
#' 
#' @param data A melted data frame for tillage roughness data with standard "variable"
#'  and "value" column names.
#' @param compare A character string for the identity variable for which the data 
#'  should be plotted separately in each panel. 
#' @return A ggplot2 object with density plots for the 4 roughness characteristics. 
#' @examples 
#' factorClods(rough_melt, observer)
compareDensity <- function(data, compare){
  plots <- list()
  for (i in 1:length(unique(data$variable))){
    plt <- data %>% filter(variable==unique(data$variable)[i]) %>%
      ggplot(aes(x=value)) +
      geom_density(aes_string(color=compare)) +
      xlab(unique(data$variable)[i])
    plots <- c(plots, list(plt))
  }
  grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], ncol=2)
}

makePNG <- function(plt, filename, ht=6, wt=6, ppi=300){
  png(filename, width=wt*ppi, height=ht*ppi, res=ppi)
  print(plt)
  dev.off()
}