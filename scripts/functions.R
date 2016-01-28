simulateNonParSample <- function(data, n_max){
  ci_df <- data.frame(n=as.numeric(), ci.spread=as.numeric())
  tick <- 10
  pb <- txtProgressBar(min=10, max=n_max, style=3, width=80)
  for (i in seq(10, n_max, 1)){
    n <- i
    ci_n <- c()
    for (j in 1:1000){
      x <- sample(data, size=n, replace=TRUE)
      x <- sort(x)
      med_low <- x[(n / 2) - ((1.96 * sqrt(n)) / 2)]
      med_high <- x[1 + (n / 2) + ((1.96 * sqrt(n)) / 2)]
      ci_n <- c(ci_n, (med_high - med_low))
    }
    ci_df <- rbind(ci_df, data.frame(n=n, ci.spread=mean(ci_n)))
    tick <- tick + 1
    setTxtProgressBar(pb, tick)
  }
  close(pb)
  ci_df
}

simulateNonParSubplot <- function(sub){
  x <- filter(twb2_frames, subplot==sub, clods=="total")$percent
  df <- simulateNonParSample(x, n_max=100)
  df <- mutate(df, subplot=sub)
  df
}
