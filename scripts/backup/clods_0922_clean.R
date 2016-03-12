load("./data/clods_sept_22.RData")
source("./scripts/clods_0922_functions.R")

clods_df$f1.total <- clods_df$f1.small + clods_df$f1.large
clods_df$f2.total <- clods_df$f2.small + clods_df$f2.large
clods_df$f3.total <- clods_df$f3.small + clods_df$f3.large

clods_df$f1.total[clods_df$f1.total > 100] <- 100
clods_df$f2.total[clods_df$f2.total > 100] <- 100
clods_df$f3.total[clods_df$f3.total > 100] <- 100

for (i in 1:nrow(clods_df)){
  clods_df$mean.total[i] <- mean(c(clods_df$f1.total[i], clods_df$f2.total[i],
                                   clods_df$f3.total[i]))
  clods_df$sd.total[i] <- sd(c(clods_df$f1.total[i], clods_df$f2.total[i],
                               clods_df$f3.total[i]))
  clods_df$limit1[i] <- min(clods_df$f1.total[i], clods_df$f2.total[i],
                            clods_df$f3.total[i])
  clods_df$limit2[i] <- max(clods_df$f1.total[i], clods_df$f2.total[i],
                            clods_df$f3.total[i])
}

photo_df$f1.total <- photo_df$f1.small + photo_df$f1.large
photo_df$f2.total <- photo_df$f2.small + photo_df$f2.large
photo_df$f3.total <- photo_df$f3.small + photo_df$f3.large

photo_df$f1.total[photo_df$f1.total > 100] <- 100
photo_df$f2.total[photo_df$f2.total > 100] <- 100
photo_df$f3.total[photo_df$f3.total > 100] <- 100

for (i in 1:nrow(photo_df)){
  photo_df$mean.total[i] <- mean(c(photo_df$f1.total[i], photo_df$f2.total[i],
                                   photo_df$f3.total[i]))
  photo_df$sd.total[i] <- sd(c(photo_df$f1.total[i], photo_df$f2.total[i],
                               photo_df$f3.total[i]))
  photo_df$limit1[i] <- min(photo_df$f1.total[i], photo_df$f2.total[i],
                            photo_df$f3.total[i])
  photo_df$limit2[i] <- max(photo_df$f1.total[i], photo_df$f2.total[i],
                            photo_df$f3.total[i])
}

photo_df <- rbind(photo_df, select(filter(clods_df, id=="1604-C"), -x, -y))
photo_df$observer <- as.character(photo_df$observer)
photo_df$observer[4] <- "GRND"
photo_df$observer <- factor(photo_df$observer, levels=c("BR", "SR", "ED", "GRND"),
                            ordered=TRUE)

save(clods_df, photo_df, file="./data/clods_sept_22.RData")
