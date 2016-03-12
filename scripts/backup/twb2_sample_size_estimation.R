# Estimation of sample size for future UAV work: UAV will photo clod frames, 
# comparison will be made between clod % determined in UAV photos and clod %
# determined from traditional ground measurement. 
# Matched pairs test, determine power/sample size relations ship. Use existing
# TwB2 clod data to estimate variance and effect size.

library(dplyr)
library(ggplot2)
library(reshape2)
library(readxl)
library(pwr)

till_df <- read_excel("../data/TwB2_Base_Combine_062315_to JB.xlsx")

names(till_df) <- tolower(names(till_df))
names(till_df) <- gsub("%", ".prcnt", names(till_df))
names(till_df)[9:14] <- gsub(" ", "", names(till_df)[9:14])
names(till_df) <- gsub("-", "_", names(till_df))
names(till_df)[9:14] <- gsub("_pnt_0.5_3", ".small", names(till_df)[9:14])
names(till_df)[9:14] <- gsub("_gt3", ".large", names(till_df)[9:14])
names(till_df) <- gsub(" ", ".", names(till_df))
names(till_df) <- gsub("_", ".", names(till_df))

till_df <- filter(till_df, !is.na(site))
till_df <- select(till_df, c(1, 2, 9:14))
till_melt <- melt(till_df, id=c("site", "subplot"))
till_melt$frame <- substr(till_melt$variable, 1, 2)
till_melt$size <- substr(till_melt$variable, 4, 8)
till_melt <- select(till_melt, -variable)
names(till_melt)[3] <- "percent"

# What will we be testing? ground to UAV photo: matched pair. To get an idea of
# what the mean differences and variance might look like, let's look at matched
# pair data from two frames at the same site from the data we have.

find_2_sample_common_sd <- function(sample1, sample2){
  n1 <- length(sample1)
  n2 <- length(sample2)
  var1 <- var(sample1)
  var2 <- var(sample2)
  common_sd <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2)/(n1 + n2 - 2))
  common_sd
}

sampf1 <- filter(till_melt, frame=="f1", size=="small")$percent
sampf2 <- filter(till_melt, frame=="f2", size=="small")$percent
common_sd <- find_2_sample_common_sd(sampf1, sampf2)
d12 <- abs(mean(sampf1) - mean(sampf2))/common_sd

sampf2 <- filter(till_melt, frame=="f2", size=="small")$percent
sampf3 <- filter(till_melt, frame=="f3", size=="small")$percent
common_sd <- find_2_sample_common_sd(sampf2, sampf3)
d23 <- abs(mean(sampf2) - mean(sampf3))/common_sd

mean_d <- mean(d12,d23)

# what sample size is required to determine a difference at the effect size 
# we might estimate from previous observations?
pwr.t.test(d=mean_d, sig.level=0.05, power=0.95, type="paired")

# what sample size is required if we are only interested in seeing a "large"
# effect? 
pwr.t.test(d=.8, sig.level=0.05, power=0.95, type="paired")

# how does required sample size change with d for given sig.level (0.05) and
# power (0.95)?

n_req <- c()
d_var <- c()
for (i in seq(0.1, 1, 0.01)){
  d <- i
  n <- pwr.t.test(d=d, sig.level=0.05, power=0.95, type="paired")$n
  n_req <- c(n_req, n)
  d_var <- c(d_var, d)
}
df <- data.frame(d=d_var, n=n_req)
ggplot(df, aes(x=d, y=n)) +
  geom_path() +
  ylim(0, 200)