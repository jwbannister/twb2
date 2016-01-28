load("./data/clods_sept_22.RData")
source("./scripts/clods_0922_functions.R")
library(ggplot2)
library(dplyr)
library(reshape2)
library(pwr)
library(outliers)

# CHECK FOR NORMALITY AND IDENTIFY OUTLIERS

sort(clods_df$mean.total) # looks like 50 and 53.3 might be low outliers?
grubbs.test(clods_df$mean.total, type=20) # test does not confirm outliers, use all data points

# plot distribution estimate of sample clod cover
p2 <- ggplot(clods_df, aes(x=mean.total)) +
  geom_density() +
  xlab("total clod percentage") +
  ylab("probability density")
makePNG(p2, "./output/distribs.png", wt=4, ht=4)

y <- quantile(clods_df$mean.total, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1] - slope * x[1]
p1 <- ggplot(clods_df, aes(sample=mean.total)) + stat_qq() +
  geom_abline(slope = slope, intercept = int, color="blue")
makePNG(p1, "./output/qqplot.png", wt=4, ht=4)

shapiro.test(clods_df$mean.total)

# CHECK FOR SPATIAL AUTOCORRELATION BETWEEN SITES

clod.dists <- as.matrix(dist(select(clods_df, x, y)))
clod.dists.inv <- 1/clod.dists
diag(clod.dists.inv) <- 0
ape::Moran.I(clods_df$mean.total, clod.dists.inv)

# get means and CIs for groups

# run one-sample t-test to get 95% confidence intervals
t.test(clods_df$mean.total)

sd(clods_df$mean.total)

# what is that power of a t-test that we are within +/- 10% (absolute) of the
# true mean?
pwr.t.test(n=length(clods_df$mean.total), 
           d=10/sd(clods_df$mean.total),
           sig.level=0.05, type="one.sample")

power.table <- data.frame(power=c(seq(0.8, 0.99, 0.01)), 
                          range=rep(10, 20), n=rep(NA, 20))
for (i in 1:20){
  result <- pwr.t.test(n=NULL, power=power.table$power[i],
             d=power.table$range[i]/sd(clods_df$mean.total),
             sig.level=0.05, type="one.sample")
  power.table$n[i] <- result$n
}

p3 <- ggplot(power.table, aes(x=power, y=n)) +
  geom_path()
makePNG(p3, "./output/powers.png", wt=5, ht=4)

comp <- photo_df %>% 
  select(f1.small, f1.large, f2.small, f2.large, f3.small, f3.large,
         id, observer) %>%
  melt(id.vars=c("id", "observer"))
comp$frame <- substr(comp$variable, 1, 2)
comp$variable <- substr(comp$variable, 4, 8)

p4 <- ggplot(comp, aes(x=observer, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ frame) +
  labs(fill="clod size") +
  scale_y_continuous(breaks=seq(0, 100, 10)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
makePNG(p4, "./output/photo_assess.png", wt=5, ht=4)

