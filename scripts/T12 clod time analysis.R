# LIBRARIES AND METADATA
## @knitr libraries ------------------------------------------------------------
library(dplyr)
library(readxl)
library(data.table)
library(ggplot2)
library(pwr)
library(stargazer)
library(reshape2)
library(gridExtra)
library(knitr)
library(tables)

# IMPORT AND CLEAN DATA
## @knitr data_import ---------------------------------------------------------------
full_data <- read_excel("../data/Surface_Data_Compile_2015_Extract.xlsx", sheet=1, skip=1)

## @knitr data_clean -------------------------------------------------------------
names(full_data) <- tolower(names(full_data))
names(full_data) <- gsub("_", ".", names(full_data))
names(full_data) <- gsub(" ", ".", names(full_data))
names(full_data) <- gsub("\\+", "", names(full_data))

# address duplicated columns in data
renameDuplicateColumns <- function(){
  a <- duplicated(names(full_data))
  b <- which(a == TRUE)
  names(full_data)[b] <<- paste(names(full_data)[b], "_1", sep="")
}
renameDuplicateColumns()

identical(full_data$pct.clods.0to3, full_data$pct.clods.0to3_1)
identical(full_data$pct.clods.3to6, full_data$pct.clods.3to6_1)
identical(full_data$pct.clods.6, full_data$pct.clods.6_1)

# reduce data only to variables used in analysis
data <- select(full_data, type, collection.id, mp.id, date, observer, pct.clods.0to3, pct.clods.3to6, pct.clods.6, fr.depth, fr.width)

data$mp.id <- factor(tolower(data$mp.id))
data$observer <- factor(gsub("/", ",", data$observer))

cleanObserver <- function(){
data_observer <- as.character(data$observer)
  for (i in 1:nrow(data)){
    CJ <- grepl("CJ", data_observer[i])
    ED <- grepl("ED", data_observer[i])
    SR <- grepl("SR", data_observer[i])
    MS <- grepl("MS", data_observer[i])
    JL <- grepl("JL", data_observer[i])
    observers <- c("CJ", "ED", "SR", "MS", "JL")
    presence <- c(CJ, ED, SR, MS, JL)
    group <- observers[presence]
    data_observer[i] <- paste(group, collapse=",") 
  }
data_observer <- factor(data_observer)
return(data_observer)
}
data$observer <- cleanObserver()

data$pct.clods.0to3 <- as.numeric(as.character(data$pct.clods.0to3))
data$pct.clods.3to6 <- as.numeric(as.character(data$pct.clods.3to6))
data$pct.clods.6 <- as.numeric(as.character(data$pct.clods.6))
data$fr.depth <- as.numeric(as.character(data$fr.depth))
data$fr.width <- as.numeric(as.character(data$fr.width))
data <- mutate(data, pct.clods.total = pct.clods.0to3 + pct.clods.3to6 + pct.clods.6)
data$type <- factor(data$type)

data <- filter(data, !is.na(pct.clods.total) & !is.na(fr.depth) & !is.na(fr.width))

# SUMMARISE

## @knitr time_grouping -----------------------------------------------------

first_till_date <- data %>% select(collection.id, mp.id, date) %>% group_by(mp.id) %>%  filter(date==first(date))

for (i in 1:nrow(first_till_date)){
  first_till_date$first.till.month[i] <- if (first_till_date$collection.id[i] == 1) "Apr" else "Dec"
}

for (i in 1:nrow(data)){
  data$days.since.till[i] <- data$date[i] - filter(first_till_date, mp.id==data$mp.id[i])$date
  data$first.till.month[i] <- filter(first_till_date, mp.id==data$mp.id[i])$first.till.month
}
data$first.till.month <- factor(data$first.till.month)

data <- data.table(data)
data <- setorder(data, first.till.month, days.since.till)

for (i in 1:nrow(data)){
  if(data$days.since.till[i]==0){
    data$time.group[i] <- 0
  } else{
      if(data$days.since.till[i]-data$days.since.till[i-1]>3){
        data$time.group[i] <- data$time.group[i-1]+1
      } else{
          data$time.group[i] <- data$time.group[i-1]
        }
    }
}
data$time.group <- factor(data$time.group)

# summarise by first.till.month
summary_data <- data %>% group_by(first.till.month, type, time.group) %>% 
  summarise(mean.days.since.till = mean(days.since.till),
            mean.pct.0to3 = mean(pct.clods.0to3), 
            sd.pct.0to3 = sd(pct.clods.0to3), 
            mean.pct.3to6 = mean(pct.clods.3to6), 
            sd.pct.3to6 = sd(pct.clods.3to6),
            mean.pct.6 = mean(pct.clods.6), 
            sd.pct.6 = sd(pct.clods.6),
            mean.pct.total = mean(pct.clods.total), 
            sd.pct.total = sd(pct.clods.total), 
            mean.fr.depth = mean(fr.depth), 
            sd.fr.depth = sd(fr.depth),
            mean.fr.width = mean(fr.width), 
            sd.fr.width = sd(fr.width)) %>%
  arrange(first.till.month, type, time.group)

# EXPLORATION ---------------------------------------------------------

## @knitr summary_stats --------------------------------------------------------
all_time0 <- filter(data, time.group==0) %>% 
  select(pct.clods.0to3, pct.clods.3to6, pct.clods.6, pct.clods.total, fr.width, fr.depth) %>%
  summarise(length(pct.clods.0to3), mean(pct.clods.0to3), sd(pct.clods.0to3), mean(pct.clods.3to6), sd(pct.clods.3to6), mean(pct.clods.6), sd(pct.clods.6), mean(pct.clods.total), sd(pct.clods.total), mean(fr.depth), sd(fr.depth), mean(fr.width), sd(fr.width))

tilld_time0 <- filter(data, time.group==0 & type=="TILL(D)") %>% 
  select(pct.clods.0to3, pct.clods.3to6, pct.clods.6, pct.clods.total, fr.width, fr.depth) %>%
  summarise(length(pct.clods.0to3), mean(pct.clods.0to3), sd(pct.clods.0to3), mean(pct.clods.3to6), sd(pct.clods.3to6), mean(pct.clods.6), sd(pct.clods.6), mean(pct.clods.total), sd(pct.clods.total), mean(fr.depth), sd(fr.depth), mean(fr.width), sd(fr.width))

tilln_time0 <- filter(data, time.group==0 & type=="TILL(N)") %>% 
  select(pct.clods.0to3, pct.clods.3to6, pct.clods.6, pct.clods.total, fr.width, fr.depth) %>%
  summarise(length(pct.clods.0to3), mean(pct.clods.0to3), sd(pct.clods.0to3), mean(pct.clods.3to6), sd(pct.clods.3to6), mean(pct.clods.6), sd(pct.clods.6), mean(pct.clods.total), sd(pct.clods.total), mean(fr.depth), sd(fr.depth), mean(fr.width), sd(fr.width))

till_time0 <- filter(data, time.group==0 & type=="TILL") %>% 
  select(pct.clods.0to3, pct.clods.3to6, pct.clods.6, pct.clods.total, fr.width, fr.depth) %>%
  summarise(length(pct.clods.0to3), mean(pct.clods.0to3), sd(pct.clods.0to3), mean(pct.clods.3to6), sd(pct.clods.3to6), mean(pct.clods.6), sd(pct.clods.6), mean(pct.clods.total), sd(pct.clods.total), mean(fr.depth), sd(fr.depth), mean(fr.width), sd(fr.width))

apr_time0<- filter(data, time.group==0 & first.till.month=="Apr") %>% 
  select(pct.clods.0to3, pct.clods.3to6, pct.clods.6, pct.clods.total, fr.width, fr.depth) %>%
  summarise(length(pct.clods.0to3), mean(pct.clods.0to3), sd(pct.clods.0to3), mean(pct.clods.3to6), sd(pct.clods.3to6), mean(pct.clods.6), sd(pct.clods.6), mean(pct.clods.total), sd(pct.clods.total), mean(fr.depth), sd(fr.depth), mean(fr.width), sd(fr.width))

dec_time0 <- filter(data, time.group==0 & first.till.month=="Dec") %>% 
  select(pct.clods.0to3, pct.clods.3to6, pct.clods.6, pct.clods.total, fr.width, fr.depth) %>%
  summarise(length(pct.clods.0to3), mean(pct.clods.0to3), sd(pct.clods.0to3), mean(pct.clods.3to6), sd(pct.clods.3to6), mean(pct.clods.6), sd(pct.clods.6), mean(pct.clods.total), sd(pct.clods.total), mean(fr.depth), sd(fr.depth), mean(fr.width), sd(fr.width))

summary_time0 <- rbind(all_time0, till_time0, tilln_time0, tilld_time0, apr_time0, dec_time0)
row.names(summary_time0) <- c("all_time0", "till_time0", "tilln_time0", "tilld_time0", "apr_time0", "dec_time0")
setnames(summary_time0, "length(pct.clods.0to3)", "n")
summary_time0 <- round(summary_time0, 2)

# EXPLORATORY

## @knitr mean_compare ---------------------------------------------------------
# comparison of initial means and densities

data_time0 <- filter(data, time.group==0)

n_till <- summary_time0[['all_time0', 'n']]
n_tilld <- summary_time0[['tilld_time0', 'n']]
n_tilln <- summary_time0[['tilln_time0', 'n']]
n_apr <- summary_time0[['apr_time0', 'n']]
n_dec <- summary_time0[['dec_time0', 'n']]

# function to find effect size for power calculations
findEffect <- function(sample1, sample2, characteristic){
  n1 <- summary_time0[[sample1, 'n']]
  mean1 <- summary_time0[[sample1, paste0('mean(', characteristic, ')')]]
  sd1 <- summary_time0[[sample1, paste0('sd(', characteristic, ')')]]
  n2 <- summary_time0[[sample2, 'n']]
  mean2 <- summary_time0[[sample2, paste0('mean(', characteristic, ')')]]
  sd2 <- summary_time0[[sample2, paste0('sd(', characteristic, ')')]]
  com_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2)/(n1 + n2 -2))
  d <- abs(mean1 - mean2)/com_sd
  d
}

df_0to3 <- data.frame(t.test=c('TILL <-> TILL(D)', 'TILL <-> TILL(N)', 'TILL(D) <-> TILL(N)', 'APR <-> DEC'), p.value=rep(NA, 4), power=rep(NA, 4))
df_0to3$p.value[1] <- t.test(data_time0[type=="TILL"]$pct.clods.0to3, data_time0[type=="TILL(D)"]$pct.clods.0to3)$p.value
df_0to3$p.value[2] <- t.test(data_time0[type=="TILL"]$pct.clods.0to3, data_time0[type=="TILL(N)"]$pct.clods.0to3)$p.value
df_0to3$p.value[3] <- t.test(data_time0[type=="TILL(D)"]$pct.clods.0to3, data_time0[type=="TILL(N)"]$pct.clods.0to3)$p.value
df_0to3$p.value[4] <- t.test(data_time0[first.till.month=="Apr"]$pct.clods.0to3, data_time0[first.till.month=="Dec"]$pct.clods.0to3)$p.value

df_0to3$power[1] <- pwr.t2n.test(n1=n_till, n2=n_tilld, d=findEffect('till_time0', 'tilld_time0', 'pct.clods.0to3'), sig.level=0.05)$power
df_0to3$power[2] <- pwr.t2n.test(n1=n_till, n2=n_tilln, d=findEffect('till_time0', 'tilln_time0', 'pct.clods.0to3'), sig.level=0.05)$power
df_0to3$power[3] <- pwr.t2n.test(n1=n_tilld, n2=n_tilln, d=findEffect('tilld_time0', 'tilln_time0', 'pct.clods.0to3'), sig.level=0.05)$power
df_0to3$power[4] <- pwr.t2n.test(n1=n_apr, n2=n_dec, d=findEffect('apr_time0', 'dec_time0', 'pct.clods.0to3'), sig.level=0.05)$power

p1a <- tableGrob(df_0to3)
p1 <- ggplot(filter(data_time0), aes(pct.clods.0to3, group=type)) + 
  geom_density(aes(colour=interaction(first.till.month,type))) +
  theme(legend.title=element_blank())

df_3to6 <- data.frame(t.test=c('TILL <-> TILL(D)', 'TILL <-> TILL(N)', 'TILL(D) <-> TILL(N)', 'APR <-> DEC'), p.value=rep(NA, 4), power=rep(NA, 4))
df_3to6$p.value[1] <- t.test(data_time0[type=="TILL"]$pct.clods.3to6, data_time0[type=="TILL(D)"]$pct.clods.3to6)$p.value
df_3to6$p.value[2] <- t.test(data_time0[type=="TILL"]$pct.clods.3to6, data_time0[type=="TILL(N)"]$pct.clods.3to6)$p.value
df_3to6$p.value[3] <- t.test(data_time0[type=="TILL(D)"]$pct.clods.3to6, data_time0[type=="TILL(N)"]$pct.clods.3to6)$p.value
df_3to6$p.value[4] <- t.test(data_time0[first.till.month=="Apr"]$pct.clods.3to6, data_time0[first.till.month=="Dec"]$pct.clods.3to6)$p.value

df_3to6$power[1] <- pwr.t2n.test(n1=n_till, n2=n_tilld, d=findEffect('till_time0', 'tilld_time0', 'pct.clods.3to6'), sig.level=0.05)$power
df_3to6$power[2] <- pwr.t2n.test(n1=n_till, n2=n_tilln, d=findEffect('till_time0', 'tilln_time0', 'pct.clods.3to6'), sig.level=0.05)$power
df_3to6$power[3] <- pwr.t2n.test(n1=n_tilld, n2=n_tilln, d=findEffect('tilld_time0', 'tilln_time0', 'pct.clods.3to6'), sig.level=0.05)$power
df_3to6$power[4] <- pwr.t2n.test(n1=n_apr, n2=n_dec, d=findEffect('apr_time0', 'dec_time0', 'pct.clods.3to6'), sig.level=0.05)$power

p2a <- tableGrob(df_3to6)
p2 <- ggplot(filter(data_time0), aes(pct.clods.3to6, group=type)) + 
  geom_density(aes(colour=interaction(first.till.month,type))) +
  theme(legend.title=element_blank())

df_6 <- data.frame(t.test=c('TILL <-> TILL(D)', 'TILL <-> TILL(N)', 'TILL(D) <-> TILL(N)', 'APR <-> DEC'), p.value=rep(NA, 4), power=rep(NA, 4))
df_6$p.value[1] <- t.test(data_time0[type=="TILL"]$pct.clods.6, data_time0[type=="TILL(D)"]$pct.clods.6)$p.value
df_6$p.value[2] <- t.test(data_time0[type=="TILL"]$pct.clods.6, data_time0[type=="TILL(N)"]$pct.clods.6)$p.value
df_6$p.value[3] <- t.test(data_time0[type=="TILL(D)"]$pct.clods.6, data_time0[type=="TILL(N)"]$pct.clods.6)$p.value
df_6$p.value[4] <- t.test(data_time0[first.till.month=="Apr"]$pct.clods.6, data_time0[first.till.month=="Dec"]$pct.clods.6)$p.value

df_6$power[1] <- pwr.t2n.test(n1=n_till, n2=n_tilld, d=findEffect('till_time0', 'tilld_time0', 'pct.clods.6'), sig.level=0.05)$power
df_6$power[2] <- pwr.t2n.test(n1=n_till, n2=n_tilln, d=findEffect('till_time0', 'tilln_time0', 'pct.clods.6'), sig.level=0.05)$power
df_6$power[3] <- pwr.t2n.test(n1=n_tilld, n2=n_tilln, d=findEffect('tilld_time0', 'tilln_time0', 'pct.clods.6'), sig.level=0.05)$power
df_6$power[4] <- pwr.t2n.test(n1=n_apr, n2=n_dec, d=findEffect('apr_time0', 'dec_time0', 'pct.clods.6'), sig.level=0.05)$power

p3a <- tableGrob(df_6)
p3 <- ggplot(filter(data_time0), aes(pct.clods.6, group=type)) + 
  geom_density(aes(colour=interaction(first.till.month,type))) +
  theme(legend.title=element_blank())

df_total <- data.frame(t.test=c('TILL <-> TILL(D)', 'TILL <-> TILL(N)', 'TILL(D) <-> TILL(N)', 'APR <-> DEC'), p.value=rep(NA, 4), power=rep(NA, 4))
df_total$p.value[1] <- t.test(data_time0[type=="TILL"]$pct.clods.total, data_time0[type=="TILL(D)"]$pct.clods.total)$p.value
df_total$p.value[2] <- t.test(data_time0[type=="TILL"]$pct.clods.total, data_time0[type=="TILL(N)"]$pct.clods.total)$p.value
df_total$p.value[3] <- t.test(data_time0[type=="TILL(D)"]$pct.clods.total, data_time0[type=="TILL(N)"]$pct.clods.total)$p.value
df_total$p.value[4] <- t.test(data_time0[first.till.month=="Apr"]$pct.clods.total, data_time0[first.till.month=="Dec"]$pct.clods.total)$p.value

df_total$power[1] <- pwr.t2n.test(n1=n_till, n2=n_tilld, d=findEffect('till_time0', 'tilld_time0', 'pct.clods.total'), sig.level=0.05)$power
df_total$power[2] <- pwr.t2n.test(n1=n_till, n2=n_tilln, d=findEffect('till_time0', 'tilln_time0', 'pct.clods.total'), sig.level=0.05)$power
df_total$power[3] <- pwr.t2n.test(n1=n_tilld, n2=n_tilln, d=findEffect('tilld_time0', 'tilln_time0', 'pct.clods.total'), sig.level=0.05)$power
df_total$power[4] <- pwr.t2n.test(n1=n_apr, n2=n_dec, d=findEffect('apr_time0', 'dec_time0', 'pct.clods.total'), sig.level=0.05)$power

p4a <- tableGrob(df_total)
p4 <- ggplot(filter(data_time0), aes(pct.clods.total, group=type)) + 
  geom_density(aes(colour=interaction(first.till.month,type))) +
  theme(legend.title=element_blank())

df_fr.depth <- data.frame(t.test=c('TILL <-> TILL(D)', 'TILL <-> TILL(N)', 'TILL(D) <-> TILL(N)', 'APR <-> DEC'), p.value=rep(NA, 4), power=rep(NA, 4))
df_fr.depth$p.value[1] <- t.test(data_time0[type=="TILL"]$fr.depth, data_time0[type=="TILL(D)"]$fr.depth)$p.value
df_fr.depth$p.value[2] <- t.test(data_time0[type=="TILL"]$fr.depth, data_time0[type=="TILL(N)"]$fr.depth)$p.value
df_fr.depth$p.value[3] <- t.test(data_time0[type=="TILL(D)"]$fr.depth, data_time0[type=="TILL(N)"]$fr.depth)$p.value
df_fr.depth$p.value[4] <- t.test(data_time0[first.till.month=="Apr"]$fr.depth, data_time0[first.till.month=="Dec"]$fr.depth)$p.value

df_fr.depth$power[1] <- pwr.t2n.test(n1=n_till, n2=n_tilld, d=findEffect('till_time0', 'tilld_time0', 'fr.depth'), sig.level=0.05)$power
df_fr.depth$power[2] <- pwr.t2n.test(n1=n_till, n2=n_tilln, d=findEffect('till_time0', 'tilln_time0', 'fr.depth'), sig.level=0.05)$power
df_fr.depth$power[3] <- pwr.t2n.test(n1=n_tilld, n2=n_tilln, d=findEffect('tilld_time0', 'tilln_time0', 'fr.depth'), sig.level=0.05)$power
df_fr.depth$power[4] <- pwr.t2n.test(n1=n_apr, n2=n_dec, d=findEffect('apr_time0', 'dec_time0', 'fr.depth'), sig.level=0.05)$power

p5a <- tableGrob(df_fr.depth)
p5 <- ggplot(filter(data_time0), aes(fr.depth, group=type)) + 
  geom_density(aes(colour=interaction(first.till.month,type))) +
  theme(legend.title=element_blank())

df_fr.width <- data.frame(t.test=c('TILL <-> TILL(D)', 'TILL <-> TILL(N)', 'TILL(D) <-> TILL(N)', 'APR <-> DEC'), p.value=rep(NA, 4), power=rep(NA, 4))
df_fr.width$p.value[1] <- t.test(data_time0[type=="TILL"]$fr.width, data_time0[type=="TILL(D)"]$fr.width)$p.value
df_fr.width$p.value[2] <- t.test(data_time0[type=="TILL"]$fr.width, data_time0[type=="TILL(N)"]$fr.width)$p.value
df_fr.width$p.value[3] <- t.test(data_time0[type=="TILL(D)"]$fr.width, data_time0[type=="TILL(N)"]$fr.width)$p.value
df_fr.width$p.value[4] <- t.test(data_time0[first.till.month=="Apr"]$fr.width, data_time0[first.till.month=="Dec"]$fr.width)$p.value

df_fr.width$power[1] <- pwr.t2n.test(n1=n_till, n2=n_tilld, d=findEffect('till_time0', 'tilld_time0', 'fr.width'), sig.level=0.05)$power
df_fr.width$power[2] <- pwr.t2n.test(n1=n_till, n2=n_tilln, d=findEffect('till_time0', 'tilln_time0', 'fr.width'), sig.level=0.05)$power
df_fr.width$power[3] <- pwr.t2n.test(n1=n_tilld, n2=n_tilln, d=findEffect('tilld_time0', 'tilln_time0', 'fr.width'), sig.level=0.05)$power
df_fr.width$power[4] <- pwr.t2n.test(n1=n_apr, n2=n_dec, d=findEffect('apr_time0', 'dec_time0', 'fr.width'), sig.level=0.05)$power

p6a <- tableGrob(df_fr.width)
p6 <- ggplot(filter(data_time0), aes(fr.width, group=type)) + 
  geom_density(aes(colour=interaction(first.till.month,type))) +
  theme(legend.title=element_blank())

## @knitr display_mean_plots ------------------------------------------------
grid.arrange(p1, p1a, ncol=2,  main="T-Test H0: abs(mu1 - mu2) = 0")
grid.arrange(p2, p2a, ncol=2,  main="T-Test H0: abs(mu1 - mu2) = 0")
grid.arrange(p3, p3a, ncol=2,  main="T-Test H0: abs(mu1 - mu2) = 0")
grid.arrange(p4, p4a, ncol=2,  main="T-Test H0: abs(mu1 - mu2) = 0")
grid.arrange(p5, p5a, ncol=2,  main="T-Test H0: abs(mu1 - mu2) = 0")
grid.arrange(p6, p6a, ncol=2,  main="T-Test H0: abs(mu1 - mu2) = 0")

## TIME GRAPHS

## @knitr build_time_graphs ------------------------------------------------
t1 <- ggplot(summary_data, aes(x=mean.days.since.till, y=mean.pct.0to3)) +
  geom_point(aes(colour=interaction(first.till.month,type))) +
  geom_smooth(aes(colour=interaction(first.till.month,type))) +
  theme(legend.title=element_blank())

t2 <- ggplot(summary_data, aes(x=mean.days.since.till, y=mean.pct.3to6)) +
  geom_point(aes(colour=interaction(first.till.month,type))) +
  geom_smooth(aes(colour=interaction(first.till.month,type))) +
  theme(legend.title=element_blank())

t3 <- ggplot(summary_data, aes(x=mean.days.since.till, y=mean.pct.6)) +
  geom_point(aes(colour=interaction(first.till.month,type))) +
  geom_smooth(aes(colour=interaction(first.till.month,type))) +
  theme(legend.title=element_blank())

t4 <- ggplot(summary_data, aes(x=mean.days.since.till, y=mean.pct.total)) +
  geom_point(aes(colour=interaction(first.till.month,type))) +
  geom_smooth(aes(colour=interaction(first.till.month,type))) +
  theme(legend.title=element_blank())

t5 <- ggplot(summary_data, aes(x=mean.days.since.till, y=mean.fr.depth)) +
  geom_point(aes(colour=interaction(first.till.month,type))) +
  geom_smooth(aes(colour=interaction(first.till.month,type))) +
  theme(legend.title=element_blank())

t6 <- ggplot(summary_data, aes(x=mean.days.since.till, y=mean.fr.width)) +
  geom_point(aes(colour=interaction(first.till.month,type))) +
  geom_smooth(aes(colour=interaction(first.till.month,type))) +
  theme(legend.title=element_blank())

## @knitr display_time_graphs --------------------------------------------------
grid.arrange(t1, t2, ncol=2)
grid.arrange(t3, t4, ncol=2)
grid.arrange(t5, t6, ncol=2)

# COMPARATIVE ------------------------------------------------------------------

# add observer dummy variables
for (i in 1:nrow(data)){
  data$cj[i] <- grepl("CJ", data$observer[i])
  data$ed[i] <- grepl("ED", data$observer[i])
  data$sr[i] <- grepl("SR", data$observer[i])
  data$jl[i] <- grepl("JL", data$observer[i])
  data$ms[i] <- grepl("MS", data$observer[i])
}

# @knitr reg_dec_type ------------------------------------------------------------
lm_0to3 <- lm(pct.clods.0to3~days.since.till+type+observer, data=filter(data, first.till.month=="Dec"))
lm_3to6 <- lm(pct.clods.3to6~days.since.till+type+observer, data=filter(data, first.till.month=="Dec"))
lm_6 <- lm(pct.clods.6~days.since.till+type+observer, data=filter(data, first.till.month=="Dec"))
lm_total <- lm(pct.clods.total~days.since.till+type+observer, data=filter(data, first.till.month=="Dec"))
stargazer(lm_0to3, lm_3to6, lm_6, lm_total, title="Regression Results")

lm_fr.depth <- lm(fr.depth~days.since.till+type+observer, data=filter(data, first.till.month=="Dec"))
lm_fr.width <- lm(fr.width~days.since.till+type+observer, data=filter(data, first.till.month=="Dec"))
stargazer(lm_fr.depth, lm_fr.width, title="Regression Results")

# @knitr reg_first.till.month --------------------------------------------
lm2_0to3 <- lm(pct.clods.0to3~days.since.till+type+cj+ed+sr+jl+ms, data=data)
lm2_3to6 <- lm(pct.clods.3to6~days.since.till+type+cj+ed+sr+jl+ms, data=data)
lm2_6 <- lm(pct.clods.6~days.since.till+type+cj+ed+sr+jl+ms, data=data)
lm2_total <- lm(pct.clods.total~days.since.till+type+cj+ed+sr+jl+ms, data=data)
stargazer(lm2_0to3, lm2_3to6, lm2_6, lm2_total, title="Regression Results")

# SAMPLE SIZE AND POWER CALCULATIONS -------------------------------------

## @knitr build_power_plots ----------------------------------------------------------

pwr_paired <- data.frame(n=rep(NA, 50), d02=rep(NA, 50), d05=rep(NA, 50), d08=rep(NA, 50))
for (i in 1:nrow(pwr_paired)){
  n <- i + 1
  pwr_paired$n[i] <- n
  pwr_paired$d02[i] <- pwr.t.test(n=n, d=.2, sig.level=.05, type="paired")$power
  pwr_paired$d05[i] <- pwr.t.test(n=n, d=.5, sig.level=.05, type="paired")$power
  pwr_paired$d08[i] <- pwr.t.test(n=n, d=.8, sig.level=.05, type="paired")$power
}
pwr_plot1 <- ggplot(pwr_paired, aes(x=n)) + 
  geom_line(aes(y=d02, colour="d = 0.2")) +
  geom_line(aes(y=d05, colour="d = 0.5")) +
  geom_line(aes(y=d08, colour="d = 0.8")) +
  labs(colour='effect size') +
  ylab('power') +
  xlab('sample size (# of pairs)') + 
  ggtitle('Paired T-Test')

pwr_ind <- data.frame(n=rep(NA, 50), d02=rep(NA, 50), d05=rep(NA, 50), d08=rep(NA, 50))
for (i in 1:nrow(pwr_ind)){
  n <- i + 1
  pwr_ind$n[i] <- n
  pwr_ind$d02[i] <- pwr.t.test(n=n, d=.2, sig.level=.05, type="two.sample")$power
  pwr_ind$d05[i] <- pwr.t.test(n=n, d=.5, sig.level=.05, type="two.sample")$power
  pwr_ind$d08[i] <- pwr.t.test(n=n, d=.8, sig.level=.05, type="two.sample")$power
}
pwr_plot2 <- ggplot(pwr_ind, aes(x=n)) + 
  geom_line(aes(y=d02, colour="d = 0.2")) +
  geom_line(aes(y=d05, colour="d = 0.5")) +
  geom_line(aes(y=d08, colour="d = 0.8")) +
  labs(colour='effect size') +
  ylab('power') +
  xlab('sample size (each group)') + 
  ggtitle('Independent Samples T-Test')

## @knitr display_power_plots--------------------------------------------------
grid.arrange(pwr_plot1, pwr_plot2, ncol=2)
