# LIBRARIES AND METADATA
## @knitr libraries ------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(reshape2)
library(readxl)

source("~/R/code/multiplot.r")

# library(data.table)
# library(pwr)
# library(stargazer)
# 
# library(gridExtra)
# library(knitr)
# library(tables)

# IMPORT AND CLEAN DATA
## @knitr data_import ---------------------------------------------------------------
till_df <- read_excel("../data/TwB2_Base_Combine_062315_to JB.xlsx")

## @knitr data_clean -------------------------------------------------------------
names(till_df) <- tolower(names(till_df))
names(till_df) <- gsub("%", "", names(till_df))
names(till_df)[9:12] <- gsub(" ", "", names(till_df)[9:12])
names(till_df) <- gsub(" ", "_", names(till_df))
names(till_df) <- gsub("-", "_", names(till_df))
till_df <- filter(till_df, !is.na(site))
till_df <- select(till_df, -clod_1, -clod_2, -clod_3, -clod_mean, -clod_std, -ratio)

till_melt <- melt(till_df, id=c("site", "subplot", "date", "observer"))

# SUMMARISE ---------------------------------------
# built summary data frames
site_df <- data.frame("site" = unique(till_df$site))
site_df <- inner_join(site_df, select(till_df, site, subplot), by = "site")

subplot_df <- data.frame("subplot" = unique(till_df$subplot))


# @knitr clod_summaries_point -------------------
# mean across three frames per point
till_small <- group_by(till_melt, site) %>% 
  filter(variable == "f1_pnt_0.5_3" | variable == "f2_pnt_0.5_3" | variable == "f3_pnt_0.5_3") %>%
  summarize(small_clod = mean(value))
till_df <- inner_join(till_df, select(till_small, site, small_clod), by = "site")

till_large <- group_by(till_melt, site) %>% 
  filter(variable == "f1_gt3" | variable == "f2_gt3" | variable == "f3_gt3") %>%
  summarize(large_clod = mean(value))
till_df <- inner_join(till_df, select(till_large, site, large_clod), by = "site")

till_total <- group_by(till_melt, site) %>% 
  filter(variable == "f1_gt3" | variable == "f2_gt3" | variable == "f3_gt3" |
           variable == "f1_pnt_0.5_3" | variable == "f2_pnt_0.5_3" | variable == "f3_pnt_0.5_3") %>%
  summarise(total_clod = sum(value)/3)
till_df <- inner_join(till_df, select(till_total, site, total_clod), by = "site")

# @knitr sd_across_frames_point ------------------
# standard deviation across three frames by point
frame_sd_small <- group_by(till_melt, site) %>%
  filter(variable == "f1_pnt_0.5_3" | variable == "f2_pnt_0.5_3" | variable == "f3_pnt_0.5_3") %>%
  summarise(small_sd = sd(value))
till_df <- inner_join(till_df, select(frame_sd_small, site, small_sd), by = "site")

frame_sd_large <- group_by(till_melt, site) %>%
  filter(variable == "f1_gt3" | variable == "f2_gt3" | variable == "f3_gt3") %>%
  summarise(large_sd = sd(value))
till_df <- inner_join(till_df, select(frame_sd_large, site, large_sd), by = "site")

frame_sd_total <- group_by(till_melt, site) %>%
  filter(variable == "f1_gt3" | variable == "f2_gt3" | variable == "f3_gt3" |
           variable == "f1_pnt_0.5_3" | variable == "f2_pnt_0.5_3" | variable == "f3_pnt_0.5_3") %>%
  summarise(total_sd = sd(value))
till_df <- inner_join(till_df, select(frame_sd_total, site, total_sd), by = "site")

# @knitr clod_summaries_subplot -------------------
# mean across all frames in a subplot
plot_small <- group_by(till_melt, subplot) %>% 
  filter(variable == "f1_pnt_0.5_3" | variable == "f2_pnt_0.5_3" | variable == "f3_pnt_0.5_3") %>%
  summarize(small_clod = mean(value))
subplot_df <- inner_join(subplot_df, select(plot_small, subplot, small_clod), by = "subplot")

plot_large <- group_by(till_melt, subplot) %>% 
  filter(variable == "f1_gt3" | variable == "f2_gt3" | variable == "f3_gt3") %>%
  summarize(large_clod = mean(value))
subplot_df <- inner_join(subplot_df, select(plot_large, subplot, large_clod), by = "subplot")

plot_total <- group_by(till_melt, subplot) %>% 
  filter(variable == "f1_gt3" | variable == "f2_gt3" | variable == "f3_gt3" |
           variable == "f1_pnt_0.5_3" | variable == "f2_pnt_0.5_3" | variable == "f3_pnt_0.5_3") %>%
  summarise(total_clod = sum(value)/3)
subplot_df <- inner_join(subplot_df, select(plot_total, subplot, total_clod), by = "subplot")

# @knitr sd_across_frames_subplot ------------------
# standard deviation across all frames in a subplot
plot_sd_small <- group_by(till_melt, subplot) %>%
  filter(variable == "f1_pnt_0.5_3" | variable == "f2_pnt_0.5_3" | variable == "f3_pnt_0.5_3") %>%
  summarise(small_sd = sd(value))
subplot_df <- inner_join(subplot_df, select(plot_sd_small, subplot, small_sd), by = "subplot")

plot_sd_large <- group_by(till_melt, subplot) %>%
  filter(variable == "f1_gt3" | variable == "f2_gt3" | variable == "f3_gt3") %>%
  summarise(large_sd = sd(value))
subplot_df <- inner_join(subplot_df, select(plot_sd_large, subplot, large_sd), by = "subplot")

plot_sd_total <- group_by(till_melt, subplot) %>%
  filter(variable == "f1_gt3" | variable == "f2_gt3" | variable == "f3_gt3" |
           variable == "f1_pnt_0.5_3" | variable == "f2_pnt_0.5_3" | variable == "f3_pnt_0.5_3") %>%
  summarise(total_sd = sd(value))
subplot_df <- inner_join(subplot_df, select(plot_sd_total, subplot, total_sd), by = "subplot")

# PRELIMINARY ANALYSIS -------------------------------------------

spacing_plot <- group_by(till_melt, subplot) %>%
  ggplot(aes(x=subplot, y=value)) +
    geom_boxplot() +
    ylab("ridge_spacing") +
    ggtitle("ridge spacing")
height_plot <- group_by(till_melt, subplot) %>%
  ggplot(aes(x=subplot, y=value)) +
    geom_boxplot() +
    ylab("rh_calc") +
    ggtitle("ridge height")
small_clod_plot <- group_by(till_melt, subplot) %>%
  filter(variable == "small_clod") %>%
  ggplot(aes(x=subplot, y=value)) +
    geom_boxplot() +
    ylab("percentage") +
    ggtitle("0.5in - 3in clods")
large_clod_plot <- group_by(till_melt, subplot) %>%
  filter(variable == "large_clod") %>%
  ggplot(aes(x=subplot, y=value)) +
    geom_boxplot() +
    ylab("percentage") +
    ggtitle(">3in clods")
total_clod_plot <- group_by(till_melt, subplot) %>%
  filter(variable == "total_clod") %>%
  ggplot(aes(x=subplot, y=value)) +
    geom_boxplot() +
    ylab("percentage") +
    ggtitle("total clods")

multiplot(spacing_plot, height_plot, cols=2)
multiplot(small_clod_plot, large_clod_plot, cols=2)
total_clod_plot



