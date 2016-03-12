load("../output/twb2_clod_data.RData")

library(dplyr)
library(ggplot2)

twb2_frames <- twb2_melt
twb2_frames$percent <- twb2_frames$percent / 100
# T-29-4 has basically no clods, so remove
twb2_frames <- filter(twb2_frames, subplot!="T-29-4")

twb2_frames %>% filter(clods!="total") %>%
  ggplot(aes(x=percent)) +
  geom_histogram() +
  facet_grid(clods ~ ., scales="free_y")

twb2_frames %>% filter(clods!="total") %>%
  ggplot(aes(x=subplot)) +
  geom_point(aes(y=percent), position=position_jitter(w=.05, h=.05)) +
  facet_grid(clods ~ ., scales="free_y")

for (i in 1:nrow(twb2_frames)){
  twb2_frames$large.clod.capacity[i] <- if (twb2_frames$subplot[i] %in% c("T2-2", "T5-4")) FALSE else TRUE
}

twb2_frames %>%
  ggplot(aes(x=percent)) +
  geom_density(aes(fill=large.clod.capacity), alpha=0.5) +
  facet_grid(clods ~ ., scales="free_y")

twb2_sites <- twb2_frames %>% group_by(site, clods) %>%
  summarize(subplot=unique(subplot), large.clod.capacity=unique(large.clod.capacity), 
            site.mean=mean(percent), frame.spread=max(percent) - min(percent))

twb2_sites %>%
  ggplot(aes(x=site.mean)) +
  geom_density(aes(fill=large.clod.capacity), alpha=0.5) +
  facet_grid(clods ~ ., scales="free_y")

twb2_sites %>%
  ggplot(aes(x=frame.spread)) +
  geom_density(aes(color=large.clod.capacity))

twb2_subplots <- twb2_frames %>% group_by(subplot, clods) %>% filter(clods=="total") %>%
  summarize(large.clod.capacity=unique(large.clod.capacity), subplot.mean=mean(percent), 
            subplot.sd=sd(percent), subplot.cv=sd(percent)/mean(percent))

twb2_frames %>% filter(clods=="total") %>%
  ggplot(aes(x=percent)) +
  geom_density(aes(fill=large.clod.capacity)) +
  facet_grid(subplot ~ ., scales="free_y")

large_clod_areas <- filter(twb2_frames, large.clod.capacity==TRUE, clods=="total")$percent
large_ci <- simulateNonParSample(large_clod_areas, 100)
large_ci <- mutate(large_ci, large.clod.capacity=TRUE)

small_clod_areas <- filter(twb2_frames, large.clod.capacity==FALSE, clods=="total")$percent
small_ci <- simulateNonParSample(small_clod_areas, 100)
small_ci <- mutate(small_ci, large.clod.capacity=FALSE)

ci_spread <- rbind(large_ci, small_ci)

ggplot(ci_spread, aes(x=n, y=ci.spread)) +
  geom_path(aes(color=large.clod.capacity)) +
  scale_y_continuous(breaks=seq(0, 1, .2)) +
  scale_x_continuous(breaks=seq(10, 100, 10))

T2_2 <- simulateNonParSubplot("T2-2")
T2_3 <- simulateNonParSubplot("T2-3")
T2_4 <- simulateNonParSubplot("T2-4")
T3_SE <- simulateNonParSubplot("T3-SE")
T3_SW <- simulateNonParSubplot("T3-SW")
T5_4 <- simulateNonParSubplot("T5-4")

ci_subs <- rbind(T2_2, T2_3, T2_4, T3_SE, T3_SW, T5_4)

ggplot(ci_subs, aes(x=n, y=ci.spread)) +
  geom_path(aes(color=subplot)) +
  scale_y_continuous(breaks=seq(0, 1, .2)) +
  scale_x_continuous(breaks=seq(10, 100, 10))

spread <- twb2_frames %>% ungroup() %>% group_by(site) %>% filter(clods=="total") %>%
  mutate(site.spread = max(percent) - min(percent)) %>% ungroup() %>% group_by(subplot) %>% 
  mutate(subplot.spread = max(percent) - min(percent)) %>% filter(frame=="f1") %>%
  select(-frame, -clods, -above.50, -large.clod.capacity, -date, -percent)

ggplot(spread, aes(x=subplot, y=subplot.spread)) +
  geom_point(size=4, color="red") +
  geom_point(aes(y=site.spread), size=2, color="red", alpha=0.5) +
  ylab("spread") +
  geom_text(aes(label=subplot, x=subplot, y=subplot.spread, hjust=0))



