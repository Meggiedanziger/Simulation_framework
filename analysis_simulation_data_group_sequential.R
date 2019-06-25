setwd("~/Documents/QUEST/PhD/R/Simulation_framework/Simulation_framework")

rm(list = ls())

library(tidyverse)
library(readr)
library(compute.es)

#final <- read.csv(file = "final_3stages_power_0.8")

#final <- read.csv(file = "final_3stages_power_0.8_with_futility_bounds")

final <- read.csv(file = "final_3stages_power_0.8_with_futility_bounds_Pocock")


###===========================================================================================================
### ANALYSIS OF OUTCOME VARIABLES
###===========================================================================================================

success <-
  final %>% 
  filter(H0 == 2)

### boxplot of effect size estimates for each stage
ggplot(aes(x = factor(stage), y = delta_emp), data = success) +
  geom_boxplot(outlier.alpha = .3) +
  geom_hline(aes(yintercept = d), color = "red", lty = 2) +
  facet_wrap(~ d, nrow = 1, ncol = 4) +
  labs(x = "Stage", y = "Empirical effect size estimate") +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12, colour = "black")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text = element_text(size = 12, colour = "black"))


### violin plot of effect size estimates for each stage
# ggplot(aes(x = factor(stage), y = delta_emp), data = success) +
#   geom_violin(stat = "ydensity") +
#   stat_summary(fun.y = median, geom = "point", size = 2, color = "red") +
#   #geom_boxplot(width = 0.2, outlier.alpha = 0.3) +
#   geom_hline(aes(yintercept = d), color = "red", lty = 1) +
#   facet_wrap(~ d, nrow = 1, ncol = 4) +
#   labs(x = "Stage", y = "Mean effect size (with CI)") +
#   theme_bw() +
#   theme(strip.text.x = element_text(size = 12, colour = "black")) +
#   theme(strip.background = element_rect(fill = "white", color = "black")) +
#   theme(axis.title.x = element_text(size = 12)) +
#   theme(axis.title.y = element_text(size = 12))+
#   theme(axis.text = element_text(size = 12, colour = "black"))


### compute CI around effect size
success$CI_lower <- tes(t = success$t_value, n.1 = success$totalN/2, n.2 = success$totalN/2)$l.d
success$CI_upper <- tes(t = success$t_value, n.1 = success$totalN/2, n.2 = success$totalN/2)$u.d  

plot_data <-
  success %>% 
  group_by(d, stage) %>% 
  summarize(mean_d_emp = mean(delta_emp),
            mean_CI_lower = mean(CI_lower),
            mean_CI_upper = mean(CI_upper))

### plot mean effect size estimate and CI for each stage and true effect size
ggplot(aes(x = stage, y = mean_d_emp), data = plot_data) +
  geom_errorbar(aes(ymin = mean_d_emp - mean_CI_lower, 
                ymax = mean_d_emp + mean_CI_upper), width = 0.03) +
  geom_point(color = "red", size = 1.9, alpha = .7) +
  geom_hline(aes(yintercept = d), color = "red", lty = 2) +
  facet_wrap(~ d, nrow = 4, ncol = 1) +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  labs(x = "Stage", y = "Mean effect size (with CI)") +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12, colour = "black")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text = element_text(size = 12, colour = "black"))

## compute power / type I error rate from significant results
final_success <-
  final %>%
  group_by(d, stage, totalN, H0) %>% 
  summarize(N = n()) %>% 
  mutate(percent = (N/10000)*100)

type_1_error <-
  final_success %>% 
  filter(d == 0 & H0 == 2)

### plot type I error rate for d = 0
ggplot(aes(x = stage, y = percent), data = type_1_error) +
  geom_bar(stat = "identity", color = "black", fill = "white") +
  labs(x = "Stage", y = "Type I error rate\n (Percentage of sigificant results)") +
  theme_bw() +
  theme(strip.text.x = element_text(size = 11, colour = "black")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text = element_text(size = 12, colour = "black")) +
  theme(legend.text = element_text(size = 11)) +
  theme(legend.title = element_text(size = 12))

power <-
  final_success %>% 
  filter(d != 0 & H0 == 2)

### plot power for each d and stage
ggplot(aes(x = stage, y = percent), data = power) +
  geom_bar(stat = "identity", color = "black", fill = "white") +
  facet_wrap(~ d) +
  labs(x = "Stage", y = "Statistical power") +
  theme_bw() +
  theme(strip.text.x = element_text(size = 11, colour = "black")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text = element_text(size = 12, colour = "black")) +
  theme(legend.text = element_text(size = 11)) +
  theme(legend.title = element_text(size = 12))


### compute summary of characteristics by true effect size and outcome (success / futility)
summary_d <-
  final %>% 
  filter(H0 != 0) %>% 
  group_by(d, H0) %>% 
  summarize(mean_d_emp = mean(delta_emp),
            med_d_emp = median(delta_emp),
            min_d = min(delta_emp),
            max_d = max(delta_emp),
            mean_N = mean(totalN))


### plot number of animals needed for success or futility by stage
ggplot(aes(x = factor(H0), y = mean_N), data = summary_d) +
  geom_bar(stat = "identity", color = "black", fill = "white") +
  facet_wrap(~ d, ncol = 4, nrow = 1) +
  labs(x = "", y = "# of animals needed (out of 36)") +
  scale_y_continuous(breaks = seq(0, 36, 6)) +
  scale_x_discrete(breaks = c("1", "2"), 
                   labels = c("Futility", "Success")) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 11, colour = "black")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text = element_text(size = 12, colour = "black")) +
  theme(legend.text = element_text(size = 11)) +
  theme(legend.title = element_text(size = 12))

percent_stopped <-
  final %>% 
  group_by(d, H0) %>% 
  filter(H0 != 0) %>% 
  summarize(stopped_early = n(),
            per_stopped = (n()/10000)*100)

### plot percentage of experiments stopped for futility / success
ggplot(aes(x = factor(H0), y = per_stopped), data = percent_stopped) +
  geom_bar(stat = "identity", color = "black", fill = "white") +
  labs(x = " ", y = "Percentage of experiments stopped") +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_x_discrete(breaks = c("1","2"),
                   labels = c("Futility", "Success")) +
  facet_wrap(~ d, ncol = 4, nrow = 1) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 11, colour = "black")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text = element_text(size = 12, colour = "black")) +
  theme(legend.text = element_text(size = 11)) +
  theme(legend.title = element_text(size = 12))


