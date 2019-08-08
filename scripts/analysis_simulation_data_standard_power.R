setwd("~/Documents/QUEST/PhD/R/Simulation_framework/Simulation_framework")


rm(list = ls())

library(tidyverse)
library(readr)
library(compute.es)

final <- read.csv(file = "final_power_0.8")

### compute summary of characteristics by true effect size
summary_delta <-
  final %>% 
  group_by(d) %>% 
  summarize(mean_d_emp = mean(delta_emp),
            med_d_emp = median(delta_emp),
            min_d = min(delta_emp),
            max_d = max(delta_emp))

power_success <-
  final %>% 
  filter(power > .8)

### compute CI around effect size
power_success$CI_lower <- tes(t = power_success$t_value, n.1 = power_success$n/2, n.2 = power_success$n/2)$l.d
power_success$CI_upper <- tes(t = power_success$t_value, n.1 = power_success$n/2, n.2 = power_success$n/2)$u.d  

plot_data <-
  power_success %>% 
  group_by(d) %>% 
  summarize(mean_d_emp = mean(delta_emp),
            mean_CI_lower = mean(CI_lower),
            mean_CI_upper = mean(CI_upper))


### plot mean effect size estimate and CI for each true effect size
ggplot(aes(x = d, y = delta_emp), data = power_success) +
  geom_errorbar(aes(ymin = delta_emp - CI_lower, 
                    ymax = delta_emp + CI_upper), width = 0.03) +
  geom_point(color = "red", size = 1.9, alpha = .7) +
  labs(x = "True effect size", y = "Mean effect size (with CI)") +
  scale_y_continuous(breaks = seq(-1, 5, 1)) +
  scale_x_continuous(breaks = c(0.5, 1, 1.5)) +
  theme_bw() +
  # theme(strip.text.x = element_text(size = 12, colour = "black")) +
  # theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12, colour = "black"))






ggplot(aes(x = n, y = power), data = final) +
  geom_point(alpha = 0.7, size = 1.9) +
  facet_wrap(~ d) +
  geom_hline(yintercept = 0.5, lty = "dashed", color = "tomato") +
  geom_hline(yintercept = 0.8, color = "tomato") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 11, colour = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text = element_text(size = 12, colour = "black")) +
  theme(legend.text = element_text(size = 11)) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.position = "none")
