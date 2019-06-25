setwd("~/Documents/QUEST/PhD/R/Simulation_framework/Simulation_framework")

rm(list = ls())

library(ggthemes)


### load function that generates hypothetical data for two groups >> normal distribution
source("simulate_data_function_normal.R")


### load function that generates hypothetical data for two groups >> lognormal distribution
#source("simulate_data_function_lognormal.R")

###===========================================================================================================
### SETTINGS
###===========================================================================================================

nsubj = 10
dinit = 1.5
nmax  = 1000000
test_sequence <- c(1:nmax)
ds <- seq(dinit, 1.5, by = 0.5)


### create empty matrix to later save statistics at each step
final_res <- matrix(NA, nrow = length(test_sequence),
                    ncol = 8,
                    dimnames = list(NULL, c("n", "d", "mean_control", "mean_treatment", 
                                            "delta_emp", "t_value", "p_value", "power")))

final_res_counter <- 1

### initialize results vectors
res_nsubj          <- c()
res_mean_treatment <- c()
res_mean_control   <- c()
res_t_value        <- c()
res_p_value        <- c()
res_d_emp          <- c()
res_power          <- c()


###===========================================================================================================
### START OF SIMULATION
###===========================================================================================================

for (dIdx in ds) {
  
  for (iteration in test_sequence) {
    
    for (i in 1:10000) {
      
      ### simulate data points
      sim_data    <- simulate_data(nsubj = nsubj, d = dIdx)
  
      ### run t.test
      ttest_model <- t.test(value ~ group, data = sim_data, alternative = "two.sided",
                            var.equal = FALSE)
      
      
      ### calculate t-statistic manually for nsubj = 10
      # sim_data_sum <-
      #   sim_data %>%
      #   group_by(group) %>%
      #   summarize(mean_group = mean(value),
      #             sd_group   = sd(value))
      # 
      # t <- (sim_data_sum[1, 2] - sim_data_sum[2, 2]) / sqrt(sim_data_sum[1, 3]^2/5 + sim_data_sum[2, 3]^2/5)
      
      
      
      ### calculate mean and sd for groups for later use
      sim_data_sum <-
        sim_data %>%
        group_by(group) %>%
        summarize(mean_group = mean(value),
                  sd_group   = sd(value))
      
      
      ### give t-test results variable names for later use
      mean_control   <- round(ttest_model$estimate[1], 3)
      mean_treatment <- round(ttest_model$estimate[2], 3)
      CI_lower       <- round(ttest_model$conf.int[1], 3)
      CI_upper       <- round(ttest_model$conf.int[2], 3)
      t_value        <- round(ttest_model$statistic, 3)
      p_value        <- round(ttest_model$p.value, 3)
      delta_emp      <- as.numeric((sim_data_sum[1, 2] - sim_data_sum[2, 2]) / 
                                    sqrt((sim_data_sum[1, 3]^2 + sim_data_sum[2, 3]^2)/2))
      
      ### compute power >> number of true positives
      sig <- (p_value <= .05)
      
      print(c(i, nsubj, dIdx, sig))
      
      
      res_mean_treatment[i] <- mean_treatment
      res_mean_control[i]   <- mean_control
      res_t_value[i]        <- t_value
      res_p_value[i]        <- p_value
      res_d_emp[i]          <- delta_emp
      res_power[i]          <- sig
      
    }
    
    final_res[final_res_counter, ] <- c(nsubj, dIdx, mean_control = mean(res_mean_control), mean_treatment = mean(res_mean_treatment),  
                                        d_emp = mean(res_d_emp), t_value = mean(res_t_value), p_value = mean(res_p_value), 
                                        power = mean(res_power))
    
    final_res_counter <- final_res_counter + 1 
    
    
    
    if (final_res[final_res_counter - 1, 8] < .8) {
      
      nsubj = nsubj + 2
      
      ### reset results vectors
      res_mean_treatment <- c()
      res_mean_control   <- c()
      res_t_value        <- c()
      res_p_value        <- c()
      res_d_emp          <- c()
      res_power          <- c()
      
      next; 
      
    } else {
      
      print(paste0("Desired power level achieved with ", nsubj, " subjects. Power is ", round(mean(res_power), 3)))
      
      break;
      
    }
    
  }
  
  ### reset sample size for next effect size
  ninit <- 10
  nsubj <- ninit
  
}


###===========================================================================================================
### END OF SIMULATION
###===========================================================================================================


final <- as.data.frame(final_res)

final <- 
  final %>% 
    filter(n != "NA")

#write.csv(final, file = "final_power_0.8")

write.csv(final, file = "test")

