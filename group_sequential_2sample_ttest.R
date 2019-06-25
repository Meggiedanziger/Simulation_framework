setwd("~/Documents/QUEST/PhD/R/Simulation_framework/Simulation_framework")


rm(list = ls())

library(gsDesign)

### load function that generates hypothetical data for two groups
source("simulate_data_function_normal.R")

### load function that generates hypothetical data for two groups >> lognormal distribution
#source("simulate_data_function_lognormal.R")

### load function to sample data for each stage
source("sample_data_for_stages.R")


### determine bounds and critical values of the sequential design
test_design <- gsDesign::gsDesign(k = 3, test.type = 3, alpha = 0.025, beta = .2,
                                  delta = .5, n.fix = 36, timing = 1, 
                                  sfu = sfHSD, sfupar = 0, sfl = sfHSD, sflpar = -2)

test_design
plot(test_design)
plot(test_design, plottype = 2)


nsubj = 36
dinit = 0
beta  = .2
alpha = .05

ds <- seq(dinit, 1.5, by = .5)

final_res <- matrix(NA, nrow = 100000000,
                    ncol = 10,
                    dimnames = list(NULL, c("nsubj","d", "beta", "totalN", "delta_emp", "t_value", "p_value", 
                                            "df", "stage", "H0")))

final_res_counter <- 1

start <- print(Sys.time())

###===========================================================================================================
### START OF SIMULATION
###===========================================================================================================


for (d in ds) {
  
  for (i in 1:10000)
    write.csv(file = paste("file_", i, "_", d, ".csv", sep = " "), simulate_data(nsubj = nsubj, d = d))
  

  for (t in 1:10000) {
    stage_1_data <- data.frame(value = sample_data_1(t), group = c(rep("control", 6), rep("treatment", 6)))
    
    sim_data_sum <-
      stage_1_data %>%
      group_by(group) %>%
      summarize(mean_group = mean(value),
                sd_group   = sd(value))
    
    
    stage_1_test   <- t.test(value ~ group, data = stage_1_data, alternative = "two.sided")
    sig1           <- stage_1_test$p.value <= test_design$upper$spend[1] 
    sig1_lower     <- stage_1_test$p.value >= test_design$lower$prob[1, 1]    
    totalN         <- nrow(stage_1_data)
    stage          <- 1
    delta_emp      <- as.numeric((sim_data_sum[1, 2] - sim_data_sum[2, 2]) / 
                                   sqrt((sim_data_sum[1, 3]^2 + sim_data_sum[2, 3]^2)/2))
    
    
    print(stage_1_test$statistic)
    
    if (sig1 == TRUE) {
      print(paste("stage 1 trial success"))
      
      final_res[final_res_counter, ] <- c(nsubj, d, beta, totalN, delta_emp = delta_emp, 
                                          t_value = stage_1_test$statistic, p_value = stage_1_test$p.value, 
                                          df = stage_1_test$parameter, stage, H0 = 2)
      
      final_res_counter <- final_res_counter + 1 
      
      next;
      
    } else if (sig1_lower == TRUE) {
        print(paste("stage 1 trial stopped for futility"))

        final_res[final_res_counter, ] <- c(nsubj, d, beta, totalN, delta_emp = delta_emp,
                                            t_value = stage_1_test$statistic, p_value = stage_1_test$p.value,
                                            df = stage_1_test$parameter, stage, H0 = 1)

        final_res_counter <- final_res_counter + 1

        next;
      
    } else {
      
      final_res[final_res_counter, ] <- c(nsubj, d, beta, totalN, delta_emp = delta_emp, 
                                          t_value = stage_1_test$statistic, p_value = stage_1_test$p.value, 
                                          df = stage_1_test$parameter, stage, H0 = 0)
      
      final_res_counter <- final_res_counter + 1 
      
      print(paste("continue to stage 2"))
      
      stage_2_data <- data.frame(value = sample_data_2(t), group = c(rep("control", 6), rep("treatment", 6)))
      
      stage_2_data <- rbind(stage_1_data, stage_2_data)
      
      sim_data_sum <-
        stage_2_data %>%
        group_by(group) %>%
        summarize(mean_group = mean(value),
                  sd_group   = sd(value))
      
      
      stage_2_test   <- t.test(value ~ group, data = stage_2_data, alternative = "two.sided")
      sig2           <- stage_2_test$p.value <= test_design$upper$spend[2]
      sig2_lower     <- stage_2_test$p.value >= test_design$lower$prob[2, 1]    
      totalN         <- nrow(stage_2_data)
      stage          <- 2
      delta_emp      <- as.numeric((sim_data_sum[1, 2] - sim_data_sum[2, 2]) / 
                                     sqrt((sim_data_sum[1, 3]^2 + sim_data_sum[2, 3]^2)/2))
      
      
      print(stage_2_test$statistic)
      
      if (sig2 == TRUE) {
        print(paste("stage 2 trial success"))
        
        final_res[final_res_counter, ] <- c(nsubj, d, beta, totalN, delta_emp = delta_emp, 
                                            t_value = stage_2_test$statistic, p_value = stage_2_test$p.value, 
                                            df = stage_2_test$parameter, stage, H0 = 2)
        
        final_res_counter <- final_res_counter + 1 
        
        next;
        
      } else if (sig2_lower == TRUE) {
        print(paste("stage 2 trial stopped for futility"))

        final_res[final_res_counter, ] <- c(nsubj, d, beta, totalN, delta_emp = delta_emp,
                                            t_value = stage_2_test$statistic, p_value = stage_2_test$p.value,
                                            df = stage_2_test$parameter, stage, H0 = 1)

        final_res_counter <- final_res_counter + 1

        next;
        
      } else {
        
        final_res[final_res_counter, ] <- c(nsubj, d, beta, totalN, delta_emp = delta_emp, 
                                            t_value = stage_2_test$statistic, p_value = stage_2_test$p.value, 
                                            df = stage_2_test$parameter, stage, H0 = 0)
        
        final_res_counter <- final_res_counter + 1 
        
        print(paste("continue to stage 3"))
        
        stage_3_data <- data.frame(value = sample_data_3(t), group = c(rep("control", 6), rep("treatment", 6)))
        
        stage_3_data <- rbind(stage_2_data, stage_3_data)
        
        sim_data_sum <-
          stage_3_data %>%
          group_by(group) %>%
          summarize(mean_group = mean(value),
                    sd_group   = sd(value))
        

        stage_3_test   <- t.test(value ~ group, data = stage_3_data, alternative = "two.sided")
        sig3           <- stage_3_test$p.value <= test_design$upper$spend[3]
        sig3_lower     <- stage_3_test$p.value >= test_design$lower$prob[3, 1]    
        totalN         <- nrow(stage_3_data)
        stage          <- 3
        delta_emp      <- as.numeric((sim_data_sum[1, 2] - sim_data_sum[2, 2]) / 
                                       sqrt((sim_data_sum[1, 3]^2 + sim_data_sum[2, 3]^2)/2))
        
        
        print(stage_3_test$statistic)
        
        if (sig3 == TRUE) {
          print(paste("stage 3 trial success"))
          
          final_res[final_res_counter, ] <- c(nsubj, d, beta, totalN, delta_emp = delta_emp, 
                                              t_value = stage_3_test$statistic, p_value = stage_3_test$p.value, 
                                              df = stage_3_test$parameter, stage, H0 = 2)
          
          final_res_counter <- final_res_counter + 1 
          
          next;
          
        } else if (sig3_lower == TRUE) {
          print(paste("stage 3 trial stopped for futility"))

          final_res[final_res_counter, ] <- c(nsubj, d, beta, totalN, delta_emp = delta_emp,
                                              t_value = stage_3_test$statistic, p_value = stage_3_test$p.value,
                                              df = stage_3_test$parameter, stage, H0 = 1)

          final_res_counter <- final_res_counter + 1

          next;
          
        } else { 
          
          print(paste("trial terminated without success"))
          #stage <- 0
          
          final_res[final_res_counter, ] <- c(nsubj, d, beta, totalN, delta_emp = delta_emp, 
                                              t_value = stage_3_test$statistic, p_value = stage_3_test$p.value, 
                                              df = stage_3_test$parameter, stage, H0 = 0)
          
          final_res_counter <- final_res_counter + 1 
          
          
          # stage_4_data <- data.frame(value = sample_data_4(t), group = c(rep("control", 6), rep("treatment", 7)))
          # 
          # stage_4_data <- rbind(stage_3_data, stage_4_data)
          # 
          # sim_data_sum <-
          #   stage_4_data %>%
          #   group_by(group) %>%
          #   summarize(mean_group = mean(value),
          #             sd_group   = sd(value))
          # 
          # 
          # stage_4_test   <- t.test(value ~ group, data = stage_4_data, alternative = "two.sided")
          # sig4           <- stage_4_test$p.value <= .05
          # sig4_lower     <- stage_4_test$p.value >= .95
          # totalN         <- nrow(stage_4_data)
          # stage          <- 4
          # delta_emp      <- as.numeric((sim_data_sum[1, 2] - sim_data_sum[2, 2]) / 
          #                                sqrt((sim_data_sum[1, 3]^2 + sim_data_sum[2, 3]^2)/2))
          # 
          # 
          # print(stage_4_test$statistic)
          
          # if (sig4 == TRUE) {
          #   print(paste("stage 4 trial success"))
          #   
          #   final_res[final_res_counter, ] <- c(nsubj, d, beta, totalN, delta_emp = delta_emp, 
          #                                       t_value = stage_4_test$statistic, p_value = stage_4_test$p.value, 
          #                                       df = stage_4_test$parameter, stage, H0 = 2)
          #   
          #   final_res_counter <- final_res_counter + 1 
          #   
          #   
          # } else {
            
          #   print(paste("trial terminated without success"))
          #   stage <- 0
          #   
          #   final_res[final_res_counter, ] <- c(nsubj, d, beta, totalN, delta_emp = delta_emp, 
          #                                       t_value = stage_4_test$statistic, p_value = stage_4_test$p.value, 
          #                                       df = stage_4_test$parameter, stage, H0 = sig4_lower)
          #   
          #   final_res_counter <- final_res_counter + 1 
          #   
          #   
          # }
          
        }
        
      }
      
    }
    
  }
  
  end <- print(Sys.time())
  
  files <- dir()
  file.remove(files[grep("file*", files)])

}


###===========================================================================================================
### END OF SIMULATION
###===========================================================================================================


diff <- end - start
diff

# files <- dir()
# file.remove(files[grep("file*", files)])


final <- as.data.frame(final_res)

final <- 
  final %>% 
  filter(nsubj != "NA")

#write.csv(final, file = "test_d0")


write.csv(final, file = "final_3stages_power_0.8_with_futility_bounds_Pocock")




