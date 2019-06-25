### simulate_data_function.R
###contains function simulate_data(nsubj = NULL, d = NULL, std_dev = 1)

### This fuction simulates data coming from a normal distribution.
### The data is coming from two hypothetical groups - control and treatment group - that both
### come from a normal distribution. The control group has a mean of 0, the treatment group has 
### a mean of -d, i.e. the mean of this group is shifted. This shift represents the standardized effect.
### The magnitude of the standardized effect is varied in this simulation.

###===========================================================================================================

### load required packages
library(MASS)
library(tidyverse)
library(truncnorm)

### function
simulate_data <- function(nsubj = 10, d = 0.5, std_dev = 1) {
    
  sim_dat     <- data.frame(subj = 1:nsubj,
                            group = rep(c("control", "treatment"), nsubj/2))
    
    ## generate data points row by row
    N     <- dim(sim_dat)[1]
    value <- rep(NA, N)
    
    for(i in 1:N) {
      if(sim_dat$group[i] == "control") {
        value[i] <- rlnorm(1, mean = 0.00001, sd = 1) # for control group generate random value from this distrubution
      } else {
        value[i] <- rlnorm(1, mean = 0, sd = 1) - d # for treatment group generate random value from this distrubution
      }
    }
    
    sim_dat$value <- value
    sim_dat
  }
  