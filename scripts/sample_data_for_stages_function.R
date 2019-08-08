


sample_data_1 <- function (k) {
  
  test_data <- read.csv(paste("file_", k, "_", d, ".csv", sep = " "))
  
  control <-
    test_data %>%
    group_by(group) %>%
    filter(group == "control")
  
  treatment <-
    test_data %>%
    group_by(group) %>%
    filter(group == "treatment")
  
  
  set.seed(123)
  
  stage_1_control <- sample(control$value, 6, replace = FALSE)
  
  stage_1_treatment <- sample(treatment$value, 6, replace = FALSE)
  
  return(c(stage_1_control, stage_1_treatment))
  
}


sample_data_2 <- function (k) {
  
  test_data <- read.csv(paste("file_", k, "_", d, ".csv", sep = " "))
  
  control <-
    test_data %>%
    group_by(group) %>%
    filter(group == "control")
  
  treatment <-
    test_data %>%
    group_by(group) %>%
    filter(group == "treatment")
  
  
  set.seed(456)
  
  stage_2_control <- sample(control$value, 6, replace = FALSE)
  
  stage_2_treatment <- sample(treatment$value, 6, replace = FALSE)
  
  return(c(stage_2_control, stage_2_treatment))
  
}


sample_data_3 <- function (k) {
  
  test_data <- read.csv(paste("file_", k, "_", d, ".csv", sep = " "))
  
  control <-
    test_data %>%
    group_by(group) %>%
    filter(group == "control")
  
  treatment <-
    test_data %>%
    group_by(group) %>%
    filter(group == "treatment")
  
  
  set.seed(789)
  
  stage_3_control <- sample(control$value, 6, replace = FALSE)
  
  stage_3_treatment <- sample(treatment$value, 6, replace = FALSE)
  
  return(c(stage_3_control, stage_3_treatment))
  
}


# sample_data_4 <- function (k) {
#   
#   test_data <- read.csv(paste("file_", k, "_", d, ".csv", sep = " "))
#   
#   control <-
#     test_data %>%
#     group_by(group) %>%
#     filter(group == "control")
#   
#   treatment <-
#     test_data %>%
#     group_by(group) %>%
#     filter(group == "treatment")
#   
#   
#   set.seed(135)
#   
#   stage_4_control <- sample(control$value, 6, replace = FALSE)
#   
#   stage_4_treatment <- sample(treatment$value, 7, replace = FALSE)
#   
#   return(c(stage_4_control, stage_4_treatment))
#   
# }
