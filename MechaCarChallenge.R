# Import libraries
library(tidyverse)
library(dplyr)
# importing csv files
mechacar_mpg <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
suspension_coil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

# MPG REGRESSION
# Create a multiple linear regression model
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD , data = mechacar_mpg)

# Create summary statistics
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD , data = mechacar_mpg))

# Create summary statistics
suspension_coil %>% 
  summarize(PSI_mean = mean(PSI), 
            PSI_median = median(PSI), 
            PSI_variance = var(PSI),
            PSI_sd = sd(PSI))
# Create summary statistics based on Manufacturing Lots
suspension_coil %>%
  group_by(Manufacturing_Lot) %>% 
  summarize(PSI_mean = mean(PSI), 
            PSI_median = median(PSI), 
            PSI_variance = var(PSI),
            PSI_sd = sd(PSI))

# SUSPENSION COIL T-TEST
# Generate random sample data points
sample1 <- suspension_coil %>% sample_n(50) 
sample2 <- suspension_coil %>% sample_n(50)

# Compare the means of two samples
t.test(sample1$PSI, sample2$PSI)

#Determine if the PSI across all manufacturing lots is statistically different from the population mean of 1,500 pounds per square inch.
t.test(suspension_coil$PSI, mu = 1500)

t.test(subset(suspension_coil, Manufacturing_Lot == "Lot1")$PSI, mu = 1500)
t.test(subset(suspension_coil, Manufacturing_Lot == "Lot2")$PSI, mu = 1500)
t.test(subset(suspension_coil, Manufacturing_Lot == "Lot3")$PSI, mu = 1500)

