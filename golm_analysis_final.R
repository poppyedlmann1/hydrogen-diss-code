############################################
# GOLM Analysis for Hydrogen Dissertation  
# Author: Poppy Edlmann                   
# Date: 29/04/2025                         
# Description: Generalised Ordered Logit   
# Models for spatial acceptance analysis   
############################################

library(readr)      # For reading CSV
library(tidyverse)  # For data wrangling
library(VGAM)       # For Generalised Ordered Logit Model (GOLM)
library(MASS)       # For alternative modelling (POLR)

setwd("C:/Users/pee1/OneDrive - University of St Andrews/Dissertation/RStudio/Hydrogen_Diss_Analysis")
data <- read_csv("diss_data.csv")
glimpse(data)

data$distance_acceptance_ord <- as.ordered(data$distance_acceptance_ord)

#--------------------------------------------
# Data Cleaning for GOLM Analysis
#--------------------------------------------

#  Select key variables for initial models #
model_data <- dplyr::select(data, distance_acceptance_ord, confidence_understanding_ord, 
                            aware_projects_bin, used_h2_bus_bin, air_quality_ord, 
                            aligns_with_future_ord)
model_data <- na.omit(model_data)  

model_data_filtered <- model_data %>%
  filter(distance_acceptance_ord != 0)

model_data_filtered$distance_acceptance_ord <- as.ordered(model_data_filtered$distance_acceptance_ord)

#--------------------------------------------
# RQ1: Exposure to Hydrogen Infrastructure
#--------------------------------------------

# Model: Does using hydrogen buses and awareness of projects predict acceptance?
model_rq1 <- vglm(distance_acceptance_ord ~ used_h2_bus_bin + aware_projects_bin,
                  family = cumulative(parallel = FALSE),
                  data = model_data_filtered)
summary(model_rq1)

#--------------------------------------------
# RQ2: Communication and Understanding
#--------------------------------------------

model_rq2_data <- dplyr::select(data, distance_acceptance_ord, confidence_understanding_ord, communication_effectiveness_ord)
model_rq2_data <- na.omit(model_rq2_data)
model_rq2_data <- model_rq2_data %>%
  filter(distance_acceptance_ord != 0)

model_rq2_data$distance_acceptance_ord <- as.ordered(model_rq2_data$distance_acceptance_ord)

# Model: Does confidence in understanding and communication effectiveness predict acceptance?
model_rq2 <- vglm(distance_acceptance_ord ~ confidence_understanding_ord + communication_effectiveness_ord,
                  family = cumulative(parallel = FALSE),
                  data = model_rq2_data)
summary(model_rq2)

#--------------------------------------------
# RQ3: Place Identity and Acceptance
#--------------------------------------------

model_rq3_data <- dplyr::select(data, distance_acceptance_ord, impact_landscape_ord, aligns_with_future_ord)
model_rq3_data <- na.omit(model_rq3_data)
model_rq3_data <- model_rq3_data %>%
  filter(distance_acceptance_ord != 0)

model_rq3_data$distance_acceptance_ord <- as.ordered(model_rq3_data$distance_acceptance_ord)

# Model: Do perceived landscape impacts and alignment with future vision predict acceptance?
model_rq3 <- vglm(distance_acceptance_ord ~ impact_landscape_ord + aligns_with_future_ord,
                  family = cumulative(parallel = FALSE),
                  data = model_rq3_data)
summary(model_rq3)


#--------------------------------------------
# Optional Simplified POLR Model for RQ3
#--------------------------------------------

# # Simplify predictors into three categories: agree / neutral / disagree
model_rq3_data <- model_rq3_data %>%
  mutate(landscape_simple = case_when(
    impact_landscape_ord <= 2 ~ "disagree",
    impact_landscape_ord == 3 ~ "neutral",
    impact_landscape_ord >= 4 ~ "agree"
  ),
  aligns_simple = case_when(
    aligns_with_future_ord <= 2 ~ "disagree",
    aligns_with_future_ord == 3 ~ "neutral",
    aligns_with_future_ord >= 4 ~ "agree"
  ))

model_rq3_data$landscape_simple <- factor(model_rq3_data$landscape_simple)
model_rq3_data$aligns_simple <- factor(model_rq3_data$aligns_simple)

# Alternative modelling using proportional odds logistic regression (POLR)
model_rq3_polr <- polr(distance_acceptance_ord ~ landscape_simple + aligns_simple,
                       data = model_rq3_data,
                       method = "logistic")
summary(model_rq3_polr)


#--------------------------------------------
# End of GOLM Analysis Code
#--------------------------------------------

# Note:
# - Models assume no proportional odds constraint (parallel = FALSE) following Williams (2006).
# - Data cleaning steps (omitting missing values, filtering invalid categories) were necessary due to small sample size.
# - Interpret coefficients as log-odds unless transformed.

# References:
# - Williams, R. (2006). Generalized Ordered Logit/Partial Proportional Odds Models for Ordinal Dependent Variables. The Stata Journal, 6(1), 58–82.
# - Yee, T. W. (2015). Vector Generalized Linear and Additive Models: With an Implementation in R. Springer.
# - Wickham, H., & Grolemund, G. (2016). R for Data Science: Import, Tidy, Transform, Visualize, and Model Data. O’Reilly Media.
# - UCLA Statistical Consulting Group. Ordinal Regression using R. https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/
# - Wüstenhagen, R., Wolsink, M., & Bürer, M. J. (2007). Social acceptance of renewable energy innovation: An introduction to the concept. Energy Policy, 35(5), 2683–2691.

############################################
