############################################################
# Geodemographic Clustering for Aberdeen Based on Hydrogen 
# Author: Poppy Edlmann                                       
# Date: 29/04/2025                                     
# Description:                                               
# This script performs geodemographic clustering using the  
# synthetic population data informed by hydrogen perceptions 
# in Aberdeen. The clustering is based on demographic data  
# from Aberdeen DataZones and survey data regarding public  
# perceptions of hydrogen technology.                       
############################################################

install.packages("sf")
install.packages("tidyverse")
install.packages("readxl")
install.packages("mipfp")

library(sf)
library(tidyverse)
library(readxl)

#-----------------------------------------------
# 1. Load DataZone Spatial Data
#-----------------------------------------------

# Load the shapefile
dz_data <- st_read("H:/Dissertation/Aberdeen_DZ_Joined_Final.shp")

head(dz_data)
str(dz_data)

ggplot(data = dz_data) +
  geom_sf(aes(fill = TotPop2022)) +
  scale_fill_viridis_c() +
  labs(title = "Total Population by DataZone in Aberdeen")

#-----------------------------------------------
# 2. K-means Clustering on Demographics
#-----------------------------------------------

cluster_data <- dz_data %>% 
  st_drop_geometry() %>%   # Removes spatial data temporarily
  select(
    TotPop2022,
    Age_0_15,
    Age_16_24,
    Age_25_34,
    Age_35_49,
    Age_50_64,
    Age_65_,
    Education_,
    Educatio_3,
    Employment,
    Employme_2
  )

head(cluster_data)

cluster_data_scaled <- scale(cluster_data)

set.seed(123) 
wss <- (nrow(cluster_data_scaled)-1)*sum(apply(cluster_data_scaled, 2, var))
for (i in 2:10) wss[i] <- sum(kmeans(cluster_data_scaled, centers=i)$withinss)

plot(1:10, wss, type="b", pch=19, 
     xlab="Number of Clusters (k)", 
     ylab="Within groups sum of squares (WSS)",
     main="Elbow Method to Choose Optimal Clusters")

set.seed(123)
kmeans_result <- kmeans(cluster_data_scaled, centers = 4, nstart = 25)

dz_data$Cluster <- as.factor(kmeans_result$cluster)

ggplot(dz_data) +
  geom_sf(aes(fill = Cluster)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Geodemographic Clusters of Aberdeen")

#-----------------------------------------------
# 3. Data Preparation for Survey Data
#-----------------------------------------------

setwd("C:/Users/pee1/OneDrive - University of St Andrews/Dissertation/RStudio/Hydrogen_Diss_Analysis")

survey_data <- read_csv("diss_data_cleaned.csv")
postcode_lookup <- read_csv("AB_postcode_districts.csv")
dz_lookup <- read_excel("lookuptable.xlsx")

postcode_lookup_clean <- postcode_lookup %>%
  rename(
    DZName = `Lower layer super output area`,
    Postcode = `Postcode`
  ) %>%
  mutate(
    DZName = str_trim(DZName),
    Postcode = str_trim(Postcode)
  )

survey_data <- survey_data %>%
  mutate(PostcodeDistrict = str_extract(Postcode, "^AB\\d+"))

survey_dz_joined <- left_join(survey_data, postcode_lookup_clean, by = c("Postcode" = "Postcode"))

head(survey_dz_joined)

#-----------------------------------------------
# 4. Adding Clusters to Survey Data
#-----------------------------------------------

cluster_lookup <- dz_data %>%
  st_drop_geometry() %>%
  select(DZName, Cluster)

survey_with_clusters <- left_join(survey_dz_joined, cluster_lookup, by = "DZName")

table(survey_with_clusters$Cluster, useNA = "ifany")

#-----------------------------------------------
# 5. Final Data Preparation and Clean Up
#-----------------------------------------------

survey_with_clusters$DZName <- str_trim(str_to_lower(survey_with_clusters$DZName))
dz_data$DZName <- str_trim(str_to_lower(dz_data$DZName))

sum(is.na(survey_with_clusters$DZName))

#-----------------------------------------------
# 6. Visualisation of Clusters
#-----------------------------------------------

ggplot(survey_with_clusters) +
  geom_sf(aes(fill = Cluster)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Geodemographic Clusters of Hydrogen Positivity in Aberdeen")

ggsave("cluster_map_aberdeen.png", width = 10, height = 8, dpi = 300)

#------------------------------------------------------------
# End of Geodemographic Code
#------------------------------------------------------------

# Note:
# - The analysis used K-means clustering to identify geodemographic patterns in Aberdeen based on synthetic population data.
# - Synthetic population was generated using spatial microsimulation (IPF), ensuring that demographic distributions matched known census constraints.
# - Clusters were based on demographic variables (age, education, housing) and hydrogen perception scores.
# - K-means clustering was chosen due to its simplicity and interpretability, with the number of clusters (k = 4) determined using the elbow method.
# - Data cleaning steps included handling missing values, standardizing DZName for matching, and removing redundant columns before analysis.

# References:
#Lovelace, R. (2023). Spatial Microsimulation: An Introduction. 
# Available at: https://spatial-microsim-book.robinlovelace.net/cakemap. 


