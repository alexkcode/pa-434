###########################
#
# PA 403 Week 5 Assignment
# Alexis Kwan
#
###########################

library(tidyverse)

migrant <- read_csv("ImmigrationData.csv")
region <- read_csv("RegionData.csv")

# 1. What is the number of Migrant by region in 2015?
migrant_1 <- pivot_longer(migrant,
                          c(tot_africa:tot_latam),
                          names_to = "region",
                          names_prefix = "tot_", 
                          values_to = "region_total", 
                          names_transform = list(year = as.numeric))

migrant_1 %>% 
  filter(year == 2015) %>%
  group_by(region) %>%
  summarise( n = sum(region_total, na.rm = TRUE) )

# 2. How many countries have a share of immigrants that it’s greater than 
# or equal to 50% of their population in 2015?

 
# 3. In which world regions are most of these countries located?


# 4. How many countries have a share of immigrants greater than or equal to 
# 50% of their population if we consider the average from 1990 to 2015? 
# Which is the country with the highest average across all year?