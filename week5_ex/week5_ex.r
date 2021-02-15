###########################
#
# PA 403 Week 5 Assignment
# Alexis Kwan
#
###########################

library(tidyverse)

migrant <- read_csv("ImmigrationData.csv")
region <- read_csv("RegionData.csv")

###########################
#
# Questions
#
###########################

# 1. What is the number of Migrant by region in 2015?
migrant_1 <- pivot_longer(migrant,
                          c(tot_africa:tot_latam),
                          names_to = "region",
                          names_prefix = "tot_", 
                          values_to = "region_total", 
                          names_transform = list(year = as.numeric)
                          )

migrant_1 %>% 
  filter(year == 2015) %>%
  group_by(region) %>%
  summarise(n = sum(region_total, na.rm = TRUE))

# region           n
# <chr>        <dbl>
# 1 africa    29668106
# 2 asia      88063241
# 3 developed 62502662
# 4 latam     32144220
# 5 oceania          0

# 2. How many countries have a share of immigrants that it’s greater than 
# or equal to 50% of their population in 2015?
migrant_2 <- 
  migrant_1 %>% 
  group_by(Country) %>%
  mutate(migrant_prop = Migrants / Pop) %>% 
  filter(year == 2015 & migrant_prop >= 0.5) %>%
  distinct(Country, CountryCode)

# There are 16 such countries.
 
# 3. In which world regions are most of these countries located?
%>%
  left_join(region, by = c("CountryCode" = "countrycode"))

# 4. How many countries have a share of immigrants greater than or equal to 
# 50% of their population if we consider the average from 1990 to 2015? 
# Which is the country with the highest average across all year?


# 5. Which region saws the largest increase in the number of refugees from 1990 to 2015? 
# And which region saws the largest decrease? 
# (Note: Pay attention to unexpected results. Do some digging if needed!).


# 6. Has there been any changes in the immigration patterns by gender over time? 
# Which country has the lowest female immigration? 
# Which region has the highest female immigration? 
# Consider the average across all the years.


###########################
#
# Missing Values
#
###########################