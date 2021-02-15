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

migrant_0 <- 
  pivot_longer(migrant,
               c(tot_africa:tot_latam),
               names_to = "region",
               names_prefix = "tot_",
               values_to = "origin_total",
               names_transform = list(year = as.numeric)) %>%
  left_join(region, by = c("CountryCode" = "countrycode"), suffix = c("_origin",""))

# 1. What is the number of Migrant by region in 2015?

migrant_1 <- 
  migrant_0 %>% 
  filter(year == 2015) %>%
  group_by(region) %>%
  summarise(n = sum(origin_total, na.rm = TRUE))
migrant_1

# region                      n
# <chr>                   <dbl>
# 1 Africa               16250082
# 2 Asia                 62244538
# 3 Developed countries 127072157
# 4 Latam                 6174187
# 5 Oceania                166414
# 6 Territories/Others     470851

# 2. How many countries have a share of immigrants that it’s greater than 
# or equal to 50% of their population in 2015?

migrant_2 <- 
  migrant_0 %>% 
  group_by(Country) %>%
  mutate(migrant_prop = Migrants / Pop) %>% 
  filter(year == 2015 & migrant_prop >= 0.5) %>%
  distinct(Country, CountryCode, migrant_prop)
migrant_2

# There are 16 such countries.
 
# 3. In which world regions are most of these countries located?

migrant_3 <-
  migrant_2 %>%
  group_by(region) %>%
  count(region) %>%
  arrange(desc(n))
migrant_3

# Most of the countries from question 2 are in territorial regions.

# 4. How many countries have a share of immigrants greater than or equal to 
# 50% of their population if we consider the average from 1990 to 2015? 
# Which is the country with the highest average across all year?

migrant_4 <- 
  migrant_0 %>% 
  group_by(Country) %>%
  mutate(migrant_prop = mean(Migrants / Pop)) %>% 
  filter(migrant_prop >= 0.5) %>%
  distinct(Country, CountryCode, migrant_prop)
migrant_4

# There are 13 countries such that average ratio of immigrants to the total
# population is greater or qual to 50%.

migrant_0 %>% 
  group_by(Country) %>%
  mutate(migrant_prop = mean(Migrants / Pop)) %>%
  filter(CountryCode == 535) %>%
  distinct(Country,year,Migrants,Pop,migrant_prop)



# 5. Which region saws the largest increase in the number of refugees from 1990 to 2015? 
# And which region saws the largest decrease? 
# (Note: Pay attention to unexpected results. Do some digging if needed!).

migrant_5 <-
  migrant_0 %>%
  left_join(migrant_0 %>% filter(year == 1990) %>% select(Refugees, CountryCode), 
            by = c("CountryCode" = "CountryCode"),
            suffix = c("","_1990")) %>%
  left_join(migrant_0 %>% filter(year == 2015) %>% select(Refugees, CountryCode), 
            by = c("CountryCode" = "CountryCode"),
            suffix = c("","_2015")) %>%
  mutate(refugee_diff = Refugees_2015 - Refugees_1990) %>%
  group_by(region) %>%
  summarise(refugee_diff_by_region = sum(refugee_diff, na.rm = TRUE)) %>%
  arrange(desc(refugee_diff_by_region))
migrant_5

# region              refugee_diff_by_region
# <chr>                                <dbl>
# 1 Asia                            1605647250
# 2 Territories/Others               855671250
# 3 Oceania                           -1715250
# 4 Developed countries              -50625000
# 5 Latam                           -609931500
# 6 Africa                         -1429246500

# Asia saw the largest net increase of refugees while Africa had the largest
# exodus of refugees.

# 6. Has there been any changes in the immigration patterns by gender over time? 
# Which country has the lowest female immigration? 
# Which region has the highest female immigration? 
# Consider the average across all the years.


###########################
#
# Missing Values
#
###########################

# 1. Use the is.na functions to calculate how many missing values in the 
# refugees, population, and migrant columns.

  
# 2. For the column(s) with missing values, conduct some additional analysis - 
# are those random by region, you think of an explanation of why there are 
# these missing values? Would you classify these missing values as MCAR, MAR, or MNAR?

  
# 3. Write a short wrap-up of what you discovered on missing values. 
# Note any surprising findings and you think missing values might affect any 
# analysis contacted with the dataset.