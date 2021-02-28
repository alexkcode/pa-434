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

migrant_0 <- left_join(migrant, region, by = c("CountryCode" = "countrycode"))

# 1. What is the number of Migrant by region in 2015?

migrant_1 <- 
  migrant_0 %>% 
  filter(year == 2015) %>%
  group_by(region) %>%
  summarise(n = sum(Migrants, na.rm = TRUE))
migrant_1

# region                      n
# <chr>                   <dbl>
# 1 Africa               20648953
# 2 Asia                 72781741
# 3 Developed countries 140393231
# 4 Latam                 9024966
# 5 Oceania                229158
# 6 Territories/Others     622187

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
  migrant_0 %>%
  group_by(region) %>%
  count(region) %>%
  arrange(desc(n))
migrant_3

# Most of the countries are African countries.

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
  distinct(Country, region, refugee_diff) %>%
  group_by(region) %>%
  summarise(refugee_diff_by_region = sum(refugee_diff, na.rm = TRUE)) %>%
  arrange(desc(refugee_diff_by_region))
migrant_5

# region              refugee_diff_by_region
# <chr>                                <dbl>
# 1 Asia                               2140863
# 2 Territories/Others                 1140895
# 3 Oceania                              -2287
# 4 Developed countries                 -67500
# 5 Latam                              -813242
# 6 Africa                            -1905662

# Asia saw the largest net increase of refugees while Africa had the largest
# net decrease of refugees.

migrant_5a <-
  migrant_0 %>%
  left_join(migrant_0 %>% filter(year == 1990) %>% select(Refugees, CountryCode), 
            by = c("CountryCode" = "CountryCode"),
            suffix = c("","_1990")) %>%
  left_join(migrant_0 %>% filter(year == 2015) %>% select(Refugees, CountryCode), 
            by = c("CountryCode" = "CountryCode"),
            suffix = c("","_2015")) %>%
  mutate(refugee_diff = Refugees_2015 - Refugees_1990) %>%
  distinct(Country, region, refugee_diff)

# Many of the differences seem to result in NAs seemingly because data was not
# available or collected for earlier year (or any of the years). 

# 6. Has there been any changes in the immigration patterns by gender over time? 
# Which country has the lowest female immigration? 
# Which region has the highest female immigration? 
# Consider the average across all the years.

migrant_6 <-
  migrant_0 %>%
  left_join(migrant_0 %>% filter(year == 1990) %>% select(Migrants, CountryCode), 
            by = c("CountryCode" = "CountryCode"),
            suffix = c("","_1990")) %>%
  left_join(migrant_0 %>% filter(year == 2015) %>% select(Migrants, CountryCode), 
            by = c("CountryCode" = "CountryCode"),
            suffix = c("","_2015")) %>%
  mutate(migrant_diff = Migrants_2015 - Migrants_1990) %>%
  group_by(region) %>%
  summarise(migrant_diff = mean(migrant_diff, na.rm = TRUE)) %>%
  arrange(desc(migrant_diff))
migrant_6

# Developed countries have seen the largest increase of immigrants arriving from 
# 1990 to 2015, while Territories and other types of countries have seen a very 
# modest increase.

migrant_6a <- 
  migrant_0 %>%
  group_by(year) %>%
  summarise(avg = mean(Migrants, na.rm = TRUE))
migrant_6a  

# Every 5 years since 1990 immigration has increased by about 50k - 100k

migrant_6b <- 
  migrant_0 %>%
  group_by(region) %>%
  summarise(avg = mean(FemaleMigrants, na.rm = TRUE)) %>%
  arrange(avg)
migrant_6b
  
# Oceania has had the lowest average female immigration over the years.

migrant_6c <- 
  migrant_0 %>%
  group_by(Country) %>%
  summarise(avg = mean(FemaleMigrants, na.rm = TRUE)) %>%
  arrange(desc(avg))
migrant_6c

# The United States has had the highest average female immigration over the years.

###########################
#
# Missing Values
#
###########################

# 1. Use the is.na functions to calculate how many missing values in the 
# refugees, population, and migrant columns.

apply(is.na(migrant_0)[, c("Refugees", "Pop", "Migrants")],
      MARGIN = 2,
      FUN = sum)

# Refugees Pop Migrants 
# 366        0       15 
  
# 2. For the column(s) with missing values, conduct some additional analysis - 
# are those random by region, you think of an explanation of why there are 
# these missing values? Would you classify these missing values as MCAR, MAR, or MNAR?

refugees_by_year <- 
  migrant_0 %>% 
  mutate(Refugees_missing = case_when(is.na(Refugees) ~ 1, TRUE ~ 0)) %>% 
  group_by(Country, region, year) %>% 
  summarise(missing = sum(Refugees_missing))

refugees_by_year %>% 
  filter(missing > 0) %>%
  group_by(region) %>% 
  summarise(missing = sum(missing)) %>% 
  arrange(desc(missing))
  
# region              missing
# <chr>                 <dbl>
# 1 Territories/Others      156
# 2 Africa                   54
# 3 Latam                    48
# 4 Asia                     42
# 5 Oceania                  42
# 6 Developed countries      24

# Territories and Africa seem to have the most missing data. The countries in these
# regions may not have the administrative infratstructure to collect such data
# so it's likely that this missing data is at least randomly missing, or MAR. 
# Since the data is either missing for all of the years or none of them, it seems 
# hard to tell if it is more systematic than that.

migrant_0 %>% 
  mutate(Migrants_missing = case_when(is.na(Migrants) ~ 1, TRUE ~ 0)) %>% 
  group_by(Country, region, year) %>% 
  summarise(missing = sum(Migrants_missing)) %>% 
  filter(missing > 0)

# In the case of the migrant counts, the countries that lack data lack it specifically
# in the years leading up to and including 2005, so this could be due to infrastructural
# issues again or domestic issues. 
# In Montenegro's case, it wasn't even an independent country until 2006. Similarly,
# South Sudan was not independent until 2011. So this category is definitively MNAR.

# 3. Write a short wrap-up of what you discovered on missing values. 
# Note any surprising findings and you think missing values might affect any 
# analysis contacted with the dataset.

# Some missing values seem to reflect real world events, like with missing migrant
# counts, while others seem more random, like with refugee data. 
# What surprised me with the missing values was that the Refugee data would be 
# missing all of the data for a country or none at all, instead of some random 
# years being missing. 

