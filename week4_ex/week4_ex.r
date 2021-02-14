
library(tidyverse)

## Merge

immigration <- read_csv("ImmigrationData.csv")
region <- read_csv("RegionData.csv")

merged <- immigration %>% left_join(region, by = c("CountryCode" = "countrycode"))

# the number of rows should be the same before and after the merge
nrow(merged) == nrow(immigration) # TRUE

# the following should also be true
n_distinct(immigration$CountryCode) == n_distinct(region$countrycode) # TRUE
n_distinct(merged$CountryCode) == n_distinct(immigration$CountryCode) # TRUE

## Analysis

# 1. Filter all observations from countries in Africa collected in 1990. 
# How many are there? (you can just look at the number of rows)

merged %>%
  select(region, year) %>%
  filter(region == "Africa" & year == 1990) %>%
  count(region)

# There are 57 observations in the year 1990.

# 2. Which countries have a number of female migrants between 1 and 2 million excluded?

merged %>%
  select(Country, FemaleMigrants) %>%
  filter(FemaleMigrants > 1000000, FemaleMigrants < 2000000) %>%
  arrange(FemaleMigrants) %>%
  distinct(Country)

# There are 21 such countries including Israel, Netherlands, Lebanon, Thailand, etc.
# across different years.

# 3. Filter countries located in Africa or Oceania. 
# How many are there? (you can just look at the number of rows)

merged %>%
  select(Country, region) %>%
  filter(region == "Africa" | region == "Oceania") %>%
  distinct(Country)

# There are 67 countries located either in Africa or Oceania regions.

# 4. Which country has the highest number of migrants among “developed” countries in 2010? 
# In answering these questions, keep only relevant columns in the output

merged %>%
  filter(region == "Developed countries" & year == 2010) %>%
  select(Country, Migrants, region, year) %>%
  arrange(desc(Migrants)) %>%
  filter(row_number()==1)

# United States had the most migrants amongst "developed" countries in the year 
# 2010 with 44183643 migrants.

# 5. Reorganize the dataset so that all columns containing information 
# about the immigrant population come right after the year column. 
# All columns should be kept in the dataset.

reorganized <-
  merged %>%
    select(-contains(c("Migrants", "year")), 
           "year", 
           contains("Migrants"))
reorganized

# Should be true and it is
ncol(reorganized) == ncol(merged)

# 6. Filter observations from the following countries: 
# Italy, Germany, Spain, France, Portugal, and Greece in 2015. 
# Use %in% to write your filter function.

merged %>% 
  filter(Country %in% c("Italy", "Germany", "Spain", "France", "Portugal", "Greece") &
           year == 2015)
