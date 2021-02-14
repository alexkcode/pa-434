## PA 434
## Assignment 3
## Alexis Kwan

# loading data from working directory
library(tidyverse)
migration <- read_csv("MigrationFlows.csv")
origin <- read_csv("Origin.csv")
population <- read_csv("Population.csv")
refugees <- read_csv("Refugees.csv")

## TASK 1

# A tidy dataset containing all common observations (e.g., countries) 
# across all four datasets

common_cols <- c("Country code","year")

# A tidy dataset containing as much information as possible across all four datasets

# I remove country names because of inconsistencies of names across datasets
# which will result in duplicates both in row counts, but also columns
# if country name is not joined on. Additionally, while it is possible to clean
# the country names up, it will require manual steps and several steps to match 
# across datasets, because for example there are grammatical differences
# in names which are nontrivial problems because they are not systematically
# different, whereas country code is consistent across datasets and is usually
# backed by ISO standards.

# check data types
str(migration)
str(origin)
str(population)
str(refugees)

# origin <- select(origin, -c("country_name"))
# origin <- rename(origin, "Country code" = "code") 
# origin <- unite(year, c("century","decade","year"), sep = "")
# origin$year <- as.numeric(origin$year)

population2 <- select(population, -c("Country"))
population2$population <- population$population * 1000

# refugees <- select(refugees, -c("Country"))

migration2 <- migration %>% 
              pivot_longer(cols = -c("Country","Country code"),
                           names_sep = "_",
                           names_to = c("Migrant gender","year"),
                           values_to = "Migrants",
                           names_transform = list(year = as.numeric))

origin2 <- origin %>%
           select(-c("country_name")) %>%
           unite(year, c("century","decade","year"), sep = "") %>%
           pivot_longer(cols = -c("code","year"),
                        names_prefix = "tot_",
                        names_to = c("origin"),
                        values_to = "Origin total") %>%
           mutate(`Country code` = code,
                  year = as.numeric(year)) 

refugees2 <- refugees %>%
             select(-c("Country")) %>%
             pivot_longer(cols = -"Country code",
                          names_to = "year",
                          names_prefix = "REFUGEES_",
                          values_to = "Refugees",
                          names_transform = list(year = as.numeric))

# full_join or left_join?
joined2 <- migration2 %>% 
            left_join(origin2, by = common_cols) %>% 
            left_join(population2, by = common_cols) %>% 
            left_join(refugees2, by = common_cols)

# 1. What is the unit of analysis of the dataset?

# The unit of analysis is countries.

# 2. What are the issues with each dataset? How do you make your data tidy?

# Some of the datasets do not have matching column names like have "country" vs.
# "country_name". There are also some columns, like "year" in origin, that are
# nonstandard and also not a column that gets joined on. The "country" column for 
# every data set contained country names that contained non-alphanumeric 
# characters that may represent characters from a larger ASCII superset or are
# potentially meaningless. 

# 3. What is expected number of rows for each of the two datasets that you 
# need to produce?

# 6 years * 232 countries = 1392 total unique year+country combinations
6 * 232 
# 3 poptypes * 1392
3 * 1392
# 3 migration types * 5 origins * 4176
3 * 5 * 4176
# 62640 unique cominbations for 2nd dataset

# 4. Make sure to report the final size of each new dataset and confirm 
# that it matches with your expectations.

# 18468
# 62640

## TASK 2

# Using the dataset containing the most information, your boss asks you to look 
# at the data and extract a few information:

# 5. What is the average percentage of migrants on the 
# total population of a country in 2015?

mean(joined2$Migrants[joined2$`Migrant gender` == "Tot" & joined2$year == "2015"]
     / joined2$population[joined2$Poptype == "Pop" & joined2$year == "2015"])

# 13.33%

# 6. Did the average percentage of refugees on the total of immigrants increased 
# or decreased from 1990?

Refugees_2015 <- mean(joined2$Refugees[joined2$year == "2015"]
                      / joined2$Migrants[joined2$year == "2015"], na.rm = T)
Refugees_1990 <- mean(joined2$Refugees[joined2$year == "1990"]
                      / joined2$Migrants[joined2$year == "1990"], na.rm = T)
Refugees_2015 - Refugees_1990

# decreased by 0.24%

# 7. What is the highest percentage of immigrants in a 
# country in 2010? What is the smallest?

max(joined2$Migrants[joined2$`Migrant gender` == "Tot" & joined2$year == "2010"]
    / joined2$population[joined2$Poptype == "Pop" & joined2$year == "2010"],  na.rm = T)

# 87.84%

min(joined2$Migrants[joined2$`Migrant gender` == "Tot" & joined2$year == "2010"]
    / joined2$population[joined2$Poptype == "Pop" & joined2$year == "2010"],  na.rm = T)
  
# 0.06% 

# 8. What is the median percentage of immigrants from the 
# different continents/geographical areas in 2015?

totals_only <- joined2 %>%
               filter(`Migrant gender` == "Tot" & Poptype == "Pop" & year == "2015")
totals_only$perc <- totals_only$`Origin total` / totals_only$Migrants
migrant_medians <- totals_only %>%
                   group_by(origin) %>%
                   summarize( median_perc = median(perc) )
migrant_medians

# 1 africa        0.66%
# 2 asia          4.80%
# 3 developed     16.30% 
# 4 latam         0.00%  
# 5 oceania       0.00%