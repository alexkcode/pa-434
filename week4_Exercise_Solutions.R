library(tidyverse)

# DATA PREPARATION FOR THE ASSIGNMENT
 
migrationflows = read_csv("MigrationFlows.csv")

population = read_csv("Population.csv")

refugees = read_csv("Refugees.csv")

origin = read_csv("Origin.csv")

## Solutions

# Make the columns for different years into one column for year
refugees2 =
  pivot_longer(refugees, 
               c(REFUGEES_1990:REFUGEES_2015), 
               names_to = "year", 
               names_prefix = "REFUGEES_", 
               values_to = "refugees", 
               names_transform = list(year = as.numeric))
refugees
refugees2 # check your work
refugees = refugees2

# Population contained 3 different types of data (total, male, and female population)
# create columns for total, male, female populations
# takes numbers from population column and categories from poptype column

population2 =
  pivot_wider(population, names_from = Poptype, values_from = population)
population2
population = population2

population
population$Pop <- population$Pop*1000
population$MalePop <- population$MalePop*1000
population$FemalePop <- population$FemalePop*1000
population

# multiple columns for years for both total population and male / female population,year should be one column
# want to make it longer, type and year should be a column

#----
migrationflows2 = 
pivot_longer(migrationflows, c(Tot_1990:Female_2015), 
             names_to = c("type", "year"), 
             values_to = "migration_flows", 
             names_sep = "_", 
             names_transform = list(year = as.numeric))
migrationflows2 # check it

# then want to separate the types of population back into columns
# which makes it wider
migrationflows3 =
  pivot_wider(migrationflows2, 
              names_from = type, 
              values_from = migration_flows)
migrationflows3

migrationflows = migrationflows3

#----

# information regarding the year is strange. Combine into one category for normal year
origin2 = unite(origin, "Year", c("century", "decade", "year"), sep = "")
origin2 # check it
origin = origin2


###  JOIN THE DATASET TOGETHER

nrow(population) # checks number of rows
nrow(migrationflows)
#1392 rows for both

# have overlapping data for year, country code and country. Join by those cateogries
# matches the year, country, and country code and then adds on the migration flow population  numbers
data = left_join(population, migrationflows, by = c("year" = "year", "Country code" = "Country code", "Country" = "Country"))

nrow(data) #1392
nrow(origin) #1392

origin$Year = as.numeric(origin$Year) #make year numeric type

# also share country, country code, and year data. 
# Match on those and then add on the country origin data
data = left_join(data, origin, by = c("year" = "Year", "Country code" = "code", "Country" = "country_name"))

# Refugees has fewer rows so we create two datsets one where all information is preserved and one where only common observations are kept

nrow(refugees) #1026 rows

###  Dataset with all observations (countries) across the 4 datasets
# matches year, country and country code and then adds on the number of refugees 
data = left_join(data, refugees, by = c("year" = "year", "Country code" = "Country code", "Country" = "Country"))
nrow(data) # 1392 observations from all 4 datasets

# Dataset with only common  observations
# leaves out data that doesn't match the year, country, or country code between the 4 datasets
data3 = inner_join(data, refugees, by = c("year" = "year", "Country code" = "Country code", "Country" = "Country"))
data3 #check it
nrow(data3) # 1026 common observations


# RENAME COLUMNS AS APPROPRIATE
## If needed rename some of the columns 

data = rename(data, CountryCode = `Country code`, Migrants = Tot, MaleMigrants = Male, FemaleMigrants = Female, Refugees = refugees )

#####################################
#### Task 2  REVIEW: DOLLAR-SIGN #### 
#####################################


####Question 5 ####
# What is the average percentage of migrants on the total population of a country in 2015? 

data$Perc_migrants = as.numeric(data$Migrants/data$Pop)

mean(data$Perc_migrants[data$year == 2015])
# 13.33 % once the population numbers are multiplied by 1000.


### Question 6 ####
# Did the average percentage of refugees on the total of immigrants increased or decreased from 1990? 

data$Perc_refugees = as.numeric(data$Refugees / data$Migrants)

mean(data$Perc_refugees[data$year == 1990], na.rm = T)
# 15.44% of immigrants are refugees in 1990.
  
mean(data$Perc_refugees[data$year == 2015], na.rm = T)
# 15.16 of immigrants are refugees in 2015.
## Average is close to the same 


#### Question 7 ####
#What is the highest percentage of immigrants in a country in 2010? What is the smallest?

min(data$Perc_migrants[data$year == 2010])  # 0.063%
max(data$Perc_migrants[data$year == 2010]) # 87.84%


#### Question 8 #### 
# What is the median percentage of immigrants from the different continents/geographical areas in 2015? 
median(data[which(data$year == 2015),]$Perc_migrants) 
# 0.05 = 0.05% from all countries
data %>% 
  filter(year == 2015) %>%
  summarise(Median=median(Perc_migrants))
# also 0.05%


