#################################
#
# PA 434: Assignment 6
# Alexis Kwan
#
#################################

library(tidyverse)
api_data <- read_csv("api_data.csv")

#################################
#
# STEP 1
#
#################################

# 1. What is the unit of analysis? In other words, what should each row represent?

# The unit of analysis is the county school.

# 2. Is the data “tidy”? Why so? Why not? If not, make the appropriate 
# modifications to tidy your data.

# In a tidy dataset every row should be an observation and every observation 
# should contain all values that were measured on that unit of observation. 
# If we consider county schools to be the unit of analysis, or what an observation
# represents, then it is not tidy because variable_name contains designations
# for variables representing aspects of the observable. Similarly, 
# community_schooltype encompasses information for two different types of variables
# in one, so it is too condensed as well.

county_schools <- pivot_wider(
  api_data,
  names_from = "variable_name",
  values_from = "percentage"
) %>% separate(
  col = community_schooltype, 
  into = c("community","schooltype"), 
  sep = "_"
) %>% mutate(
  schooltype = case_when(schooltype == "E" ~ "Elementary School",
                         schooltype == "M" ~ "Middle School",
                         schooltype == "H" ~ "High School")
)

# 3. Briefly report anything that the reader should now about missing data 
# in the dataset.

apply(X = is.na(county_schools), MARGIN = 2, FUN = sum)

# The dataset does not have any missing values, so there appears to be no issues
# with missing data.

#################################
#
# STEP 2
#
#################################

# 1. Use plots to explore the distribution or frequency of variables in your dataset. 
# The variable uid and county can be excluded from the analysis. Report your 
# findings and considerations.

install.packages("gridExtra")

library(gridExtra)

meals_p <- ggplot(data = county_schools) + 
  geom_histogram(mapping = aes(x = meals)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100))
  
colgrad_p <- ggplot(data = county_schools) + 
  geom_histogram(mapping = aes(x = colgrad)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100))
  
fullqual_p <- ggplot(data = county_schools) + 
  geom_histogram(mapping = aes(x = fullqual)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100))

api_p <- ggplot(data = county_schools) + 
  geom_histogram(mapping = aes(x = api))

schooltype_p <- ggplot(data = county_schools) +
  geom_bar(mapping = aes(x = schooltype)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

community_p <- ggplot(data = county_schools) +
  geom_bar(mapping = aes(x = community)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

grid.arrange(arrangeGrob(meals_p, colgrad_p, fullqual_p), 
             arrangeGrob(api_p),
             arrangeGrob(schooltype_p, community_p),
             ncol = 3)

# The meals variable seems to be uniformly distributed for the most part, while 
# colgrad, representing the percentage of parents with degrees, is seemingly 
# normal but is very right skewed. On the other hand fullqual, the percentage 
# of teachers that are fully qualified, is very left skewed normal distribution.
# api looks to be a more centered normal distribution but with a flatter peak.
# Surburban_E type seems to be the largest proportion of school types.

# 2. Plot the correlation between the performance index and parents’ education.

ggplot(data = county_schools) + 
  geom_point(mapping = aes(x = colgrad, y = api))

# 2a. State your expectations before drawing the graph and discuss what you have 
# learned from the graph.



# 2a [OPTIONAL] If you are confortable with it, add a regression line to your graph.

ggplot(data = county_schools, mapping = aes(x = colgrad, y = api)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# 2b. Finalize your graph by showing the breakdown by community type and then split 
# the graph according to the type of school.

ggplot(data = county_schools, mapping = aes(x = colgrad, y = api)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~community + schooltype)

# 2c. Summarize your findings.

# Performance across different communities seems to be very similar, with urban
# high schools performing marginally better than high schools in other areas.
# Overall, as the percentage of parents with degrees increases, performance in
# school increases regardless of area or type of school. However, it should be 
# noted that the sample size for rural schools, especially high school and middle
# school appears to be significantly smaller than for other areas and elementary
# school overall has the most samples compared to other levels of schooling. 

# 3. Create a bar plot representing the 10 schools with the highest academic 
# performance index and the 10 schools with the lowest academic performance. 
# Make sure that they are colored differently and ranked from the highest API 
# to the lowest. Add a line representing the overall API average across all schools.
# Hint: you first need to use dplyr functions to create the data you need.

county_schools_ranked <- 
  county_schools %>% 
  arrange(api) %>% 
  unite("name", county, uid, sep = " ") %>%
  mutate(api_rank = row_number(desc(api))) %>%
  mutate(top = case_when(api_rank <= 10 ~ "top_10",
                         api_rank > max(api_rank) - 10 ~ "bottom_10",
                         TRUE ~ "else")) %>%
  filter(top != "else")

county_schools_ranked %>% 
  ggplot(mapping = aes(x = api, y = reorder(name, api), fill = top)) +
  geom_col() +
  geom_vline(xintercept = mean(county_schools$api)) +
  xlab("Performance Index") +
  ylab("School") +
  labs(fill = "Rank")

# 4. A state policymakers is trying to identify which counties should receive 
# funding priority in order to improve their average performance index.

# 4a. Think about a couple of criteria to select priority counties

# The most obvious metric for a criteria seems to be the overall ranking, i.e.
# if the county is in the bottom 10 of the performance ranking they should 
# receive priority.
# Another issue that could be addressed is the percentage of fully qualified 
# teachers. If the percent is below a certain threshold, that county should get
# more funding.

# 4b. Create a plot showing which counties should be prioritized and why.
# Note: You are welcome to explore anything else that captures your attention 
# and attach the code + explanation for review

ggplot(data = county_schools, mapping = aes(x = meals, y = api)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~community + schooltype)

ggplot(data = county_schools, mapping = aes(x = fullqual, y = api)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~community + schooltype)

ggplot(data = county_schools, mapping = aes(x = fullqual, y = meals)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~community + schooltype)

# There's an inverse correlation between the number kids that are qualified for
# subsizied meals and number of fully qualified teachers. School that either
# have the least number of qualified teachers or most number of kids with
# subsidized meals tend to perform poorly and may require additional funding.

county_schools_ranked %>% 
  ggplot(mapping = aes(y = api, x = fullqual, color = top)) +
  geom_point() +
  geom_vline(xintercept = median(county_schools$fullqual)) +
  ylab("Performance Index") +
  xlab("% of Fully Qualified Teachers") +
  labs(fill = "Rank")

# Schools below a threshold, like the mean value of the % of qualified teachers, 
# could be prioritized.
# Or be given funding based on some quartile split of the % of qualified teachers.