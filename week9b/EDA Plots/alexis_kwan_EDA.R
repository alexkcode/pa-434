# install.packages("GGally")

library(tidyverse)
library(GGally)
library("lubridate")
# Exploratory Data Analysis


# Grouped geom_bar graph shows the difference in counts across the different types
# but also shows how the numbers evolve over the different months. It also gives
# clear indicators of when certain types of crime happen, signaled by the tallest
# bars, thus addressing the first two research questions.
crime %>%
  mutate(month = stamp("April")(month)) %>%
  ggplot(aes(x = month, fill = type)) +
  geom_bar(position = "dodge") + 
  labs(x = "Month", y = "Count", fill = "Type of Crime")

# Alternatively using a smoothed line graph can show how the crimes change from
# month to month in a clearer fashion, but the ability to compare the counts
# for each individual month is lost.
crime %>%
  group_by(month, type) %>%
  count() %>%
  ggplot(aes(x = month, y = n, color = type)) +
  geom_smooth()

filtered_crime <- 
  crime %>%
  group_by(location, type) %>%
  count(name = "Count") %>% 
  arrange(desc(Count))

# Heatmap seemed to be the best choice for answering the third research question.
# The color intensity is a good way to see the relative propensity for the crime
# to occure based on the location split in the y-axis. This matrix like display
# gives the ability to immediately survey the different combinations of scenarios.
# I only chose the top 20 because they were the high frequency and I felt those 
# were likely the most relevant because they made up the highest proportion. The
# number 20 was arbitrary.
head(filtered_crime, 20) %>%
  ggplot(aes(x = type, y = location, fill = Count)) + 
  geom_tile() +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  xlab("Type of Crime") +
  ylab("Location") +
  # I changed it from the original gray theme because I felt the gray background
  # may confuse the reader since colors have meaning associated with the data.
  theme_classic()

