
install.packages("fivethirtyeight")
library(tidyverse)
library(fivethirtyeight)

cabinet_turnover

cabinet_turnover %>%
  group_by(position) %>%
  summarise(avg_length = mean(length, na.rm = TRUE)) %>%
  arrange(avg_length)

cabinet_turnover %>%
  group_by(appointee, president) %>%
  summarise(count = n()) %>%
  group_by(appointee) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

install.packages("nycflights13")
library(nycflights13)

data <- flights

# For each destination, compute the total minutes of delay. For each flight,
# compute the proportion of the total delay for its destination. (G2 and G3)

data1 <- data %>%
  filter(dep_delay > 0) %>%
  group_by(dest) %>%
  mutate(sum_delay = sum(dep_delay, na.rm = TRUE),
         prop_delay = dep_delay / sum_delay)
data1

