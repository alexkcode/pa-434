library(tidyverse)
# Exploratory Data Analysis

# Crime seasonality by type
crime %>%
  group_by(month, type) %>%
  count() %>%
  ggplot(aes(x = month, y = n, color = type)) +
    geom_line() +
    scale_x_date()


crime %>% 
  group_by(month(Date), location, type) %>%
  count() %>%
  ggplot(aes(x = `month(Date)`, y = n, fill = location)) +
  geom_col() # too many locations

crime %>%
  count(location) %>%
  arrange(desc(n)) %>%
  view()

