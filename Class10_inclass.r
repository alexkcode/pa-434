library(stevedata)
library(tidyverse)

data = gss_wages

data$maritalcat_F = as.factor(data$maritalcat)

plot_3 = data %>%
  mutate(maritalcat_F =
           fct_rev(
             fct_infreq(data$maritalcat_F))) %>%
  ggplot() +
  geom_bar(mapping = aes(maritalcat_F))
plot_3

# plot the average level of income (a continuous
# variable) by education level (a categorical variable)
data %>%
  mutate(educcat_f = as.factor(educcat)) %>%
  filter(!is.na(educcat_f)) %>%
  group_by(educcat_f) %>%
  summary(realrinc_avg = mean(realrinc, na.rm = T)) %>%
  ggplot() +
  geom_bar(mapping = aes(y = educcat_f, x = realrinc_avg),
           stat = "identity")

data %>%
  mutate(educcat_f = as_factor(educcat)) %>%
  filter(!is.na(educcat_f),
         !is.na(age)) %>%
  group_by(educcat_f, age) %>%
  summary(realrinc_avg = mean(realrinc, na.rm = T)) %>%
  ggplot() +
  geom_line(mapping = aes(x = educcat_f, y = realrinc_avg, color = educcat_f))
