install.packages("stevedata")

library("stevedata")
library(tidyverse)

data(election_turnout)

ggplot(data = election_turnout) + geom_histogram(mapping = aes(x = turnoutho))

ggplot(data = election_turnout) + 
  geom_histogram(mapping = aes(x = turnoutho), binwidth = 10)

ggplot(data = election_turnout) + 
  geom_histogram(mapping = aes(x = turnoutho), binwidth = 5)

ggplot(data = election_turnout) + 
  geom_density(mapping = aes(x = turnoutho)) + 
  geom_vline(xintercept = median(election_turnout$turnoutho))

ggplot(data = election_turnout) +   
  geom_bar(mapping = aes(x = region)) +   
  geom_hline(yintercept = 10)                               

ggplot(data = election_turnout) +   
  geom_bar(mapping = aes(x = region, fill = as.character(trumpw)))

ggplot(data = election_turnout) +  
  geom_bar(mapping = aes(x = region, fill = as.character(trumpw)), position = "fill")

