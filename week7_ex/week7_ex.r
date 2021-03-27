install.packages("tidytuesdayR")

library(tidyverse)
library(ggrepel)

tuesdata <- tidytuesdayR::tt_load("2021-01-05")
transit_cost <- tuesdata$transit_cost

# STEP 1
# Research questions

# Assuming inflation is either irrelevant or doesn't affected cost numbers:
# How does the "real" total cost of a transit project compare to its cost per km?
# Does the cost per km increase as the length of the transit line increases?
# Which countries are best able to maintain cost efficiency for long transit lines?

# STEP 2
# Variable choice, 1, 2, or 3 variables at most

# country, real_cost, cost_km_millions

transit_cost1 <-
  transit_cost %>%
  # since we care about countries, remove ones without country info
  # cost_km_millions has some NAs
  filter(!is.na(country),!is.na(cost_km_millions)) %>%
  # real_cost is not clean
  mutate(
    real_cost = as.numeric(real_cost),
    group = case_when(
      length > 100 & abs(cost_km_millions - mean(cost_km_millions)) < sd(cost_km_millions) ~ "Efficient",
      cost_km_millions > 1000 ~ "Costly",
      TRUE ~ "Unexceptional"
    )
  )

# STEP 3
# Think about your plot. Start by going through the tools available to you at this stage (e.g., scatterplots,
# histograms, bar plots...). Decide which one is the most appropriate. Sketch out on a piece of paper how
# your graph would look like. Focus your attention on key elements only: what do the axes represent? What
# variables are plotted?

# For the second question we want to know about the relationship between the total cost
# and cost per km, so a line graph seems appropriate.

# Step 4
# Start working in R. Go back to step 2 and clean your data. Once your data are ready, move to step 3 and
# realize a simple exploratory plot to see how your data look like.
# You might be satisfied by your plot and decide to move forward. It is also possible that your plot disappoints
# you and you will have to start again the process!

transit_cost1 %>%
  ggplot(mapping = aes(x = length, y = cost_km_millions)) + geom_point()

# Step 5
# Start improving your plot by doing basic adjustements: fix your axes, put labels, insert your title and
# subtitle, check out axes labels and ticks...

# the point of using points here (pun intended) is isolate the points where
# there is low cost/km for long line length projects
transit_cost1 %>%
  ggplot(mapping = aes(x = length, y = cost_km_millions)) + 
  geom_point() +
  xlab("Length of Line in km") +
  ylab("Cost/km in millions of USD") +
  ggtitle("Cost Effiency by Line Length") +
  theme(plot.title = element_text(hjust = 0.5))

# Step 6
# Before working on colors and themes, I would encourage you to look at the aes parameters. Is there anything
# else that you could visualize in your plot? E.g., change the size or shape of your dots in a scatterplot; split
# your bars in a bar plot; and so on.
# You should also think about vlines and hlines that might help you tell a story to your audience - what is the
# main findings that you want your audience to see right away?
# Try to think about the data once more and the story that you want to tell before making your graph prettier.

transit_cost1 %>%
  ggplot(mapping = aes(x = length, y = cost_km_millions)) +
  geom_point() +
  xlab("Length of Line in km") +
  ylab("Cost/km in millions of USD") +
  ggtitle("Cost Effiency by Line Length") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = median(transit_cost1$cost_km_millions)) +
  geom_label_repel(aes(label = ifelse(
    length > 100 | cost_km_millions > 1000, country, ''
  )), hjust = 0, vjust = 0)

# Step 7
# Pick color for your graph. Adjust borders and fonts. Add a theme that you like and that highlights your
# findigs.

p <- transit_cost1 %>%
  ggplot(mapping = aes(x = length, y = cost_km_millions, color = group)) +
  geom_point() +
  xlab("Length of Line in km") +
  ylab("Cost/km in millions of USD") +
  ggtitle("Transit Line Cost Effiency") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_label_repel(
    aes(label = ifelse(group != "Unexceptional", country, '')),
    box.padding   = 0.35,
    point.padding = 0.5,
    segment.color = 'grey50'
  ) +
  theme(legend.title = element_blank())
p

# Step 8
# Save your plot. Go back to it a few hours later. Can you tell the main message right away? Show it to
# someone else: can they clearly draw the same information from the plot?
# Adjust your plot as necessary based on this feedback.

ggsave(filename = "cost_efficiency_by_length.png", plot = p, device = "png")
