install.packages("remotes")
remotes::install_github("dcl-docs/dcldata")

library("dcldata")

data_df = as.data.frame(congress) # dataframe
data_tb = as_tibble(congress) # tibble

dim(data_df)
dim(data_tb)

colnames(example_eagle_pairs)

example_eagle_pairs[,3:12]

# Always save your dataset as a new one. R has no 'go back' option
test =
  # Call your dataset
  pivot_longer(example_eagle_pairs,
                            # Identify the column of interest
                            -c("state","state_abbr"),
                            # Create a new column where you'll store the names of the c
                            names_to = "year",
                            # Create a new column where you'll store the value of a col
                            values_to = "observed_pairs")

test

us_rent_income

# Save your output as a new dataset
test2 =
  # Call the dataset that you want to modify
  pivot_wider(us_rent_income,
                               # Indicate the column from where the name(s) of the new colu
                               names_from = "variable" ,
                               # Indicate the column(s) from where the values of the new co
                               values_from = c(4:5))
test2



example_gymnastics_3

example_gymnastics_3R =
  pivot_longer(example_gymnastics_3,
               cols = -country,
               #Indicate the separator to split the column names
               names_sep = "_",
               names_to = c("exercise_type", "year", "gender"),
               values_to = "exercise_score",
               # transform the class of the column
               names_transform = list(year = as.numeric))

example_gymnastics_3R
