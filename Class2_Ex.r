
# sample creates a random sample with x number of elements
# 5 2 4 3 1
sample(x = 5)

# clearly x determines the content of the sample
# while size determines the size of the sample
# 3 1
sample(x = 5, size = 2)

# 56 71 76  5
sample(x = 1:100, size = 4)

# 5 10  8  7  6  3
sample(x = 1:10, size = 6)

# Error in sample.int(length(x), size, replace, prob) : 
# cannot take a sample larger than the population when 'replace = FALSE'
sample(x = 1:5, size = 6, replace = F)

# 3 5 5 3 1 1
sample(x = 1:5, size = 6, replace = T)

# 5 4 3 2 1
set.seed(23)
sample(5)

# 5 1 4 2 3
set.seed(34)
sample(5)

# 5 4 3 2 1
set.seed(23)
sample(5)

install.packages("randomNames")

library(randomNames)

test_names = randomNames(4)

# "Kaibetoney, Benjamin" "Lockwood, Justin" "Alarid, Rocky" "Rodriguez, Alexis"  
names

# "character"
class(names)

## Creating the Dataset

# all values should be drawn with replacement since there no reason to indicate 
# probability of any single value is not equal to the others

# Variable 1
# generate a random of sample of 40 observations ranging from 0 to 255
set.seed(434)
unemployed_days <- sample(x = 0:255, size = 40, replace = T)
unemployed_days

# Variable 2
# generate a random variable ranging from 1 to 25
set.seed(434)
job_apps <- sample(x = 1:25, size = 40, replace = T)
job_apps

# Variable 3
# dummy which indicates the gender of the individual
# Gender variables are generaly coded as 0 = male and 1 = female
gender <- sample(x = 0:1, size = 40, replace = T)
gender

# Variable 4
names = randomNames(40, gender = gender)
names

## Questions

# 6) Combine them in one dataframe.
df <- data.frame(unemployed_days, job_apps, gender, names)
df

# 7) Check the dimensions of the dataframe (# of columns and # of rows)
dim(df)

# rows = 40  columns = 4
  
# 8) Check the class of each columns. Fix them if the class is not appropriate 
#    (e.g., we don't want to keep factors)
lapply(df, class)

# $unemployed_days
# [1] "integer"
# 
# $job_apps
# [1] "integer"
# 
# $gender
# [1] "integer"
# 
# $names
# [1] "character"

# 9) Run some basic statistics to explore

# Average number of unemployment day and standard deviation
mean(df$unemployed_days)

# 122

sd(df$unemployed_days)

# 71.80993

# Minimum and maximum number of applications submitted as well as average number
summary(df$job_apps)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    8.00   13.00   13.03   18.25   25.00 

# Number of male and female applicants included in the sample
table(df$gender)

# man woman
# 0  1 
# 22 18 

# Average number of submitted applications for female applicants
mean(df$job_apps[df$gender == 1])

# 15

# Maximum number of employment days for female and male applicants
max(255 - df$unemployed_days[df$gender == 0])

# employment days for men
# 245

max(255 - df$unemployed_days[df$gender == 1])

# employment days for women
# 245

# 10) Think about policy! While this is a fake dataset, let's start thinking as 
#     policy makers and public managers would. Based on your results, what would you 
#     tell to a policy maker planning a new policy intervention on unemployment? 
#     If needed, you can run more descriptive statistics

# the average number of unemployed days for women
mean(df$unemployed_days[df$gender == 1])   

# 119.7222

# the average number of unemployed days for men
mean(df$unemployed_days[df$gender == 0])  

# 123.8636

# The most useful statistics here would probably be means and I would compare
# the means, if I can assume normality and equal variances, using t.test

t.test(job_apps ~ gender, data = df)

t.test(unemployed_days ~ gender, data = df)

# Although, from the tests and mean numbers there does seem to be a significant
# enough difference understand what the differences are.
# In the real world, most likely, women have an average number of unemployed days
# greater than men. In that case it might mean creating a program to bridge the 
# gap and remove barriers so that women get jobs sooner, for example.
