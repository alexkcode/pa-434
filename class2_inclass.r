hours_cooked_me <- c(2, 2, 2, 2, 2, 2, 2)

hours_cooked_me

hours_cooked_partner <- floor(runif(7, min=0, max=3))

hours_cooked_partner

sum(hours_cooked_me + hours_cooked_partner)

sum(hours_cooked_me) == 2 + 2 + 2 + 2 + 2 + 2 + 2

names <- c("Mary", "Mark", "John", "David", "Claudia")

yob <- c(1990, 1987, 1980, 1985, 1993)

class(names)

class(yob)

rbind(names, yob)

cbind(names, yob)

rows <- rbind(names, yob)

columns <- cbind(names, yob)

dim(rows)

dim(columns)

rows[-2,]

data_df <- as.data.frame(columns)

class(data_df$names)

class(data_df$yob)

data_df$names_R <- as.character(data_df$names)

data_df$yob <- as.numeric(as.character(as.factor(data_df$yob)))

data_df$yob

data_df[data_df$names == "David",]

data_df[2021 - data_df$yob < 40,]

data_df[2021 - data_df$yob < median(2021 - data_df$yob),]
