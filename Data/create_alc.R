#Sari Kinnula, 7.11.2020. Exercise 3 for the course Introduction to Open Data Science 2020. Logistic regression.
#Data is from UCI Machine Learning Repository: https://archive.ics.uci.edu/ml/datasets/Student+Performance. P. Cortez and A. Silva. Using Data Mining to Predict Secondary School Student Performance. In A. Brito and J. Teixeira Eds., Proceedings of 5th FUture BUsiness TEChnology Conference (FUBUTEC 2008) pp. 5-12, Porto, Portugal, April, 2008, EUROSIS, ISBN 978-9077381-39-7. 

url_math <-read.csv("student-mat.csv")
url_por <-read.csv("student-por.csv")

# Look at the dimensions of the data
dim(url_math)
dim(url_por)

# Look at the structure of the data
str(url_math)
str(url_por)

url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets"

# web address for math class data
url_math <- paste(url, "student-mat.csv", sep = "/")

# print out the address
url_math

# read the math class questionaire data into memory
math <- read.table(url_math, sep = ";" , header=TRUE)

# web address for portuguese class data
url_por <- paste(url, "student-por.csv", sep = "/")

# print out the address
url_por

# read the portuguese class questionaire data into memory
por <- read.table(url_por, sep = ";", header = TRUE)

# look at the column names of both data
colnames(math)
colnames(por)

#Join the two datasets:

# access the dplyr library
library(dplyr)

# common columns to use as identifiers
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# join the two datasets by the selected identifiers
math_por <- inner_join(math, por, by = join_by, suffix = c(".math", ".por"))

# see the new column names
colnames(math_por)

# glimpse at the data
glimpse(math_por)

# print out the column names of 'math_por'
colnames(math_por)

# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# columns that were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column  vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data
glimpse(alc)

#Create a new column alc_use:

# access the 'tidyverse' packages dplyr and ggplot2
library(dplyr); library(ggplot2)

# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# initialize a plot of alcohol use
g1 <- ggplot(data = alc, aes(x = alc_use, fill = sex))

# define the plot as a bar plot and draw it
g1 + geom_bar()

# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

# initialize a plot of 'high_use'
g2 <- ggplot(alc, aes(high_use))

# draw a bar plot of high_use by sex
g2 + facet_wrap("sex") + geom_bar()

glimpse(alc)

write.table(alc, file = "alc.txt", sep = "\t",
            row.names = FALSE)
my_data<- read.delim("alc.txt")
str(my_data)
head(my_data)
