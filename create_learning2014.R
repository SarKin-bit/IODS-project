#Sari Kinnula, 5.11.2020. Exercise 2 for the course Introduction to Open Data Science 2020.
#Data reference: http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt

lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)
# Look at the dimensions of the data
dim(lrn14)

# Look at the structure of the data
str(lrn14)

#There are 183 rows (obs.) and 60 columns (variables)

library(dplyr)
# questions related to deep, surface and strategic learning
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# select the columns related to deep learning and create column 'deep' by averaging
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

# select the columns related to surface learning and create column 'surf' by averaging
surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)

# select the columns related to strategic learning and create column 'stra' by averaging
strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)

# choose a handful of columns to keep
keep_columns <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points")


# select the 'keep_columns' to create a new dataset
learning2014 <- select(lrn14, one_of(keep_columns))

# see the structure of the new dataset
str(learning2014)

# Exclude observations where the exam points variable is zero:

# select rows where points is greater than zero
learning2014 <- filter(learning2014, Points > 0)
str(learning2014)

write.table(learning2014, file = "learning2014.txt", sep = "\t",
            row.names = FALSE)
my_data<- read.delim("learning2014.txt")
str(my_data)
head(my_data)
