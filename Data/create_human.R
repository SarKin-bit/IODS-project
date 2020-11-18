hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

#Explore the datasets, see dimensions, structure and summaries of the variables.
dim(hd)
str(hd)
summary(hd)


dim(gii)
str(gii)
summary(gii)

#print out the column names of the data.
colnames(hd)

#change the name of "Human.Development.Index..HDI" to "hdi".
colnames(hd)[3] <- "hdi"

#change the name of "Life.Expectancy.at.Birth" to "life_exp".
colnames(hd)[4] <- "life_exp"

#change the name of "Expected.Years.of.Education" to "edu_exp".
colnames(hd)[5] <- "edu_exp"

#change the name of "Mean.Years.of.Education" to "years_edu".
colnames(hd)[6] <- "years_edu"

#change the name of "Gross.National.Income..GNI..per.Capita" to "gni_cap".
colnames(hd)[7] <- "gni_cap"

#change the name of "GNI.per.Capita.Rank.Minus.HDI.Rank" to "gni_cap_minus".
colnames(hd)[8] <- "gni_cap_minus"

#print out the new column names of the data.
colnames(hd)



#print out the column names of the data.
colnames(gii)

#change the name of "Gender.Inequality.Index..GII." to "gii".
colnames(gii)[3] <- "gii_i"

#change the name of "Maternal.Mortality.Ratio" to "mat_mor".
colnames(gii)[4] <- "mat_mor"

#change the name of "Adolescent.Birth.Rate" to "adol_birth".
colnames(gii)[5] <- "adol_birth"

#change the name of "Percent.Representation.in.Parliament" to "reps_parl".
colnames(gii)[6] <- "reps_parl"

#change the name of "Population.with.Secondary.Education..Female" to "sec_edu_f".
colnames(gii)[7] <- "sec_edu_f"

#change the name of "Population.with.Secondary.Education..Male." to "sec_edu_m".
colnames(gii)[8] <- "sec_edu_m"

#change the name of "Labour.Force.Participation.Rate..Female." to "lab_part_f".
colnames(gii)[9] <- "lab_part_f"

#change the name of "Labour.Force.Participation.Rate..Male." to "lab_part_m".
colnames(gii)[10] <- "lab_part_m"

#print out the new column names of the data.
colnames(gii)


#5.	Mutate the “Gender inequality” data and create two new variables. The first one 
#should be the ratio of Female and Male populations with secondary education in each country.
#(i.e. edu2F / edu2M). The second new variable should be the ratio of labour force participation of 
#females and males in each country (i.e. labF / labM). (1 point)

library(dplyr)
gii <- mutate(gii, sec_edu_rat = sec_edu_f/sec_edu_m)
gii <- mutate(gii, lab_rat = lab_part_f/lab_part_m)

#print out the new column names of the data.
colnames(gii)

# Join the two datasets using the variable coutry as the identifier

?inner_join

human<-inner_join(gii, hd, by = "Country", suffix = c(".gii", ".hd"))

# Check that everything looks like it should be

human
dim(human)

#There are 19 variables and 195 observations, like there should be.


write.table(human, file="data/human.txt", sep="\t")

#Check that everything looks ok
read.table("data/human.txt", sep="\t")



