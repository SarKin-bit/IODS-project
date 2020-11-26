hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

#Explore the datasets, see dimensions, structure and summaries of the variables.
dim(hd)
str(hd)
summary(hd)

#There are 195 observations and 8 variables.

dim(gii)
str(gii)
summary(gii)

#There are 195 observations and 10 variables.

#print out the column names of the data.
colnames(hd)

#change the name of "Human.Development.Index..HDI" to "HDI".
colnames(hd)[3] <- "HDI"

#change the name of "Life.Expectancy.at.Birth" to "Life.Exp".
colnames(hd)[4] <- "Life.Exp"

#change the name of "Expected.Years.of.Education" to "Edu.Exp".
colnames(hd)[5] <- "Edu.Exp"

#change the name of "Mean.Years.of.Education" to "Years.Edu".
colnames(hd)[6] <- "Years.Edu"

#change the name of "Gross.National.Income..GNI..per.Capita" to "GNI".
colnames(hd)[7] <- "GNI"

#change the name of "GNI.per.Capita.Rank.Minus.HDI.Rank" to "GNI.minus".
colnames(hd)[8] <- "GNI.minus"

#print out the new column names of the data.
colnames(hd)



#print out the column names of the data.
colnames(gii)

#change the name of "Gender.Inequality.Index..GII." to "GII".
colnames(gii)[3] <- "GII"

#change the name of "Maternal.Mortality.Ratio" to "Mat.Mor".
colnames(gii)[4] <- "Mat.Mor"

#change the name of "Adolescent.Birth.Rate" to "Ado.Birth".
colnames(gii)[5] <- "Ado.Birth"

#change the name of "Percent.Representation.in.Parliament" to "Parli.F".
colnames(gii)[6] <- "Parli.F"

#change the name of "Population.with.Secondary.Education..Female" to "Sec.Edu.F".
colnames(gii)[7] <- "Sec.Edu.F"

#change the name of "Population.with.Secondary.Education..Male." to "Sec.Edu.M".
colnames(gii)[8] <- "Sec.Edu.M"

#change the name of "Labour.Force.Participation.Rate..Female." to "Lab.Part.F".
colnames(gii)[9] <- "Lab.Part.F"

#change the name of "Labour.Force.Participation.Rate..Male." to "Lab.Part.M".
colnames(gii)[10] <- "Lab.Part.M"

#print out the new column names of the data.
colnames(gii)


#Mutate the “Gender inequality” data and create two new variables.


library(dplyr)
gii <- mutate(gii, Edu2.FM = Sec.Edu.F/Sec.Edu.M)
gii <- mutate(gii, Labo.FM = Lab.Part.F/Lab.Part.M)

#Print out the new column names of the data.
colnames(gii)

#Join the two datasets using the variable coutry as the identifier.

?inner_join

human<-inner_join(gii, hd, by = "Country", suffix = c(".gii", ".hd"))

#Check that everything looks like it should be.

human
dim(human)

#There are 19 variables and 195 observations.

write.table(human, file="data/human.txt", sep="\t")

#Check that everything looks ok.
read.table("data/human.txt", sep="\t")

#Data wrangling continues, week 5.

#Explore the structure and the dimensions of the data and describe the dataset briefly.
read.table("data/human.txt", sep="\t")
dim(human)
str(human)
summary(human)

#There are 19 variables and 195 observations. Variables are:
#Gender Inequality Index Rank (GII.Rank),
#Country name (Country),
#Gender Inequality Index GII (GII), 
#Maternal Mortality Ratio (Mat.Mor), 
#Adolescent Birth Rate (Ado.Birth), 
#Percentage of female representatives in parliament (Parli.F),
#Population with Secondary Education, Female (Sec.Edu.F), 
#Population with Secondary Education, Male (Sec.Edu.M), 
#Labour Force Participation Rate,Female (Lab.Part.F), 
#Labour Force Participation Rate, Male (Lab.Part.M), 
#Edu2.F / Edu2.M (Edu2.FM), 
#Labo2.F / Labo2.M (Labo.FM), 
#Human development index rank (HDI.Rank),
#Human development index (HDI), 
#Life expectancy at birth (Life.Exp),
#Expected Years of Education (Edu.Exp),
#Mean Years of Education (Years.Edu)
#Gross National Income per capita (GNI),
#GNI per Capita Rank Minus HDI Rank (GNI.minus).


#Mutate the data: transform the Gross National Income (GNI) variable to numeric. 
library(stringr)
human<-mutate(human, GNI=str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric())

#Check the structure.
str(human)


#Exclude unneeded variables: keep only the columns matching the following variable names (described in the meta file above):
#"Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F" 

#Make a vector of the columns to keep.
keep <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")

#Select the 'keep' columns.
human <- dplyr::select(human, one_of(keep))

#Print out a completeness indicator of the 'human' data.
complete.cases(human)

#Print out the data along with a completeness indicator as the last column.
data.frame(human[-1], comp = complete.cases(human))

#Filter out all rows with NA values.
human <- filter(human, complete.cases(human))

#Make a vector for columns to keep.
keep <- c("Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")

#Select the 'keep' columns.
human <- dplyr::select(human, one_of(keep))


#Remove all rows with missing values.
human <- filter(human, complete.cases(human))


#Remove the observations which relate to regions instead of countries. 
#Save the regions.
regions<-c("East Asia and the Pacific", 
           "Latin America and the Caribbean", "Sub-Saharan Africa", 
           "World", "Europe and Central Asia",
           "South Asia", "Arab States")

#Remove the regions.
human<-human[!human$Country%in%regions,]

#Add country as row name.
rownames(human)<-human$Country


#Check the file.
head(human)
dim(human)

#Remove the first column for country.
human<-human[,2:9]

#Check the number of columns.
head(human)
dim(human)

#There are 155 observations and 8 variables.

#Save table.
write.table(human, file = "human.txt", sep = "\t",
            row.names = TRUE)
my_data<- read.delim("human.txt")
str(my_data)
head(my_data)
