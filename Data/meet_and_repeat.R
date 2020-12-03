#Sari Kinnula
#27.11.2020

#Dataset 1 is from https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt
#Dataset 2 is from https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt

#Acces dplyr and tidyr 
library(dplyr)
library(tidyr)

#Read data. 
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", header=TRUE)
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header=TRUE)


#Explore the data
str(BPRS)
summary(BPRS)
glimpse(BPRS)

str(RATS)
summary(RATS)
glimpse(RATS)

#In the wide format BPRS data has 40 observation  (rows) of 11 variables. Columns are called treatment, subject and weeks from 0 to 8.
#RATS data has 16 observations of 13 variables. The variables are called Id, Group and different week days, for example WD1, WD8, WD15 and so on.
#In the wide format there a column for each variable. Wide format is easier to read and interpret than long format.


#Change the categorical variables to factors.
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)

glimpse(BPRS)
glimpse(RATS)

#Convert the data sets to long form. Add a week variable to BPRS and a Time variable to RATS. 
#Convert BPRS to long form.
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)

#Extract the week number.
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks,5,5)))

#Convert RATS to long form and ad Time variable.
RATSL <- RATS %>% gather(key = WD, value = Weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(WD, 3, 4))) 

#Take a glimpse at the BPRSL and RATSL data.
glimpse(BPRSL)
glimpse(RATSL)


#In the long format, BPRS data has 360 rows and 5 columns. Now each observation has it's own row.
#RATS data has 176 rows and 5 columns. 
#In the long format each row is one time point per subject where as in the wide format observations made at different time points are in different columns.

#As said before, wide format is easier to read and interpret than long format, but long format is better when for example using ggplot2 function.


write.table(BPRSL, file = "BPRSL.txt", sep = "\t",
            row.names = TRUE)
write.table(RATSL, file = "RATSL.txt", sep = "\t",
            row.names = TRUE)



