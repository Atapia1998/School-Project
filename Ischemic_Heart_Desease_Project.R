#
#
#Arturo E Tapia 668309766 atapia25
#Project1
# 9.24.20
#stats 382


#part1 preparation
#1-3
setwd("D:/Arturo/Documents/Ischemic_Heart_Desease_Project")
Ischemic_Heart_Disease <- read.csv(file.path("Ischemic_Heart_Disease.csv"), sep = ",")
str(Ischemic_Heart_Disease)
summarydat <- summary(Ischemic_Heart_Disease)

#4
Ischemic_Heart_Disease[is.na(Ischemic_Heart_Disease)] <- 0
#I think this is bad to do as many of the NA could impede from the person getting help
# if they have NA  for drugs taken maybe its because they are allergic or have yet to start treatment

#5
Ischemic_Heart_Disease$Gender[Ischemic_Heart_Disease$Gender == 1] <- "Male"
Ischemic_Heart_Disease$Gender[Ischemic_Heart_Disease$Gender == 2] <- "Female"
Ischemic_Heart_Disease$Gender[Ischemic_Heart_Disease$Gender == 3] <- "Otherwise"
Ischemic_Heart_Disease$Gender <- as.factor(Ischemic_Heart_Disease$Gender)
class(Ischemic_Heart_Disease$Gender)

#6
Ischemic_Heart_Disease$Age_category <- ifelse(Ischemic_Heart_Disease$Age < 40, "Young Adult", "Adult")
Ischemic_Heart_Disease$Age_category[Ischemic_Heart_Disease$Age_category == "Adult" &  Ischemic_Heart_Disease$Age >= 60]  <- "Older"
Ischemic_Heart_Disease$Age_category[Ischemic_Heart_Disease$Age_category == "Adult" &  Ischemic_Heart_Disease$Age <= 59 & Ischemic_Heart_Disease$Age >= 40 ]  <- " Middle Aged"


#part2
#7-11
max(Ischemic_Heart_Disease$Total.Claim.Cost)
min(Ischemic_Heart_Disease$Total.Claim.Cost)
sum(Ischemic_Heart_Disease$Total.Claim.Cost)
mean(Ischemic_Heart_Disease$Total.Claim.Cost)
sd(Ischemic_Heart_Disease$Total.Claim.Cost)
age_table <- table(Ischemic_Heart_Disease$Gender)
agefrequencytable <- prop.table(age_table)

#12-14
age_Q1 <- quantile(Ischemic_Heart_Disease$Age,.25)
age_Q3 <- quantile(Ischemic_Heart_Disease$Age,.75)
age_lowerfence <- age_Q1 - (1.5 * (age_Q3 - age_Q1))
age_upperfence <- age_Q3 + (1.5 * (age_Q3 - age_Q1))

Intervention_Q1 <- quantile(Ischemic_Heart_Disease$Interventions ,.25)
Intervention_Q3 <- quantile(Ischemic_Heart_Disease$Interventions ,.75)
Intervention_lowerfence <- Intervention_Q1 - (1.5 * (Intervention_Q3 - Intervention_Q1))
Intervention_upperfence <- Intervention_Q3 + (1.5 * (Intervention_Q3 - Intervention_Q1))

ER_Q1 <- quantile(Ischemic_Heart_Disease$ER.Visits ,.25)
ER_Q3 <- quantile(Ischemic_Heart_Disease$ER.Visits ,.75)
ER_lowerfence <- ER_Q1 - (1.5 * (ER_Q3 - ER_Q1))
ER_upperfence <- ER_Q3 + (1.5 * (ER_Q3 - ER_Q1))

age_dataframe <- subset(Ischemic_Heart_Disease, Ischemic_Heart_Disease$Age <= age_lowerfence | Ischemic_Heart_Disease$Age >= age_upperfence)
Intervention_dataframe <- subset(Ischemic_Heart_Disease, Ischemic_Heart_Disease$Interventions <= Intervention_lowerfence | Ischemic_Heart_Disease$Interventions >= Intervention_upperfence)
ER_dataframe <- subset(Ischemic_Heart_Disease, Ischemic_Heart_Disease$ER.Visits <= ER_lowerfence | Ischemic_Heart_Disease$ER.Visits >= ER_upperfence)
#15-16
summary(age_dataframe$Total.Claim.Cost)
sd(age_dataframe$Total.Claim.Cost)
summary(Intervention_dataframe$Total.Claim.Cost)
sd(Intervention_dataframe$Total.Claim.Cost)
summary(ER_dataframe$Total.Claim.Cost)
sd(ER_dataframe$Total.Claim.Cost)
summary(Ischemic_Heart_Disease$Total.Claim.Cost)
sd(Ischemic_Heart_Disease$Total.Claim.Cost)

age_outlier_table <- table(age_dataframe$Gender)
Intervention_outlier_table <- table(Intervention_dataframe$Gender)
ER_outlier_table <- table(ER_dataframe$Gender)
prop.table(age_outlier_table)
prop.table(Intervention_outlier_table)
prop.table(ER_outlier_table)
agefrequencytable



#17
count <- nrow(Ischemic_Heart_Disease)
totalcount <- 0
for(i in 1:count){
  if(Ischemic_Heart_Disease$Drugs[i] > 1)
    totalcount <- totalcount + 1
}

#18

Interactions <- 0
Ischemic_Heart_Disease <- cbind(Ischemic_Heart_Disease,Interactions)

for(j in 1:count){
  Ischemic_Heart_Disease$Interactions[j] <- Ischemic_Heart_Disease$Interventions[j] + Ischemic_Heart_Disease$ER.Visits[j]
  
}


#PART 3 GRAPHS

Age_category_table <- table(Ischemic_Heart_Disease$Age_category)
barplot(Age_category_table)
#A majority of those with heart disease are over the age of 40 as
#the biggest values consist of middle aged and older people
hist(Ischemic_Heart_Disease$ER.Visits,main = "Frequency of ER visits" ,xlab = "ER visits")
#It seems that most of the people on this list very rarely need to 
#to the ER, most likely when they first experience symptoms for heart desease
boxplot(Ischemic_Heart_Disease$Interventions)


