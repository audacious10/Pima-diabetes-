#Reading the dataset
diabetes <- read.csv("F:/Courses/MVA/diabetes.csv")
diabetes

#Lets have 0/1 as No/Yes

diabetes$Outcome <- as.factor(diabetes$Outcome)
levels(diabetes$Outcome) <- c("No","Yes")

colnames(diabetes)[colnames(diabetes)=="Outcome"] <- "Diabetic"

#Inspecting the dataset 
summary(diabetes)

library(PerformanceAnalytics)
library(ggplot2)
library(GGally)
library(Amelia)
library(ggcorrplot)
library(magrittr)
library(dplyr)
library(plotly)
library(ggcorrplot)
library(ggplot2)

attach(diabetes)

# t-tests, Diabetic vs. Non diabetic
with(data=diabetes,t.test(BMI[Diabetic=="Yes"],BMI[Diabetic=="No"],var.equal=TRUE))
with(data=diabetes,t.test(BloodPressure[Diabetic=="Yes"],BloodPressure[Diabetic=="No"],var.equal=TRUE))
with(data=diabetes,t.test(Glucose[Diabetic=="Yes"],Glucose[Diabetic=="No"],var.equal=TRUE))
with(data=diabetes,t.test(Pregnancies[Diabetic=="Yes"],Pregnancies[Diabetic=="No"],var.equal=TRUE))
with(data=diabetes,t.test(SkinThickness[Diabetic=="Yes"],SkinThickness[Diabetic=="No"],var.equal=TRUE))

# Hotelling's T2 test. Comparing multivariate means between Diabetic and Non Diabetic
install.packages("Hotelling")
library(Hotelling)
T2testAB <- hotelling.test(Pregnancies + Glucose + BloodPressure + SkinThickness + BMI ~Diabetic, data=diabetes)
# Output of the function hotelling.test is given
cat("T2 statistic =",T2testAB$stat[[1]],"\n")
print(T2testAB)
#  T2 statistic is located in the first element of the list "stat"
View(T2testAB)

# testing Variation
# F-test for BMI
var.test(BMI[Diabetic=="Yes"],BMI[Diabetic=="No"])

var.test(BloodPressure[Diabetic=="Yes"],BMI[Diabetic=="No"])

var.test(Age[Diabetic=="Yes"],Age[Diabetic=="No"])

var.test(Glucose[Diabetic=="Yes"],Glucose[Diabetic=="No"])

var.test(SkinThickness[Diabetic=="Yes"],SkinThickness[Diabetic=="No"])

#F-test
attach(diabetes)
var.test(Pregnancies[Diabetic=="Yes"],Pregnancies[Diabetic=="No"])
var.test(Glucose[Diabetic=="Yes"],Glucose[Diabetic=="No"])
var.test(Insulin[Diabetic=="Yes"],Insulin[Diabetic=="No"])
var.test(SkinThickness[Diabetic=="Yes"],SkinThickness[Diabetic=="No"])
var.test(BMI[Diabetic=="Yes"],BMI[Diabetic=="No"])
var.test(DiabetesPedigreeFunction[Diabetic=="Yes"],DiabetesPedigreeFunction[Diabetic=="No"])
var.test(Age[Diabetic=="Yes"],Age[Diabetic=="No"])
var.test(BloodPressure[Diabetic=="Yes"],BloodPressure[Diabetic=="No"])

#OUR QUESTIONS :

# What exactly do we need to search for ?

# ANSWER : The database consists of various medical factors that would give us the outcome or the 
# result that if the female is diabetic or non-diabetic. Further exrtending them, models can be built 
# which will fit the data and will help us predict whether the females are diagnosed with diabetes 
# or not, this would be completely on the accuracy. 


# Are the measurements related ?

# ANSWER : We can understand the relation between the variables after performing the above tests,
# The better understanding and clearer picture was after having a look at the correlogram matrix
# So there are twom correlation coefficients, positive and negative, positive states that they are
# directly related and negative states that they are inversely related.


# Number of Pregnancies has an impact over diabetes outcome ?

diabetes <- read.csv("F:/Courses/MVA/diabetes.csv")
ggplot(diabetes,aes(x=Pregnancies,fill=factor(Diabetic)))+geom_bar(position="Dodge")+scale_fill_manual(values=c("yellow","green"))+scale_x_continuous(limits=c(0,16))+labs(title="Pregnancies v Outcome")


# ANSWER : The visualization is quite clear stating that it does not have an impact on the pregnancies.
