diabetes = read.csv("F:/Courses/MVA/diabetes.csv")
str(diabetes)
attach(diabetes)

library(ggplot2)
ggplot(diabetes, aes(y= Age ,x=BMI, fill='')) + geom_boxplot()
ggplot(diabetes, aes(y=Age,x=BloodPressure, fill='')) + geom_boxplot()
ggplot(diabetes, aes(y=Age,x=Insulin, fill='')) + geom_boxplot()
ggplot(diabetes, aes(y=Insulin,x=BMI, fill='')) + geom_boxplot()

boxplot(Age ~ BMI)
unique(diabetes$default)
default_dummy=ifelse(diabetes$default=='yes',1,0) 

unique(diabetes$BMI)
BMI_dummy=ifelse(diabetes$BMI=='yes',1,0) 

unique(diabetes$Outcome)
Outcome_dummy=ifelse(diabetes$Outcome=='yes',1,0) 

unique(diabetes$Insulin)
Insulin_dummy=ifelse(diabetes$Insulin=='yes',1,0)

unique(diabetes$BloodPressure)
BloodPressure_dummy=ifelse(diabetes$BloodPressure=='yes',1,0)

diabetes1 =data.frame(Age,BMI,Outcome,Insulin,BloodPressure)

# Computing Correlation Matrix
corrm.z = cor(diabetes)
corrm.z
plot(corrm.z)
diabetes_pca <- prcomp(diabetes, scale=TRUE)
diabetes_pca
summary(diabetes_pca)

plot(diabetes_pca)
(eigen_diabetes <- diabetes_pca$sdev^2) 
names(eigen_diabetes) <- paste("PC",1:9,sep="")
eigen_diabetes
head(diabetes_pca$x)

dim(diabetes_pca$x)

plot(diabetes_pca)
summary(diabetes_pca)

plot(eigen_diabetes, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")


library(psych)
vss(diabetes1)
#therefore choosing 5 Factors 

#Oblique rotation 
fit.pc <- fa(diabetes1, nfactors=5, rotate="oblimin")

fit.pc

#what are factors for each variable , what is affectin and
#Fit based upon off diagonal values = 0.96 higher the better
#residual should be less
round(fit.pc$values, 3)
fit.pc$loadings
#View(fit.pc)
# Loadings with more digits
for (i in c(1,2,3,4,5)) { print(fit.pc$loadings[[1,i]])}
# Communalities
fit.pc$communality
# Rotated factor scores, Notice the columns ordering: RC1, RC3, RC2 and RC4
fit.pc$scores
# Play with FA utilities

fa.parallel(diabetes1) # See factor recommendation
fa.plot(fit.pc) # See Correlations within Factors
fa.diagram(fit.pc) # Visualize the relationship
