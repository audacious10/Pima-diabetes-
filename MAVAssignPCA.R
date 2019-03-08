#Reading the dataset
diabetes <- read.csv("F:/Courses/MVA/diabetes.csv")

#Lets have 0/1 as No/Yes

diabetes$Outcome <- as.factor(diabetes$Outcome)
levels(diabetes$Outcome) <- c("No","Yes")

colnames(diabetes)[colnames(diabetes)=="Outcome"] <- "Diabetic"

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
library(reshape2)

dim(diabetes)
attach(diabetes)
head(diabetes)

#Get the Correlations between the measurements
cor(diabetes[-9])

#Use of prcomp to compute the principal components(eigenvalues & eigenvectors). 
#With scale=TRUE, variable means are set to zero, and variances set to one
diabetes_pca = prcomp(diabetes[-9],scale=TRUE)
diabetes_pca
summary(diabetes_pca)


# sample scores stored in diabetes_pca$x
# singular values (square roots of eigenvalues) stored in diabetes_pca$sdev
# loadings (eigenvectors) are stored in diabetes_pca$rotation
# variable means stored in diabetes_pca$center
# variable standard deviations stored in diabetes_pca$scale
# A table containing eigenvalues and %'s accounted, follows
# Eigenvalues are sdev^2

(eigen_diabetes = diabetes_pca$sdev^2)
names(eigen_diabetes) = paste("PC",1:5,sep="")
eigen_diabetes
sumlambdas = sum(eigen_diabetes)
sumlambdas
propvar = eigen_diabetes/sumlambdas
propvar
cumvar_diabetes = cumsum(propvar)
cumvar_diabetes
matlambdas = rbind(eigen_diabetes,propvar,cumvar_diabetes)

rownames(matlambdas) = c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)
summary(diabetes_pca)
diabetes_pca$rotation
print(diabetes_pca)
# Sample scores stored in diabetes_pca$x
diabetes_pca$x
# Identifying the scores by their diabetic status
diabtyp_pca = cbind(data.frame(Diabetic),diabetes_pca$x)
diabtyp_pca
# Means of scores for all the PC's classified by Survival status
tabmeansPC = aggregate(diabtyp_pca[,2:6],by=list(Diabetic=diabetes$Diabetic),mean)
tabmeansPC
tabmeansPC = tabmeansPC[rev(order(tabmeansPC$Diabetic)),]
tabmeansPC
tabfmeans = t(tabmeansPC[,-1])
tabfmeans
colnames(tabfmeans) = t(as.vector(tabmeansPC[1]))
tabfmeans

# Standard deviations of scores for all the PC's classified by Diabetic status
tabsdsPC = aggregate(diabtyp_pca[,2:6],by=list(Diabetic=diabetes$Diabetic),sd)
tabfsds = t(tabsdsPC[,-1])
colnames(tabfsds) = t(as.vector(tabsdsPC[1]))
tabfsds
t.test(PC1~diabetes$Diabetic,data=diabtyp_pca)
t.test(PC2~diabetes$Diabetic,data=diabtyp_pca)
t.test(PC3~diabetes$Diabetic,data=diabtyp_pca)
t.test(PC4~diabetes$Diabetic,data=diabtyp_pca)
t.test(PC5~diabetes$Diabetic,data=diabtyp_pca)

# F ratio tests
var.test(PC1~diabetes$Diabetic,data=diabtyp_pca)
var.test(PC2~diabetes$Diabetic,data=diabtyp_pca)
var.test(PC3~diabetes$Diabetic,data=diabtyp_pca)
var.test(PC4~diabetes$Diabetic,data=diabtyp_pca)
var.test(PC5~diabetes$Diabetic,data=diabtyp_pca)

# Levene's tests (one-sided)
library(car)
(LTPC1 = leveneTest(PC1~diabetes$Diabetic,data=diabtyp_pca))

(LTPC1 = leveneTest(PC1~diabetes$Diabetic,data=diabtyp_pca))
(p_PC1_1sided = LTPC1[[3]][1]/2)
(LTPC2 = leveneTest(PC2~diabetes$Diabetic,data=diabtyp_pca))
(p_PC2_1sided = LTPC2[[3]][1]/2)
(LTPC3 = leveneTest(PC3~diabetes$Diabetic,data=diabtyp_pca))
(p_PC3_1sided = LTPC3[[3]][1]/2)
(LTPC4 = leveneTest(PC4~diabetes$Diabetic,data=diabtyp_pca))
(p_PC4_1sided = LTPC4[[3]][1]/2)
(LTPC5 = leveneTest(PC5~diabetes$Diabetic,data=diabtyp_pca))
(p_PC5_1sided = LTPC5[[3]][1]/2)


# Plotting the scores for the first and second components
plot(diabtyp_pca$PC1, diabtyp_pca$PC2,pch=ifelse(diabtyp_pca$Diabetic == "S",1,16),xlab="PC1", ylab="PC2", main="")
abline(h=0)
abline(v=0)
legend("bottomleft", legend=c("Diabetic","Non-diabetic"), pch=c(1,16))
plot(eigen_diabetes, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(eigen_diabetes), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log (eigenvalue) diagram")
print(summary(diabetes_pca))
View(diabetes_pca)
diag(cov(diabetes_pca$x))
xlim = range(diabetes_pca$x[,1])
diabetes_pca$x[,1]
diabetes_pca$x
plot(diabetes_pca$x,xlim=xlim,ylim=xlim)
diabetes_pca$rotation[,1]
diabetes_pca$rotation
plot(diabetes[,-1])
diabetes_pca$x
plot(diabetes_pca)

#get the original value of the data based on PCA
center = diabetes_pca$center
scale = diabetes_pca$scale
new_diabetes = as.matrix(diabetes[,-1])
new_diabetes
#The aboved two gives us the same thing. predict is a good function to know.
out = sapply(1:5, function(i){plot(diabetes$Diabetic,diabetes_pca$x[,i],xlab=paste("PC",i,sep=""),ylab="Diabetic")})
pairs(diabetes_pca$x[,1:5], ylim = c(-6,4),xlim = c(-6,4),panel=function(x,y,...){text(x,y,diabetes$Diabetic)})



