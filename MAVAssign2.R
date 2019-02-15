#Reading the dataset
diabetes <- read.csv("F:/Courses/MVA/diabetes.csv")
diabetes

#Inspecting the dataset 
summary(diabetes)

#Renaming column name 
#Outcome doesnt really give the impact, so making it as 'Diabetic ?'
colnames(diabetes)[9] <- "Diabetic"

#Lets have 0/1 as No/Yes

diabetes$Diabetic <- as.factor(diabetes$Diabetic)

levels(diabetes$Diabetic) <- c("No","Yes")

library(PerformanceAnalytics)

library(ggplot2)

library(GGally)

library(Amelia)

library(ggcorrplot)

#stripchart is like scatter plots (or dot plots) of the given data. 
#It's like an alternative to boxplots when sample sizes are small.
stripchart(diabetes$BloodPressure,
           main="Blood pressure levels",
           xlab="Pressure levels",
           ylab="",
           method="jitter",
           col="orange",
           pch=1)


#Visualizing correlation matrix 
diabetes %>%
  plot_ly(y = ~Pregnancies, color = ~BMI, type = "box")

#ggcor used for pairwise correlation matrix plot
ggcorr(diabetes[,-9], name = "corr", label = TRUE)+
  
  theme(legend.position="none")+
  
  labs(title="Correlation Plot of Variance")+
  
  theme(plot.title=element_text(face='italic',color='red',hjust=0.5,size=12))

#missmap
missmap(diabetes, main ="Missing values vs observed")
#comparison of missing values vs observed 

# Correlation matrix
data(diabetes)
corr <- round(cor(diabetes), 1)
# Plotting
ggcorrplot (corr, hc.order = TRUE, 
           type = "upper", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("papayawhip", "lightsteelblue1", "olivedrab4"), 
           title="Correlogram", 
           ggtheme=theme_bw)
#Output : 
#Our observation is that the variable Glucose has a higher impact on the Outcome variable. 
#Also they are highly Co-rrelated. 
#Pregnancies and Age are strongly correlated with coeeficient value 0.54

attach(diabetes)
plot(DiabetesPedigreeFunction, Outcome, main="Scatterplot For Diabetes", 
     xlab="DegreeFunction ", ylab="Outcome", pch=20)
