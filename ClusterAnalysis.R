diabetes <- read.csv("F:/Courses/MVA/diabetes.csv") 
str(diabetes) 
attach(diabetes) 
boxplot(Age) 
boxplot(BMI) 
hist(Age) 
library(ggplot2) 
ggplot(diabetes, aes(Age, fill=Outcome)) + geom_histogram() + facet_wrap(~BMI) +labs(title = "Age and BMI" ,x="Age", y="BMI") 

ggplot(diabetes, aes(Age, fill=Glucose)) + geom_histogram() + facet_wrap(~Outcome)+   labs(title = "Age and Diabetes" ,x="Age", y="Glucose") 

ggplot(diabetes, aes(Age,BMI,color=Outcome))  + geom_point() + 
  labs(title = "Relation Between Age and BMI: Diabetic or Not" ,x="Age", y="Age")	 

summary(BMI)

ggplot(diabetes, aes(Age,Glucose,color=Outcome))  + geom_point() 

#Clustering 

# Standardizing the data with scale() 
# Clustering

install.packages("cluster", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(cluster)
require(graphics)

# Hierarchical cluster analysis

# Standardizing the data with scale() function
matstd.diabetes = scale(diabetes[,2:9])

# Creating a (Euclidean) distance matrix of the standardized data
dist.diabetes = dist(matstd.diabetes, method = "euclidean")

# Invoking hclust i.e cluster analysis by single linkage method
clusdiabetes.nn = hclust(dist.diabetes, method = "single")


# Plotting
# Create extra margin room in the dendrogram
par(mar=c(8, 4, 4, 2) + 0.1)
# "clusdiabetes.nn" is converted into a object of class "dendrogram"
# In order to allow better flexibility in the (vertical) dendrogram plotting.
plot(as.dendrogram(clusdiabetes.nn),ylab="Glucose Content",ylim=c(0,100),
     main="Glucose within Body")

#Horizontal Dendrogram

dev.new()
par(mar=c(5, 4, 4, 7) +0.1)
plot(as.dendrogram(clusdiabetes.nn), xlab= "Glucose Content", xlim=c(100,0),
     horiz = TRUE,main="Glucose within Body")



#K-Means Clustering

install.packages("cluster", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(cluster)
require(graphics)

attach(diabetes)
# Standardizing the data with scale()
matstd.diabetes = scale(diabetes[,2:9])
# K-means, k=2, 3, 4, 5, 6
# Centers are numbers thus, 10 random sets are chosen

(kmeans2.diabetes = kmeans(matstd.diabetes,2,nstart = 10))
# Calculating the percentage of variation for 2 clusters
perc.var.2 = round(100*(1 - kmeans2.diabetes$betweenss/kmeans2.diabetes$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2

# Calculating the percentage of variation for 3 clusters
(kmeans3.diabetes = kmeans(matstd.diabetes,3,nstart = 10))
perc.var.3 = round(100*(1 - kmeans3.diabetes$betweenss/kmeans3.diabetes$totss),1)
names(perc.var.3) <- "Perc. 3 clus"
perc.var.3

# Calculating the percentage of variation for 4 clusters
(kmeans4.diabetes = kmeans(matstd.diabetes,4,nstart = 10))
perc.var.4 = round(100*(1 - kmeans4.diabetes$betweenss/kmeans4.diabetes$totss),1)
names(perc.var.4) <- "Perc. 4 clus"
perc.var.4

# Calculating the percentage of variation for 5 clusters
(kmeans5.diabetes = kmeans(matstd.diabetes,5,nstart = 10))
perc.var.5 = round(100*(1 - kmeans5.diabetes$betweenss/kmeans5.diabetes$totss),1)
names(perc.var.5) <- "Perc. 5 clus"
perc.var.5

# Calculating the percentage of variation for 6 clusters
(kmeans6.diabetes = kmeans(matstd.diabetes,6,nstart = 10))
perc.var.6 = round(100*(1 - kmeans6.diabetes$betweenss/kmeans6.diabetes$totss),1)
names(perc.var.6) <- "Perc. 6 clus"
perc.var.6

# Calculating the percentage of variation for 6 clusters
(kmeans7.diabetes = kmeans(matstd.diabetes,7,nstart = 10)) 
perc.var.7 = round(100*(1 - kmeans6.diabetes$betweenss/kmeans6.diabetes$totss),1)
names(perc.var.7) <- "Perc. 7 clus"
perc.var.7


k.max <- 15 # Maximal number of clusters 
wss <- sapply(1:k.max,function(k){kmeans(matstd.diabetes, k, nstart=50 )$tot.withinss}) 

plot(1:k.max, wss, 
     type="b", pch = 19, frame = FALSE,       xlab="Number of clusters K", 
     ylab="Total within-clusters sum of squares") 

abline(v = 3, lty =2)

(kmeans8.diabetes = kmeans(matstd.diabetes,8,nstart = 10)) 

plot(diabetes$Age, diabetes$SkinThickness,col=(kmeans8.diabetes$cluster+1), main="K-Means Clustering Results with K=8",  pch=20,cex=2) 

plot(diabetes$Age, diabetes$Insulin,col=(kmeans8.diabetes$cluster+1), main="K-Means Clustering Results with K=8",  pch=20,cex=2) 



feat.scaled <- scale(diabetes[,c("Age","Insulin")]) 
set.seed(15555) 
pclusters <- kmeans(feat.scaled, 4, nstart=20, iter.max=100) 

groups <- pclusters$cluster 
#clusterDF <- cbind(as.data.frame(feat.scaled), Cluster=as.factor(groups) 
plot(diabetes$Age, diabetes$Insulin, col=groups) 


pclusters <- kmeans(feat.scaled, 9, nstart=20, iter.max=100) 
groups <- pclusters$cluster 
#clusterDF <- cbind(as.data.frame(feat.scaled), Cluster=as.factor(groups) 
plot(diabetes$Age, diabetes$BMI, col=groups) 


