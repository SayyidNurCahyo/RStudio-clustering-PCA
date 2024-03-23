library(psych)
library(ggplot2)
library(PerformanceAnalytics)
library(dplyr)
library(mvnormtest)
library(cluster) #Clustering
library(factoextra) #Clustering & Viz
library(tidyverse) #Data Manipulation
library(corrplot)
library(fpc) #Clustering
library(clValid) #Choose c
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(stats)
library(readxl)
#Data Preparation
df=read_excel("C:/Users/LENOVO/OneDrive/Dokumen/andat/data_kemkes.xlsx")
rownames(df)=df$Provinsi
df=df[,2:8]
head(df)
summary(df)

#Multivariate Normality
dft=t(df)
mshapiro.test(dft)

#Bartlett Test (Homogeneity)
bartlett.test(df)

#Correlation/Multicolinearity
r=cor(df)
r
corrplot(r)
chart.Correlation(df)
cortest.bartlett(r,n=nrow(df))

#KMO Test
KMO(r) #overall>0.5, stop if <0.5; MSA>0.5, drop if <0.5 (1 by 1)

#PCA
df.pr=prcomp(df, center=TRUE, scale=TRUE)
df.pr
summary(df.pr)


#Eigen Value
round(df.pr$sdev^2,2) 
screeplot(df.pr, type = "l", npcs=7, main="Screeplot")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"), col=c("red"), lty=5, cex=0.6)

#Variance Explained
exp_var=df.pr$sdev^2/sum(df.pr$sdev^2)
exp_var
fviz_eig(df.pr, addlabels=TRUE, ylim=c(0,60))

#Cumulative Variance Plot
cumpro=cumsum(df.pr$sdev^2/sum(df.pr$sdev^2))
cumpro
plot(cumpro[0:7], xlab="PC#", ylab="Amount of explained variance", main="Cumulative Variance Plot")
abline(v = 2, col="blue", lty=5)
abline(h = 0.6653290, col="blue", lty=5)
legend("bottomright", legend=c("Cut-off @PC2"), col=c("blue"), lty=5, cex=0.6)

#PC Eq
round(df.pr$rotation[,1:2],3)






#New df
df=read_excel("C:/Users/LENOVO/OneDrive/Dokumen/andat/data_kemkes.xlsx")
df.pc=data.frame(df$Provinsi,df.pr$x[,1:2])
rownames(df.pc)=df.pc$df.Provinsi
head(df.pc)
rpc=cor(df.pc[,2:3])
cortest.bartlett(rpc,n=nrow(df.pc[,2:3]))

#Heat Map
fviz_pca_var(df.pr,col.var="coord",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=T, axes=c(2,3))

#Clustering K-Means
dataclus=df.pc[,-1]
fviz_nbclust(dataclus, kmeans, method="wss") #Elbow Method
fviz_nbclust(dataclus, kmeans, method="silhouette") #Silhouette Method
intern=clValid(dataclus, nClust = 2:5,clMethods = c("hierarchical","kmeans","pam"), validation = "internal")
summary(intern)
km_fit = kmeans(dataclus,centers=2)
print(km_fit)
df.clus=data.frame(dataclus,km_fit$cluster) #Adding Cluster to DF
fviz_cluster(km_fit,data=dataclus) #Cluster Plot
table(km_fit$cluster) #Number of members in each clusters
km_fit$centers #Represented object from each clusters
df.clus %>%
  mutate(cluster=km_fit.cluster) %>%
  group_by(cluster) %>%
  summarise_all("mean") #Desc of each clusters







df=read_excel("C:/Users/LENOVO/OneDrive/Dokumen/andat/data_kemkes3.xlsx")
rownames(df)=df$Provinsi
df=df[,2:8]
head(df)
summary(df)

#Multivariate Normality
dft=t(df)
mshapiro.test(dft)

#Bartlett Test (Homogeneity)
bartlett.test(df)

#Correlation/Multicolinearity
r=cor(df)
r
corrplot(r)
chart.Correlation(df)
cortest.bartlett(r,n=nrow(df))

#KMO Test
KMO(r) #overall>0.5, stop if <0.5; MSA>0.5, drop if <0.5 (1 by 1)

#PCA
df.pr=prcomp(df, center=TRUE, scale=TRUE)
df.pr
summary(df.pr)


#Eigen Value
round(df.pr$sdev^2,2) 
screeplot(df.pr, type = "l", npcs=7, main="Screeplot")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"), col=c("red"), lty=5, cex=0.6)

#Variance Explained
exp_var=df.pr$sdev^2/sum(df.pr$sdev^2)
exp_var
fviz_eig(df.pr, addlabels=TRUE, ylim=c(0,60))

#Cumulative Variance Plot
cumpro=cumsum(df.pr$sdev^2/sum(df.pr$sdev^2))
cumpro
plot(cumpro[0:7], xlab="PC#", ylab="Amount of explained variance", main="Cumulative Variance Plot")
abline(v = 2, col="blue", lty=5)
abline(h = 0.6653290, col="blue", lty=5)
legend("bottomright", legend=c("Cut-off @PC2"), col=c("blue"), lty=5, cex=0.6)

#PC Eq
round(df.pr$rotation[,1:3],3)






#New df
df=read_excel("C:/Users/LENOVO/OneDrive/Dokumen/andat/data_kemkes3.xlsx")
df.pc=data.frame(df$Provinsi,df.pr$x[,1:3])
rownames(df.pc)=df.pc$df.Provinsi
head(df.pc)
rpc=cor(df.pc[,2:4])
cortest.bartlett(rpc,n=nrow(df.pc[,2:4]))

#Heat Map
fviz_pca_var(df.pr,col.var="coord",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=T, axes=c(2,4))

#Clustering K-Means
dataclus=df.pc[,-1]
fviz_nbclust(dataclus, kmeans, method="wss") #Elbow Method
fviz_nbclust(dataclus, kmeans, method="silhouette") #Silhouette Method
intern=clValid(dataclus, nClust = 2:5,clMethods = c("hierarchical","kmeans","pam"), validation = "internal")
summary(intern)
km_fit = kmeans(dataclus,centers=4)
print(km_fit)
df.clus=data.frame(dataclus,km_fit$cluster) #Adding Cluster to DF
fviz_cluster(km_fit,data=dataclus) #Cluster Plot
table(km_fit$cluster) #Number of members in each clusters
km_fit$centers #Represented object from each clusters
df.clus %>%
  mutate(cluster=km_fit.cluster) %>%
  group_by(cluster) %>%
  summarise_all("mean") #Desc of each clusters
