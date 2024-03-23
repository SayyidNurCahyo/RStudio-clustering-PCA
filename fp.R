library(cluster) #Clustering
library(factoextra) #Clustering & Viz
library(tidyverse) #Data Manipulation
library(dplyr)
library(fpc) #Clustering
library(ggiraphExtra) #Radar plot
library(clValid) #Choose c
library(readxl)
library(PerformanceAnalytics)
library(mvnormtest)
library(psych)

icdrate = function(Data, nc, c)
{
  n = dim(Data)[1]
  p = dim(Data)[2]
  X = Data[,1:(p-1)]
  Group = Data[,p]
  p = dim(X)[2]
  Mean.X = matrix(ncol = p, nrow = (nc+1))
  for (i in 1:nc)
  {
    for (j in 1:p)
    {
      Mean.X[i,j] = mean(X[which(Group==i),j])
      Mean.X[(nc+1),j] = mean(X[,j])
    }
  }
  SST = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      SST[i,j] = (X[i,j] - Mean.X[(nc+1),j])^2
    }
  }
  SST = sum(sum(SST))
  SSE = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      for (k in 1:nc)
      {
        if (Group[i]==k)
        {
          SSE[i,j] = (X[i,j] - Mean.X[k,j])^2
        }
      }
    }
  }
  SSE = sum(sum(SSE))
  Rsq = (SST-SSE)/SST
  icdrate = 1-Rsq
  Pseudof = (Rsq/(c-1))/((icdrate)/(nc-c))
  list(Rsq=Rsq, icdrate=icdrate, pseudof=Pseudof)
}

#Data Preparation
data=read_excel("C:/Users/LENOVO/OneDrive/Dokumen/andat/B1.xlsx")
dataclus=subset(data,select=-c(id))
boxplot(dataclus)

#uji asumsi
dft=t(dataclus)
mshapiro.test(dft)
bartlett.test(list(data$Usia,data$`Jenis Kelamin`,data$Portofolio,data$`Pengalaman Kerja`,data$`Pendidikan Terakhir`,data$`IPK/Nilai Akhir`,data$`Jurusan/Bidang Keahlian`,data$`Pengalaman Organisasi/Event`,data$Sertifikasi,data$Projek,data$Bahasa,data$Prestasi,data$Lokasi))
chart.Correlation(dataclus)
r=cor(dataclus)
cortest.bartlett(r,n=nrow(dataclus))
KMO(r)
#PCA
df.pr=prcomp(dataclus, center=TRUE, scale=TRUE)
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
plot(cumpro[0:10], xlab="PC#", ylab="Amount of explained variance", main="Cumulative Variance Plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.6766717, col="blue", lty=5)
legend("bottomright", legend=c("Cut-off @PC2"), col=c("blue"), lty=5, cex=0.6)

#PC Eq
round(df.pr$rotation[,1:6],3)


#Clustering
df=dataclus
df.pc=df.pr$x[,1:6]
View(df.pc)
rpc=cor(df.pc[,1:6])
cortest.bartlett(rpc,n=nrow(df.pc[,1:6]))

#Choose Optimum k for K-Means
dataclus=df.pc
fviz_nbclust(dataclus, kmeans, method="wss") #Elbow Method
fviz_nbclust(dataclus, kmeans, method="silhouette") #Silhouette Method
intern=clValid(dataclus, nClust = 2:10,clMethods = c("kmeans"), validation = "internal")
summary(intern)

#Run K-Means
km_fit = kmeans(dataclus,centers=2)
print(km_fit)
df.clus=data.frame(dataclus,km_fit$cluster) #Adding Cluster to DF
View(df.clus)
win.graph()
fviz_cluster(km_fit,data=dataclus) #Cluster Plot
km_fit$centers #Represented object from each clusters

#icdrate dan pseudo F
km_fit2 = kmeans(dataclus,centers = 2)
new_data <- cbind(dataclus,data.frame(km_fit2$cluster))
new_data$km_fit2.cluster <- as.factor(new_data$km_fit2.cluster)
icdrate(new_data,length(new_data),2)
km_fit3 = kmeans(dataclus,centers = 3)
new_data <- cbind(dataclus,data.frame(km_fit3$cluster))
new_data$km_fit3.cluster <- as.factor(new_data$km_fit3.cluster)
icdrate(new_data,length(new_data),3)
km_fit4 = kmeans(dataclus,centers = 4)
new_data <- cbind(dataclus,data.frame(km_fit4$cluster))
new_data$km_fit4.cluster <- as.factor(new_data$km_fit4.cluster)
icdrate(new_data,length(new_data),4)

t(apply(km_fit$centers, 1, function(r)r*attr(dataclus,'scaled:scale') + attr(dataclus, 'scaled:center')))




