#2
data<-as.matrix(read.table("39542q2.txt",header=FALSE,sep=" "))
scaled.data = scale(data[,1:3], scale = T)
pairs(scaled.data)

set.seed(32145)
par(mfrow=c(2,1), mai = c(0.8, 0.8, 0.5, 0.3))
kmc = kmeans(scaled.data, 3, nstart = 20, algorithm ="Lloyd")
plot(scaled.data, col = kmc$cluster, main="")

maxk = 10
wssvec = numeric(maxk) 
for (k in 1:maxk) {
  kmcn = kmeans(scaled.data, k, nstart = 20, algorithm ="Lloyd")
  wssvec[k] = kmcn$tot.withinss 
}
plot(1:maxk, wssvec, type="b", xlab="No of clusters, K", ylab="Within Group SS", main="")

#4a
install.packages("copula")
library("copula")
par(mfrow = c(2,2), mai = c(0.8, 0.8, 0.5, 0.3))
theta = c(-700, -0.01, 0.01, 700)
n = 5000
for (i in theta) {
  cop = frankCopula(i)
  distr = mvdc(cop, margins = c("gamma","gamma"),
               paramMargins = list(list(2,2), list(2,2))
  )
  samples = rMvdc(n, distr)
  plot(samples ,xlab="x",ylab="y")
}




