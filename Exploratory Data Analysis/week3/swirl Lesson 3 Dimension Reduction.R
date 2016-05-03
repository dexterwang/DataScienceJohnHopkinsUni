head(dataMatrix)
heatmap(dataMatrix)
myedit("addPatt.R")
source("addPatt.R",local=TRUE)
heatmap(dataMatrix)
mat
svd(mat)
matu%*%diag%*%matv
matu %*% diag %*% matv
sdfa
a
matu %*% diag %*% t(matv)
svd(scale(mat))
prcomp(scale(mat))
svd1 <- svd(dataMatrix)
svd1$v[,1]
svd1$d
head(constantMatrix)
svd2
svd2$d
svd2
svd2$v[,1:2]
svd2
svd2$d
dim(faceData)
asdfjdasiof
a
a1 <- (svd1$u[,1] * svd1$d[1]) %*% t(svd1$v[,1])
myImage(a1)
a2 <- (svd1$u[,1:2] * svd1$d[1:2]) %*% t(svd1$v[,1:2])
a2 <- (svd1$u[,1:2] * diag(svd1$d[1:2])) %*% t(svd1$v[,1:2])
a2 <- (svd1$u[,1:2] * diag(svd1$d[1:2]) %*% t(svd1$v[,1:2])
)
a2 <- svd1$u[,1:2] * diag(svd1$d[1:2]) %*% t(svd1$v[,1:2])
a2 <- svd1$u[,1:2] %*% diag(svd1$d[1:2]) %*% t(svd1$v[,1:2])
myImage(a2)
a2 <- svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5])
myImage( svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5]))
myImage( svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10]))
history(max.show=50)






set.seed(678910)
for(i in 1:40){
  # flip a coin
  coinFlip <- rbinom(1,size=1,prob=0.5)
  # if coin is heads add a common pattern to that row
  if(coinFlip){
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3),each=5)
  }
}