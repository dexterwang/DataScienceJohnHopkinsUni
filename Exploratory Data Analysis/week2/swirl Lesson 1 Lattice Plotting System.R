library(swirl)
swirl()
head(airqulity)
head(airquality)
xyplot(Ozone~Wind,data=airquality)
xyplot(Ozone~Wind,data=airquality,col="red",pch=8,main="Big Apple Data")
xyplot(Ozone~Wind,data=airquality,pch=8,col="red",main="Big Apple Data")
xyplot(Ozone~Wind,da,pch=8,col="red",main="Big Apple Data")
xyplot(Ozone~Wind|as.factor(Month),data= airquality,layout = c(5,1))
xyplot(Ozone~Wind|Month,data= airquality,layout = c(5,1))
xyplot(Ozone~Wind,data=airquality)
p <- xyplot(Ozone~Wind,data=airquality)
print(p)
names(p)
mynames[myfull]
p[["formula"]]
p[["x.limits"]]
table(f)
xyplot(y~x|f,layout=c(2,1))
v1
v2
myedit("plot1.R")
source("plot.R",local=TRUE)
source(pathtofile("plot.R"),local=TRUE)
source(pathtofile("plot1.R"),local=TRUE)
myedit("plot2.R")
source(pathtofile("plot2.R"),local=TRUE)
str(diamonds)
table(diamonds$color)
table(diamonds$color,diamonds$cut)

myedit("myLabels,R")
myedit("myLabels,R")
myedit("myLabels.R")
source(pathtofile("myLabels.R"),local=TRUE)
xyplot(price~carat|color*cut,data=diamonds,strip=FALSE,pch=20,xlab=myxlab,ylab=myylab,main=mymain)

myedit("myLabels.R")
source(pathtofile("myLabels.R"),local=TRUE)
xyplot(price~carat|color*cut,data=diamonds,strip=FALSE,pch=20,xlab=myxlab,ylab=myylab,main=mymain)

xyplot(price~carat|color*cut,data=diamonds,pch=20,xlab=myxlab,ylab=myylab,main=mymain)
history(max.show=100)





p <- xyplot(y ~ x | f, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)  ## First call the default panel function for 'xyplot'
  panel.abline(h = median(y), lty = 2)  ## Add a horizontal line at the median
})
print(p)
invisible()



p2 <- xyplot(y ~ x | f, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)  ## First call default panel function
  panel.lmline(x, y, col = 2)  ## Overlay a simple linear regression line
})
print(p2)
invisible()


myxlab <- "Carat"
myylab <- "Price"
mymain <- "Diamonds are Sparkly!"

