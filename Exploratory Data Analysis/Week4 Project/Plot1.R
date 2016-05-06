setwd("C:/D/R/Exploratory Data Analysis/week4 project")

# read data from source file

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# calculate total emission by each year

Yearly_Total_Emission <- with(NEI,tapply(Emissions,year,sum))

# plot the yearly PM2.5 emission 
# the graph shows the yearly total emission decreased over the period of 1999 - 2008 

plot(names(Yearly_Total_Emission)
	 ,Yearly_Total_Emission/1000
	 ,type="b"
	 ,xlim=c(1999,2008)
	 ,xlab="Year"
	 ,ylab="Total Emission (in one thousand tons)"
	 ,main = "Total yearly PM2.5 emission from all sources"
	 )

# save screen output as png
dev.copy(png,"plot1.png", width=480, height=480)
dev.off

