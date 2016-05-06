setwd("C:/D/R/Exploratory Data Analysis/week4 project")

# read data from source file

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# get the subset of data for  Baltimore City, Maryland (fips == "24510")

NEI_Baltimore <- NEI[NEI$fips=="24510",]

# calculate total emission by each year

Yearly_Total_Emission <- with(NEI_Baltimore,tapply(Emissions,year,sum))

# plot the yearly PM2.5 emission 
# the graph shows the yearly total emission decreased between 1999 and 2002 from about 3200 tons to 2400 tons
# it bounced back in 2005 to about 3000 tons and significantly decreased to below 2000 tons in 2008

plot(names(Yearly_Total_Emission)
	 ,Yearly_Total_Emission
	 ,type="b"
	 ,xlim=c(1999,2008)
	 ,xlab="Year"
	 ,ylab="Total Emission (in tons)"
	 ,main = "Total yearly PM2.5 emission in Baltimore city, Maryland"
	 )

# save screen output as png
dev.copy(png,"plot2.png", width=480, height=480)
dev.off()

