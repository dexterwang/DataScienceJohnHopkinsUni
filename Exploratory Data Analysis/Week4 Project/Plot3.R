setwd("C:/D/R/Exploratory Data Analysis/week4 project")

if (!require("ggplot2")) {
  install.packages("ggplot2")
}

if (!require("reshape")) {
  install.packages("reshape")
}

library(ggplot2)
library(reshape)


# read data from source file

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# get the subset of data for Baltimore City, Maryland (fips == "24510")

NEI_Baltimore <- NEI[NEI$fips=="24510",]

# calculate total emission by each year by each type

Yearly_Total_Emission <- with(NEI_Baltimore,tapply(Emissions,list(year,type),sum))

# construct a data frame to contain the yearly emission

df_YTE <- data.frame(Yearly_Total_Emission)

df_YTE$year <- row.names(Yearly_Total_Emission)

# tidy the data using reshape package
# it groups variables and transpose columns to rows
# alternatively "dplyr" and "tidyr" do the same thing 

df_YTE <- melt(df_YTE ,  id.vars = "year", variable.name = 'type')


# plot the yearly emission trends by type using ggplot2
# the graph shows PM2.5 emission from all the other source types decreased over the period
# of 1999 ~ 2008 except for type "POINT", which increased from 1999 to 2005 and dropped back
#in 2008
g<- ggplot(df_YTE,aes(year,value,group=1))+geom_line()+facet_grid(.~variable)
g+ labs(title="Total yearly PM2.5 emission in Baltimore City from different sources",x="Year",y="total emission (in tons)") 





# save screen output as png
dev.copy(png,"plot3.png", width=480, height=480)
dev.off()

