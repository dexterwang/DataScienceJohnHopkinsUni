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

# let's assume emissions with type "ON ROAD" are motor vehicle related
# get the subset of NEI which is motor vehicle related in Baltimore city and Los Angeles County


subNEI <- NEI[NEI$type == "ON-ROAD"& NEI$fips %in% c("24510","06037"),]

# calculate total emission by each year 

Yearly_Total_Emission <- with(subNEI,tapply(Emissions,list(year,fips),sum))

# construct a data frame to contain the yearly emission

df_YTE <- data.frame(Yearly_Total_Emission)

df_YTE$year <- row.names(Yearly_Total_Emission)


# tidy the data using reshape package

df_YTE <- melt(df_YTE ,  id.vars = "year", variable.name = 'fips')

df_YTE$city <- NA

df_YTE[df_YTE$variable=="X06037",]$city <- "Los Angeles"

df_YTE[df_YTE$variable=="X24510",]$city <- "Baltimore"


# plot the yearly emission trends for motor vehicle related sources using ggplot2
# Los Angeles city has greater changes in moter vehicle related PM2.5 emission 
# compared to that in Baltimore city
g<- ggplot(df_YTE,aes(year,value,group=1))+geom_line()+facet_grid(.~city)
g+ labs(title="Total yearly PM2.5 emission from motor vehicle related sources",x="Year",y="total emission (in tons)") 





# save screen output as png
dev.copy(png,"plot6.png", width=480, height=480)
dev.off()

