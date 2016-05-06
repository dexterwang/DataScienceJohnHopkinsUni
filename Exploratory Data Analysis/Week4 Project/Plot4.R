setwd("C:/D/R/Exploratory Data Analysis/week4 project")

if (!require("ggplot2")) {
  install.packages("ggplot2")
}

library(ggplot2)


# read data from source file

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


# by searching Source Classification Code file,
# there are three sectors related to coal combustion 
# in EI.Sector (fourth column)

unique(SCC[grep("\\<coal\\>", SCC[,4], ignore.case = TRUE) ,4])

# [1] Fuel Comb - Electric Generation - Coal    
#     Fuel Comb - Industrial Boilers, ICEs - Coal 
#     Fuel Comb - Comm/Institutional - Coal   

# get the SCC code for coal combustion related sectors
subSCC <- SCC[grep("\\<coal\\>", SCC[,4], ignore.case = TRUE) ,1]

# get the subset of NEI which is coal combusition related 
subNEI <- NEI[NEI$SCC %in% subSCC,]


# calculate total emission by each year 

Yearly_Total_Emission <- with(subNEI,tapply(Emissions,year,sum))

# construct a data frame to contain the yearly emission

df_YTE <- data.frame(Yearly_Total_Emission)

df_YTE$year <- row.names(Yearly_Total_Emission)


# plot the yearly emission trends for coal combusition related sources using ggplot2
# the graph shows PM2.5 emission decreased to around 550,000 tons between 1999 and 2005 
# before a significant drop to under 350,000 tons til 2008 
g<- ggplot(df_YTE,aes(year,Yearly_Total_Emission/1000,group=1))+geom_line()
g+ labs(title="Total yearly PM2.5 emission from coal combusition related sources",x="Year",y="total emission (in one thousand tons)") 





# save screen output as png
dev.copy(png,"plot4.png", width=480, height=480)
dev.off()

