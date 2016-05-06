setwd("C:/D/R/Exploratory Data Analysis/week4 project")

if (!require("ggplot2")) {
  install.packages("ggplot2")
}

library(ggplot2)


# read data from source file

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


# by searching Source Classification Code file,
# Short.Name (the third column) seems the best place to find targeted subset of data
# by matching "Motor Vehicle[s]" 
# there are 20 categories related 

unique(SCC[grep("\\<motor vehicle[s]*\\>", SCC[,3], ignore.case = TRUE) ,3])

#[1] Surface Coating /Motor Vehicles /Total: All Solvent Types                            Surface Coating /Motor Vehicles /Acetone                                            
# [3] Surface Coating /Motor Vehicles /Butyl Acetate                                       Surface Coating /Motor Vehicles /Butyl Alcohols: All Types                          
# [5] Surface Coating /Motor Vehicles /n-Butyl Alcohol                                     Surface Coating /Motor Vehicles /Isobutyl Alcohol                                   
# [7] Surface Coating /Motor Vehicles /Diethylene Glycol Monobutyl Ether                   Surface Coating /Motor Vehicles /Diethylene Glycol Monoethyl Ether                  
# [9] Surface Coating /Motor Vehicles /Diethylene Glycol Monomethyl Ether                  Surface Coating /Motor Vehicles /Ethyl Acetate                                      
#[11] Surface Coating /Motor Vehicles /Ethylene Glycol Monoethyl Ether (2-Ethoxyethanol)   Surface Coating /Motor Vehicles /Ethylene Glycol Monomethyl Ether (2-Methoxyethanol)
#[13] Surface Coating /Motor Vehicles /Ethylene Glycol Monobutyl Ether (2-Butoxyethanol)   Surface Coating /Motor Vehicles /Glycol Ethers: All Types                           
#[15] Surface Coating /Motor Vehicles /Isopropanol                                         Surface Coating /Motor Vehicles /Methyl Ethyl Ketone                                
#[17] Surface Coating /Motor Vehicles /Methyl Isobutyl Ketone                              Surface Coating /Motor Vehicles /Special Naphthas                                   
#[19] Surface Coating /Motor Vehicles /Solvents: NEC                                       Motor Vehicle Fires /Unspecified   

# get the SCC code for motor vehicle related sectors
subSCC <- SCC[grep("\\<motor vehicle[s]*\\>", SCC[,3], ignore.case = TRUE) ,1]

# get the subset of NEI which is motor vehicle related in Baltimore city 
subNEI <- NEI[NEI$SCC %in% subSCC & NEI$fips=="24510",]
# there are only two observations found

#          fips        SCC Pollutant Emissions     type year
#4888524  24510 2810050000  PM25-PRI     10.17 NONPOINT 2002
#11064649 24510 2810050000  PM25-PRI     10.17 NONPOINT 2005

# Seems like the method above is not quite approprate 
# So let's assume emissions with type "ON ROAD" are motor vehicle related

subNEI <- NEI[NEI$type == "ON-ROAD"& NEI$fips=="24510",]  


# calculate total emission by each year 

Yearly_Total_Emission <- with(subNEI,tapply(Emissions,year,sum))

# construct a data frame to contain the yearly emission

df_YTE <- data.frame(Yearly_Total_Emission)

df_YTE$year <- row.names(Yearly_Total_Emission)


# plot the yearly emission trends for motor vehicle related sources using ggplot2
# the emission decreased over the period of 1999 and 2008
g<- ggplot(df_YTE,aes(year,Yearly_Total_Emission))+geom_bar(stat="identity")
g+ labs(title="Total yearly PM2.5 emission from motor vehicle related sources in Baltimore City",x="Year",y="total emission (in tons)") 


# save screen output as png
dev.copy(png,"plot5.png", width=480, height=480)
dev.off()

