# load required libraries

if (!require("ggplot2")) {
  install.packages("ggplot2")
}

library(ggplot2)


if (!require("grid")) {
  install.packages("grid")
}

library(grid)


if (!require("dplyr")) {
  install.packages("dplyr")
}

library(dplyr)



# assume StormData.csv has been downloaded and unzipped under the working directory

# the data can be downloaded from 

# https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2

setwd("C:/D/R/Reproducible Research/Week4 Project")

destfile <- "./StormData.csv"

fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipfile <- "StormData.csv.bz"

if(!file.exists(destfile)){
    res <- tryCatch(download.file(fileURL,
                              destfile=zipfile,
                              method="auto"),
                error=function(e) 1)
}

# read.csv can directly read compressed bz file containing csv file.
data <- read.csv(zipfile,header=TRUE,sep=",")

str(data)

subdata<- data[,c("EVTYPE","INJURIES","FATALITIES")]

# check if there is missing data in each columns
subdata[is.na(subdata$FATALITIES)|is.na(subdata$INJURIES)|is.na(subdata$EVTYPE),]

health_impact_sum <- subdata %>% group_by(EVTYPE) %>% mutate(total_impact = INJURIES+FATALITIES) %>% summarise_each(funs(sum)) %>% arrange(desc(total_impact),desc(INJURIES),desc(FATALITIES))

health_impact_mean <- subdata %>% group_by(EVTYPE) %>% mutate(total_impact = INJURIES+FATALITIES) %>% summarise_each(funs(mean)) %>% arrange(desc(total_impact),desc(INJURIES),desc(FATALITIES))


# only select the top 10 events by sum/mean of total number of injuries and fatalities
health_impact_sum <- health_impact_sum[1:10,]

health_impact_mean <- health_impact_mean[1:10,]

health_impact_sum

health_impact_mean


#prepare the plot, reorder the factor coloumn by total_impact value
health_impact_sum <- transform(health_impact_sum, EVTYPE=reorder(EVTYPE, -total_impact) )

health_impact_mean <- transform(health_impact_mean, EVTYPE=reorder(EVTYPE, -total_impact) )

health_impact_sum <- health_impact_sum %>% gather(impact_name,count,-EVTYPE,-total_impact)

health_impact_mean <- health_impact_mean %>% gather(impact_name,count,-EVTYPE,-total_impact)


g<-ggplot(health_impact,aes(x=EVTYPE,y=count,fill=impact_name))+geom_bar(stat="identity") +coord_flip() 

g<- g+labs(title="Top 10 events which are most harmful to population health",y="Count",x="Event Name")+ theme(legend.position="right",
       legend.title=element_blank(),plot.margin = unit(c(1,2,4,1),"lines"))

g<-g+ annotation_custom(grob = textGrob("The number of injuries and fatalities caused by tornado \nis significantly higher than that of the other top 10 events"),  
        xmin = -1.5, xmax = -1, ymin = 45000, ymax = 50000)

gt <- ggplot_gtable(ggplot_build(g))
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)


tornado_impact <- subdata %>% filter(EVTYPE=="TORNADO") %>% mutate(total_impact = INJURIES+FATALITIES) %>% arrange(desc(total_impact))

head(tornado_impact)
summary(tornado_impact)

hist(tornado_impact$total_impact) 


subdata2<- data[,c("EVTYPE","PROPDMG","CROPDMG")]

# check if there is missing data in each columns
subdata2[is.na(subdata2$PROPDMG)|is.na(subdata2$CROPDMG)|is.na(subdata2$EVTYPE),]

names(subdata2) <- c("Event_Type","Property_Damage","Crop_Damage")

economic_impact_sum <- subdata2 %>% group_by(Event_Type) %>% mutate(total_damage = Property_Damage+Crop_Damage) %>% summarise_each(funs(sum)) %>% arrange(desc(total_damage),desc(Property_Damage),desc(Crop_Damage))

economic_impact_mean <- subdata2 %>% group_by(Event_Type) %>% mutate(total_damage = Property_Damage+Crop_Damage) %>% summarise_each(funs(mean)) %>% arrange(desc(total_damage),desc(Property_Damage),desc(Crop_Damage))


# only select the top 10 events by sum of total number of Property_Damage and Crop_Damage
economic_impact_sum <- economic_impact_sum[1:10,]

economic_impact_mean <- economic_impact_mean[1:10,]

economic_impact_sum 

economic_impact_mean

#prepare the plot, reorder the factor coloumn by total_damage value
economic_impact_sum <- transform(economic_impact_sum, Event_Type=reorder(Event_Type, -total_damage) )

economic_impact_mean <- transform(economic_impact_mean, Event_Type=reorder(Event_Type, -total_damage) )

economic_impact_sum <- economic_impact_sum %>% gather(impact_name,total,-Event_Type,-total_damage)

economic_impact_mean <- economic_impact_mean %>% gather(impact_name,total,-Event_Type,-total_damage)



g1<-ggplot(economic_impact_sum,aes(x=Event_Type,y=total/1000,fill=impact_name))+geom_bar(stat="identity") +coord_flip() 

g1<- g1+labs(title="Top 10 events with highest total damage",y="Sum of Damage (in million)",x="Event Name")+ theme(legend.position="bottom",
       legend.title=element_blank() ) #,plot.margin = unit(c(1,2,4,1),"lines"))


g2<-ggplot(economic_impact_mean,aes(x=Event_Type,y=total,fill=impact_name))+geom_bar(stat="identity") +coord_flip() 

g2<- g2+labs(title="Top 10 events with highest mean damage",y="Sum of Damage (in thousand)",x="")+ theme(legend.position="bottom",
       legend.title=element_blank() ) #,plot.margin = unit(c(1,2,4,1),"lines"))



pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(1, 4, 1), "null"))))
grid.text("title of this panel", vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
print(g1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(g2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))





#other people's work

http://rpubs.com/NicZ8/180408

http://rpubs.com/ramneek/research

#g<-g+ annotation_custom(grob = textGrob("The number of injuries and fatalities caused by tornado \nis significantly higher than that of the other top 10 events"),  
        xmin = -1.2, xmax = -1, ymin = 45000, ymax = 50000)

gt <- ggplot_gtable(ggplot_build(g))
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)



date <- as.Date(data$BGN_DATE,"%m/%d/%Y")
