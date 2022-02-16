#Installing or importing all packages

library(expss)
library(VIF)
library(forecast)
library(Metrics)
library(DMwR2)
library(dplyr)
library(maps)
library(ggplot2)
library(aqsr)

myuser <- create_user(email="RCuevas@mdeq.ms.gov",
                      key="greyhawk63")

#CO Summary


CO <- aqs_annualData(aqs_user=myuser,
                     endpoint="byState",
                     state="28",
                     bdate="20210101",
                     edate="20211231",
                     param="42101")

dim(CO)

write.csv(CO, file='CO.csv')

newdataCO1H <- select(filter(CO, pollutant_standard == "CO 1-hour 1971"),c(sample_duration,second_max_value,local_site_name,second_max_datetime,latitude,longitude))
write.csv(newdataCO1H, file="CO1HR.csv")

newdataCO8H <- select(filter(CO, pollutant_standard == "CO 8-hour 1971"),c(sample_duration,second_max_value,local_site_name,second_max_datetime,latitude,longitude))
write.csv(newdataCO8H, file="CO8HR.csv")

#1 Hour CO
ggplot(newdataCO1H, aes(x = local_site_name , newdataCO1H, aes(x = local_site_name))) +
  geom_point(aes(y = second_max_value, color = "1-Hour"), size = 3) +
  xlab("Site")+ylab("CO1HR") +
  labs(title="1 and CO")

#8 Hour CO
ggplot(newdataCO8H, aes(x = local_site_name , newdataCO8H, aes(x = local_site_name))) +
  geom_point(aes(y = second_max_value, color = "8-Hour"), size = 3) +
  xlab("Site")+ylab("CO8HR") +
  labs(title="8 Hour CO")


#Statewide Map of 1-HR CO(PPB)
map(database = 'county','mississippi')
newdataCO1H <- read.csv("CO1HR.csv")
points(x = newdataCO1H$longitude, y = newdataCO1H$latitude, pch = 19, col = 'blue')
title(main = "1-HR CO")
text(newdataCO1H$longitude, y = newdataCO1H$latitude, labels = newdataCO1H$second_max_value, pos = 2, font = 2, cex = 1.5)

#Statewide Map of 8-HR CO(PPB)
map(database = 'county','mississippi')
newdataCO8H <- read.csv("CO8HR.csv")
points(x = newdataCO8H$longitude, y = newdataCO8H$latitude, pch = 19, col = 'blue')
title(main = "8-HR CO")
text(newdataCO8H$longitude, y = newdataCO8H$latitude, labels = newdataCO8H$second_max_value, pos = 2, font = 2, cex = 1.5)