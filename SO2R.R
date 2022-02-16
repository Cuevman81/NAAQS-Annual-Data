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


#S02 Summary


SO2 <- aqs_annualData(aqs_user=myuser,
                      endpoint="byState",
                      state="28",
                      bdate="20200101",
                      edate="20201231",
                      param="42401")

dim(SO2)

write.csv(SO2, file='SO2.csv')

newdataSO21H <- select(filter(SO2, pollutant_standard == "SO2 1-hour 2010"),c(sample_duration,local_site_name,ninety_ninth_percentile,latitude,longitude))
write.csv(newdataSO21H, file="SO21HR.csv")

newdataSO23H <- select(filter(SO2, pollutant_standard == "SO2 3-hour 1971"),c(sample_duration,second_max_value,local_site_name,second_max_datetime,latitude,longitude))
write.csv(newdataSO23H, file="SO23HR.csv")

#1 Hour SO2
ggplot(newdataSO21H, aes(x = local_site_name , newdataSO21H, aes(x = local_site_name))) +
  geom_point(aes(y = ninety_ninth_percentile, color = "1-Hour"), size = 3) +
  xlab("Site")+ylab("SO21HR") +
  labs(title="1-Hour SO2")

#3 Hour SO2
ggplot(newdataSO23H, aes(x = local_site_name , newdataSO23H, aes(x = local_site_name))) +
  geom_point(aes(y = second_max_value, color = "3-Hour"), size = 3) +
  xlab("Site")+ylab("SO23HR") +
  labs(title="3-Hour SO2")


#Statewide Map of 1-HR SO2(PPB)
map(database = 'county','mississippi')
newdataSO21H <- read.csv("SO21HR.csv")
points(x = newdataSO21H$longitude, y = newdataSO21H$latitude, pch = 19, col = 'blue')
title(main = "1-HR SO2")
text(newdataSO21H$longitude, y = newdataSO21H$latitude, labels = newdataSO21H$ninety_ninth_percentile, pos = 2, font = 2, cex = 1.5)

#Statewide Map of 3-HR SO2(PPB)
map(database = 'county','mississippi')
newdataCO8H <- read.csv("SO23HR.csv")
points(x = newdataSO23H$longitude, y = newdataSO23H$latitude, pch = 19, col = 'blue')
title(main = "3-Hour SO2")
text(newdataSO23H$longitude, y = newdataSO23H$latitude, labels = newdataSO23H$second_max_value, pos = 2, font = 2, cex = 1.5)