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

myuser <- create_user(email="",
                      key="")


#S02 Summary


SO2 <- aqs_annualData(aqs_user=myuser,
                      endpoint="byState",
                      state="28",
                      bdate="20210101",
                      edate="20211231",
                      param="42401")

dim(SO2)

write.csv(SO2, file='SO2.csv')

SO21H <- select(filter(SO2, pollutant_standard == "SO2 1-hour 2010"),c(sample_duration,local_site_name,ninety_ninth_percentile,latitude,longitude))
write.csv(SO21H, file="SO21HR.csv")

SO23H <- select(filter(SO2, pollutant_standard == "SO2 3-hour 1971"),c(sample_duration,second_max_value,local_site_name,second_max_datetime,latitude,longitude))
write.csv(SO23H, file="SO23HR.csv")

#1 Hour SO2
ggplot(SO21H, aes(x = local_site_name , SO21H, aes(x = local_site_name))) +
  geom_point(aes(y = ninety_ninth_percentile, color = "1-Hour"), size = 3) +
  xlab("Site")+ylab("SO21HR") +
  labs(title="1-Hour SO2")

#3 Hour SO2
ggplot(SO23H, aes(x = local_site_name , SO23H, aes(x = local_site_name))) +
  geom_point(aes(y = second_max_value, color = "3-Hour"), size = 3) +
  xlab("Site")+ylab("SO23HR") +
  labs(title="3-Hour SO2")


#Statewide Map of 1-HR SO2(PPB)
map(database = 'county','mississippi')
SO21H <- read.csv("SO21HR.csv")
points(x = SO21H$longitude, y = SO21H$latitude, pch = 19, col = 'blue')
title(main = "1-HR SO2")
text(SO21H$longitude, y = SO21H$latitude, labels = SO21H$ninety_ninth_percentile, pos = 2, font = 2, cex = 1.5)

#Statewide Map of 3-HR SO2(PPB)
map(database = 'county','mississippi')
SO23H <- read.csv("SO23HR.csv")
points(x = SO23H$longitude, y = SO23H$latitude, pch = 19, col = 'blue')
title(main = "3-Hour SO2")
text(SO23H$longitude, y = SO23H$latitude, labels = SO23H$second_max_value, pos = 2, font = 2, cex = 1.5)
