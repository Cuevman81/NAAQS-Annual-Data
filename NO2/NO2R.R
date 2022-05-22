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


#NO2 Summary


NO2 <- aqs_annualData(aqs_user=myuser,
                     endpoint="byState",
                     state="28",
                     bdate="20200101",
                     edate="20201231",
                     param="42602")

dim(NO2)

write.csv(NO2, file='NO2.csv')

NO21H <- select(filter(NO2, pollutant_standard == "NO2 1-hour 2010"),c(sample_duration,ninety_eighth_percentile,local_site_name,second_max_datetime,latitude,longitude))
write.csv(NO21H, file="NO21HR.csv")

NO2Annual <- select(filter(NO2, pollutant_standard == "NO2 Annual 1971"),c(sample_duration,arithmetic_mean,local_site_name,second_max_datetime,latitude,longitude))
write.csv(NO2Annual, file="NO2Annual.csv")

#1 Hour NO2
ggplot(NO21H, aes(x = local_site_name , NO21H, aes(x = local_site_name))) +
  geom_point(aes(y = ninety_eighth_percentile, color = "1-Hour"), size = 3) +
  xlab("Site")+ylab("NO21HR") +
  labs(title="1 Hour NO2")

#8 Annual NO2
ggplot(NO2Annual, aes(x = local_site_name , NO2Annual, aes(x = local_site_name))) +
  geom_point(aes(y = arithmetic_mean, color = "Annual"), size = 3) +
  xlab("Site")+ylab("NO2Annual") +
  labs(title="Annual NO2")


#Statewide Map of 1-HR NO2(PPB)
map(database = 'county','mississippi')
NO21H <- read.csv("NO21HR.csv")
points(x = NO21H$longitude, y = NO21H$latitude, pch = 19, col = 'blue')
title(main = "1-HR NO2")
text(NO21H$longitude, y = NO21H$latitude, labels = NO21H$ninety_eighth_percentile, pos = 2, font = 2, cex = 1.5)

#Statewide Map of Annual NO2(PPB)
map(database = 'county','mississippi')
NO2Annual <- read.csv("NO2Annual.csv")
points(x = NO2Annual$longitude, y = NO2Annual$latitude, pch = 19, col = 'blue')
title(main = "Annual NO2")
text(NO2Annual$longitude, y = NO2Annual$latitude, labels = NO2Annual$arithmetic_mean, pos = 2, font = 2, cex = 1.5)
