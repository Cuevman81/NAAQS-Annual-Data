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


#PM25 Summary

PM25 <- aqs_annualData(aqs_user=myuser,
                       endpoint="byState",
                       state="28",
                       bdate="20200101",
                       edate="20201231",
                       param="88101")

dim(PM25)

write.csv(PM25, file='PM25.csv')

newdataPM25 <- select(filter(PM25, pollutant_standard == "PM25 Annual 2012"),c(sample_duration,arithmetic_mean,ninety_eighth_percentile,local_site_name,latitude,longitude))
write.csv(newdataPM25, file="PM25Annual.csv")

#Summary Statistics
summary(file = 'PM25Annual.csv', newdataPM25$arithmetic_mean)
summary(file = 'PM25Annual.csv', newdataPM25$ninety_eighth_percentile)

#Annual and Daily PM2.5
ggplot(newdataPM25, aes(x = local_site_name)) +
  geom_point(aes(y = arithmetic_mean, color = "Annual"), size = 3) +
  geom_point(aes(y = ninety_eighth_percentile, color = "Daily"), size = 3) +
  xlab("Site")+ylab("PM2.5") +
  labs(title="Annual and Dailiy PM2.5") +
  theme(axis.text.x=element_text(angle=70, hjust=1))


#Statewide Map of Annual PM25(PPB)
map(database = 'county','mississippi')
PM25Annual <- read.csv("PM25Annual.csv")
points(x = PM25Annual$longitude, y = PM25Annual$latitude, pch = 19, col = 'blue')
title(main = "Annual Weighted PM2.5")
text(PM25Annual$longitude, y = PM25Annual$latitude, labels = PM25Annual$arithmetic_mean, pos = 3, font = 2)

#Statewide Map of 24-Hour PM25(PPB)
map(database = 'county','mississippi')
PM25Annual <- read.csv("PM25Annual.csv")
points(x = PM25Annual$longitude, y = PM25Annual$latitude, pch = 19, col = 'blue')
title(main = "24-Hour PM2.5")
text(PM25Annual$longitude, y = PM25Annual$latitude, labels = PM25Annual$ninety_eighth_percentile, pos = 3, font = 2)