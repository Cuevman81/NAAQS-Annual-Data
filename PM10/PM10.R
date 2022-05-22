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


#PM10 Summary

PM10 <- aqs_annualData(aqs_user=myuser,
                       endpoint="byState",
                       state="28",
                       bdate="20210101",
                       edate="20211231",
                       param="81102")

dim(PM10)

write.csv(PM10, file='PM1021.csv')

PM10_21 <- select(filter(PM10, pollutant_standard == "PM10 24-hour 2006"),c(sample_duration,first_max_value,second_max_value,local_site_name,latitude,longitude))
write.csv(PM10_21, file="PM10_21.csv")

#Summary Statistics
summary(file = 'PM10_21.csv', PM10_21$first_max_value)
summary(file = 'PM10_21.csv', PM10_21$second_max_value)

#Annual PM10
ggplot(PM10_21, aes(x = local_site_name)) +
  geom_point(aes(y = second_max_value, color = "Annual"), size = 3) +
  xlab("Site")+ylab("PM2.5") +
  labs(title="Annual PM10") +
  theme(axis.text.x=element_text(angle=70, hjust=1))


#Statewide Map of Annual PM10(PPB)
map(database = 'county','mississippi')
PM10Annual <- read.csv("PM10_21.csv")
points(x = PM10Annual$longitude, y = PM10Annual$latitude, pch = 19, col = 'blue')
title(main = "Annual PM10")
text(PM10Annual$longitude, y = PM10Annual$latitude, labels = PM10Annual$second_max_value, pos = 3, font = 2)
