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


#Ozone Summary

Ozone <- aqs_annualData(aqs_user=myuser,
                    endpoint="byState",
                    state="28",
                    bdate="20200101",
                    edate="20201231",
                    param="44201")

dim(Ozone)

write.csv(Ozone, file='Ozone.csv')

newdataO3 <- select(filter(Ozone, pollutant_standard == "Ozone 8-hour 2015"),c(sample_duration,fourth_max_value,fourth_max_datetime,local_site_name,latitude,longitude))
write.csv(newdataO3, file="Ozone4H.csv")

#Summary Statistics
summary(file = 'Ozone4H.csv', newdataO3$fourth_max_value)

#Date for 4th Highest Ozone Value
ggplot(newdataO3, aes(x = fourth_max_datetime , y = fourth_max_value)) +
  geom_point(aes(color = local_site_name), size = 3) +
  xlab("Date")+ylab("4th Highest Ozone Value") +
  labs(title="Date for 4th Highest Ozone Value") +
  theme(axis.text.x=element_text(angle=70, hjust=1))

#Statewide Map of 4th Highest Ozone Value(PPB)
map(database = 'county','mississippi')
Ozone4H <- read.csv("Ozone4H.csv")
points(x = Ozone4H$longitude, y = Ozone4H$latitude, pch = 19, col = 'blue')
title(main = "Annual 4th Highest Ozone Value (ppm)")
text(Ozone4H$longitude, y = Ozone4H$latitude, labels = Ozone4H$fourth_max_value, pos = 1, col = 'darkgreen', font = 2)


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


#CO Summary


CO <- aqs_annualData(aqs_user=myuser,
                     endpoint="byState",
                     state="28",
                     bdate="20200101",
                     edate="20201231",
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



