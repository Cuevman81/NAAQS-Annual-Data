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

#4th Max Value 10 Year Trends
O3_10YT <- aqs_annualData(aqs_user=myuser,
                          endpoint="byState",
                          state="28",
                          bdate="20190101",
                          edate="20191231",
                          param="44201")

dim(O3_10YT)

write.csv(O3_10YT, file='History/Ozone19.csv')

library(dplyr)
library(readr)
O310YT <- list.files(path = "History", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(O310YT, "History/12_21.csv")

O310YT = read.csv("History/12_21.csv")

O310YT <- select(filter(O310YT, pollutant_standard == "Ozone 8-hour 2015"),c(sample_duration,fourth_max_value,fourth_max_datetime,local_site_name,latitude,longitude))
write.csv(O310YT, file="Ozone4H10YT.csv")

ggplot( O310YT[O310YT$local_site_name == "Cleveland Delta State",], aes(x = fourth_max_datetime , y = fourth_max_value)) +
  geom_point(aes(color = local_site_name), size = 3) +
  geom_smooth() +
  xlab("Date")+ylab("4th Highest Ozone Value") +
  labs(title="Date for 4th Highest Ozone Value") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (O310YT[O310YT$local_site_name == "Cleveland Delta State",])

ggplot( O310YT[O310YT$local_site_name == "Gulfport Youth Court",], aes(x = fourth_max_datetime , y = fourth_max_value)) +
  geom_point(aes(color = local_site_name), size = 3) +
  geom_smooth() +
  xlab("Date")+ylab("4th Highest Ozone Value") +
  labs(title="Date for 4th Highest Ozone Value") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (O310YT[O310YT$local_site_name == "Gulfport Youth Court",])

ggplot( O310YT[O310YT$local_site_name == "Hernando",], aes(x = fourth_max_datetime , y = fourth_max_value)) +
  geom_point(aes(color = local_site_name), size = 3) +
  geom_smooth() +
  xlab("Date")+ylab("4th Highest Ozone Value") +
  labs(title="Date for 4th Highest Ozone Value") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (O310YT[O310YT$local_site_name == "Hernando",])

ggplot( O310YT[O310YT$local_site_name == "Hinds CC",], aes(x = fourth_max_datetime , y = fourth_max_value)) +
  geom_point(aes(color = local_site_name), size = 3) +
  geom_smooth() +
  xlab("Date")+ylab("4th Highest Ozone Value") +
  labs(title="Date for 4th Highest Ozone Value") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (O310YT[O310YT$local_site_name == "Hinds CC",])

ggplot( O310YT[O310YT$local_site_name == "Jackson NCORE",], aes(x = fourth_max_datetime , y = fourth_max_value)) +
  geom_point(aes(color = local_site_name), size = 3) +
  geom_smooth() +
  xlab("Date")+ylab("4th Highest Ozone Value") +
  labs(title="Date for 4th Highest Ozone Value") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (O310YT[O310YT$local_site_name == "Jackson NCORE",])

ggplot( O310YT[O310YT$local_site_name == "Meridian",], aes(x = fourth_max_datetime , y = fourth_max_value)) +
  geom_point(aes(color = local_site_name), size = 3) +
  geom_smooth() +
  xlab("Date")+ylab("4th Highest Ozone Value") +
  labs(title="Date for 4th Highest Ozone Value") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (O310YT[O310YT$local_site_name == "Meridian",])

ggplot( O310YT[O310YT$local_site_name == "Pascagoula",], aes(x = fourth_max_datetime , y = fourth_max_value)) +
  geom_point(aes(color = local_site_name), size = 3) +
  geom_smooth() +
  xlab("Date")+ylab("4th Highest Ozone Value") +
  labs(title="Date for 4th Highest Ozone Value") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (O310YT[O310YT$local_site_name == "Pascagoula",])

ggplot( O310YT[O310YT$local_site_name == "TUPELO AIRPORT NEAR OLD NWS OFFICE",], aes(x = fourth_max_datetime , y = fourth_max_value)) +
  geom_point(aes(color = local_site_name), size = 3) +
  geom_smooth() +
  xlab("Date")+ylab("4th Highest Ozone Value") +
  labs(title="Date for 4th Highest Ozone Value") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (O310YT[O310YT$local_site_name == "TUPELO AIRPORT NEAR OLD NWS OFFICE",])

ggplot( O310YT[O310YT$local_site_name == "Waveland",], aes(x = fourth_max_datetime , y = fourth_max_value)) +
  geom_point(aes(color = local_site_name), size = 3) +
  geom_smooth() +
  xlab("Date")+ylab("4th Highest Ozone Value") +
  labs(title="Date for 4th Highest Ozone Value") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (O310YT[O310YT$local_site_name == "Waveland",])

ggplot( O310YT[O310YT$local_site_name == "Coffeeville",], aes(x = fourth_max_datetime , y = fourth_max_value)) +
  geom_point(aes(color = local_site_name), size = 3) +
  geom_smooth() +
  xlab("Date")+ylab("4th Highest Ozone Value") +
  labs(title="Date for 4th Highest Ozone Value") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (O310YT[O310YT$local_site_name == "Coffeeville",])
