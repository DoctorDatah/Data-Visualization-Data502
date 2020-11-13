library(cowplot)
library(ggplot2)
library(scales)
library(reshape2)
library(xlsx)
library(tibbletime)
library(dplyr)
library(lubridate)
#install.packages("tibbletime")
#install.packages("lubridate")

#############################################################################

data <-read.csv("C:/Users/Malik/Documents/GitHub/Data-Visualization-Data502/Home Works/HW 7/ext_data.csv")
summary(data)
head(data)

# Doing on whole data

data$SampleTime = as.POSIXct( strptime(x = as.character(data$SampleTime), format="%m/%d/%Y %H:%M"))
data$date = data$SampleTime
data = data %>% mutate(year = year(date),
                   month = month(date, label=T),
                   day = day(date),
                   hour =hour(date))


head(data)

#install.packages("viridis")
library(viridis)
?scale_fill_viridis() 
#Uses the viridis color scale.


### Plotting heat map
g1 <- ggplot(data, aes(x = day,hour)) +
  geom_tile(aes(fill = TempF)) +
  xlab("Day(s)") + ylab("Hour(s)") +
  ggtitle("Hourly Temperature") +
  guides(fill=guide_legend(title="Temperature in F"))  +
  scale_fill_viridis()   
g1

g2 <- ggplot(data, aes(x = day,hour)) +
  geom_tile(aes(fill = Humidity)) +
  xlab("Day(s)") + ylab("Hour(s)") +
  ggtitle("Hourly Humidity") +
  guides(fill=guide_legend(title="Humidity"))  +
  scale_fill_viridis()   
g2


g3 <- ggplot(data, aes(x = day,hour)) +
  geom_tile(aes(fill = WindSpeedMPH)) +
  xlab("Day(s)") + ylab("Hour(s)") +
  ggtitle("Hourly WindSpeedMPH") +
  guides(fill=guide_legend(title="WindSpeedMPH"))  +
  scale_fill_viridis()   
g3

# all 3 together
plot_grid(g1,g2,g3, labels = "AUTO")



#### Facet ### 
g4 <- ggplot(data, aes(x = day,hour)) +
  geom_tile(aes(fill = Humidity)) +
  xlab("Day(s)") + ylab("Hour(s)") +
  ggtitle("Hourly Humidity") +
  guides(fill=guide_legend(title="Humidity"))  +
  scale_fill_viridis()   + facet_grid(.~ day, scales="free_x")
g4

####### Aggregate 

# converting  months abbreviations to numbers 
data$month = match(data$month,month.abb)
head(data)

agg = aggregate(data,
                by = list(data$day),
                FUN = mean)
head(agg)

agg2 = agg[c("day","SampleTime","TempF","Humidity","WindSpeedMPH")]



