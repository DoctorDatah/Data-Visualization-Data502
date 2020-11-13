library(cowplot)
library(ggplot2)
library(scales)
library(reshape2)
library(xlsx)
library(tibbletime)
library(dplyr)
library(lubridate)
library(plotly)
library(trelliscopejs)
library(viridis)


#############################################################################
# Reading data
#############################################################################

fbi<-read.csv("C:/Users/Malik/Documents/GitHub/Data-Visualization-Data502/Dataset/DATA_FBI Crime Rate.csv")

fbi_totals<-fbi[c(1:20), c(1,2,3,5,9,11,13,15,17,19,21)]
fbi_rates<- fbi[c(1:20), c(1,2,4,6,10,12,14,16,18,20,22)]

colnames(fbi_totals)<- c("year", "population", "violent_crimes", "murder", "rape", "robbery", "aggravated_assault", "property_crimes", "bulgarly", "larceny", "motor_theft")

colnames(fbi_rates)<- c("year", "population", "violent_crimes_rate", "murder_rate", "rape_rate", "robbery_rate","aggravated_assault_rate", "property_crimes_rate", "bulgarly_rate", "larceny_rate", "motor_theft_rate")

fbi_violent_totals<- fbi_totals[,c(1:7)]
fbi_property_totals<-fbi_totals[,c(1:2,8:11)]

fbi_violent_rates<- fbi_rates[,c(1:7)]
fbi_property_rates<-fbi_rates[,c(1:2,8:11)]


fbi_violent_melt<- reshape2::melt(fbi_violent_totals, id.vars=c("year","population"), measure.vars=c(3:ncol(fbi_violent_totals)), value.name="value")
ggplot(fbi_violent_melt, aes(year, value, color=variable)) + geom_line() +xlab("year") + ylab("value") + ggtitle("violent crimes")

fbi_property_melt<- reshape2::melt(fbi_property_totals, id.vars=c("year","population"), measure.vars=c(3:ncol(fbi_property_totals)), value.name="value")

fbi_violent_rates_melt<- reshape2::melt(fbi_violent_rates, id.vars=c("year","population"), measure.vars=c(3:ncol(fbi_violent_rates)), value.name="value")

fbi_property_rates_melt<- reshape2::melt(fbi_property_rates, id.vars=c("year","population"), measure.vars=c(3:ncol(fbi_property_rates)), value.name="value")

violent_property_totals<- rbind(fbi_violent_melt, fbi_property_melt)
violent_property_rates<-rbind(fbi_violent_rates_melt, fbi_property_rates_melt)

all_crimes<- cbind(violent_property_totals, violent_property_rates)
all_crimes<-all_crimes[,c(1,2,3,4,7,8)]
colnames(all_crimes)<-c("year","population","crimes","crime_total_value","crime_rates","crime_rate_values")


######
rob = subset(all_crimes, all_crimes$crimes %in% c("robbery"))
bul =  subset(all_crimes, all_crimes$crimes %in% c("bulgarly"))

rob = rob[,c(1,2,4,6)]
bul = bul[,c(1,2,4,6)]
colnames(rob) = c("year", "population", "rob_totals", "rob_rated")
colnames(bul) = c("year", "population", "bul_totals", "bul_rated")

sub_data2 = cbind(rob,bul)
sub_data2 = sub_data2[,c(1,2,3,4,6,7,8)]



######## plot ########
ggplot(sub_data2, aes(x=rob_totals , y = bul_totals, size=population, label = year )) + 
  geom_point(alpha =0.7, aes(color=year)) +
  scale_colour_viridis(option = "D") +
  geom_text(size=3,check_overlap = T,nudge_y = .5) +
    xlab("robbery Totals ") + ylab("Bulgry totals")


options(scipen=10000)

ggplot(sub_data2, aes(x=rob_totals , y = bul_totals, size=year, label = population )) + 
  geom_point(alpha =0.7, aes(color=population)) +
  scale_colour_viridis(option = "D") +
  geom_text(size=3,check_overlap = T,nudge_y = .5) +
  xlab("robbery Totals ") + ylab("Bulgry totals") 
            
            