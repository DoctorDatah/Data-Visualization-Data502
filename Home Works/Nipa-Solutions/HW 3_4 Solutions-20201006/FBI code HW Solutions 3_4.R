fbi <-read.csv("C:/Users/Malik/Documents/GitHub/Data-Visualization-Data502/Dataset/DATA_FBI Crime Rate.csv")

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
ggplot(fbi_property_melt, aes(year, value, color=variable)) + geom_line() +xlab("year") + ylab("value") + ggtitle("property crimes")

fbi_violent_rates_melt<- reshape2::melt(fbi_violent_rates, id.vars=c("year","population"), measure.vars=c(3:ncol(fbi_violent_rates)), value.name="value")
ggplot(fbi_violent_rates_melt, aes(year, value, color=variable)) + geom_line() +xlab("year") + ylab("value") + ggtitle("violent crimes")

fbi_property_rates_melt<- reshape2::melt(fbi_property_rates, id.vars=c("year","population"), measure.vars=c(3:ncol(fbi_property_rates)), value.name="value")
ggplot(fbi_property_rates_melt, aes(year, value, color=variable)) + geom_line() +xlab("year") + ylab("value") + ggtitle("property crimes")

par(mfrow=c(1,1))
boxplot(fbi_violent_totals[,3:7], main="violent crimes", xlab = "crimes", ylab="values")
boxplot(fbi_property_totals[,3:6], main="property crimes", xlab="crimes", ylab="values")
boxplot(fbi_violent_rates[,3:7], main="violent crimes", xlab = "crimes", ylab="values")
boxplot(fbi_property_rates[,3:6], main="property crimes", xlab="crimes", ylab="values")


violent_property_totals<- rbind(fbi_violent_melt, fbi_property_melt)
violent_property_rates<-rbind(fbi_violent_rates_melt, fbi_property_rates_melt)

all_crimes<- cbind(violent_property_totals, violent_property_rates)
all_crimes<-all_crimes[,c(1,2,3,4,7,8)]
colnames(all_crimes)<-c("year","population","crimes","crime_total_value","crime_rates","crime_rate_values")

#write.csv(all_crimes, "C:/Nipa/stats stuff/DataVis/Homework Assignments/FBI data ALL from R.csv")
