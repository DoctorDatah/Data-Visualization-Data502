# install.packages("cowplot")
library(cowplot)

library(ggplot2)
library(scales)
library(reshape2)

#################################################################################

crime_df <-read.csv("C:/Users/Malik/Documents/GitHub/Data-Visualization-Data502/Dataset/DATA_FBI Crime Rate.csv")
crime_df_cases = crime_df[c(1:20),c(1,2,3,5,9,11,13,15,17,19,21)]
crime_df_rates = crime_df[c(1:20),c(1,2,4,6,8,10,12,14,16,18,20,22)]
names(crime_df_rates)
#################################################################################

crime_df_rates_melted = reshape2::melt(crime_df_rates,id.vars="Year", measure.vars=c(3:ncol(crime_df_rates)), value.name="value")
write.csv(crime_df_rates_melted,"C:/Users/Malik/Documents/GitHub/Data-Visualization-Data502/Dataset/DATA_FBI Crime Rate_melted_r.csv")

# Line Plot
p = ggplot(crime_df_rates_melted, aes(Year,value,color=variable)) + geom_line() + xlab("year") + ylab("value") + ggtitle("Crime Rate Comparison")
p 

# Box Plot
crime_df_rates_violent = crime_df_rates[,3:8]
crime_df_rates_property = crime_df_rates[,9:ncol(crime_df_rates)]

boxplot(crime_df_rates_violent[,c(2:ncol(crime_df_rates_violent))], main="violent crimes", xlab = "crimes", ylab= "values")
boxplot(crime_df_rates_property[,c(2:ncol(crime_df_rates_property))], main="property crines", xlab="crimes", ylab="values")


# improving line graph


plt = ggplot(crime_df_rates_melted, aes(Year,value,color=variable)) + geom_line(lwd=1)  + labs(title = "Crime Rate Comparison",
         subtitle = "Over the period of 1996 t0 2016",
         caption = "Data source: HW 5",
         color = "Crimes Legend") + scale_x_continuous(breaks=seq(1997, 2016, 1))

plt = plt+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                  panel.background = element_blank(),
                              axis.line = element_line(colour = "Black"),
                              panel.border = element_blank())

plt = plt+theme(axis.text.y = element_text(angle = 90,size = 13),
                axis.text.x = element_text(size = 13,angle = 90)) 




plt = plt + theme(plot.title = element_text(color = "red", size = 14, face = "bold"),
  plot.subtitle = element_text(color = "blue"),
  plot.caption = element_text(color = "purple", face = "italic"))

plt = plt+theme(axis.title.y = element_text(angle = 90, color = "Blue", size = 14, face = "bold"),
                axis.title.x = element_text( color = "Blue", size = 14, face = "bold"))

plt = plt+ theme(legend.title = element_text(color = "black", size = 13),
                 legend.text = element_text(color = "navy"))

plt 

# Plting together 
plot_grid(p, plt, labels = "AUTO")


