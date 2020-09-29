# install.packages("scales")
library(ggplot2)
library(scales)

#################################################################################

crime_df <-read.csv("C:/Users/Malik/Documents/GitHub/Data-Visualization-Data502/Dataset/DATA_FBI Crime Rate.csv")


#################################################################################
p = ggplot(data=crime_df, aes( x=Year,y=Population1))
scale_it = scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

p + geom_line() + 
  scale_it +
  ggtitle("Poplutaion over years") 

#--------------------------------------------------------------------------#
# All Crimes
ggplot(crime_df, aes( x=Year)) + scale_it +
  geom_line(aes(y =Violent.crime2, colour = "Violent.crime2")) +
  geom_line(aes(y =Rape..revised..definition3., colour = "Violent.crime2")) + 
  geom_line(aes(y =Rape..legacy..definition4., colour = "Violent.crime2")) + 
  geom_line(aes(y = Murder.and.nonnegligent..manslaughter , colour = "Murder.and.nonnegligent..manslaughter")) +
  geom_line(aes(y =Robbery, colour = "Robbery")) + 
  geom_line(aes(y =Aggravated..assault, colour = "Aggravated..assault")) +
  geom_line(aes(y =Robbery, colour = "Robbery")) + 
  geom_line(aes(y =Property..crime, colour = "Property..crime")) + 
  geom_line(aes(y =Burglary, colour = "Burglary")) + 
  geom_line(aes(y =Larceny..theft, colour = "Larceny..theft")) + 
  geom_line(aes(y =Motor..vehicle..theft, colour = "Motor..vehicle..theft"))+
  ggtitle("All Crimes Comparison Distribution over years") +
  ylab("Cases")
