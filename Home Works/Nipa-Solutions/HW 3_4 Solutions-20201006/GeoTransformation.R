############################################################################################
###GRAD STUDENTS###################
# install.packages("rJava")

# install.packages("xlsx")

library(xlsx)
fbi<-read.xlsx("C:/Users/Malik/Documents/GitHub/Data-Visualization-Data502/Dataset/FBI Crime Geographically.xls", sheetIndex = 1)
#select columns
fbi<-fbi[c(5:202),]
#rename columns
colnames(fbi)<- c('Area','Year','Population', 'Violent_Crime_Rate','Violent_Crime_Rate_Per100k', 'Murder_Rate','Murder_Rate_Per100k',
                  'Rape_Revised_Rate','Rape_Revised_Rate_Per100k', 'Rape_Legacy_Rate','Rape_Legacy_Rate_Per100k','Robbery_Rate','Robbery_Rate_Per100k','Aggravated_Assault_Rate','Aggravated_Assault_Rate_Per100k',
                  'Property_Crime_Rate','Property_Crime_Rate_Per100k','Burglary_Rate','Burglary_Rate_Per100k','Larceny_Rate',
                  'Larceny_Rate_Per100k','Motor_Rate','Motor_Rate_Per100k')

#isolate the column with the states/regions
area<-data.frame(fbi[,1])
area<-subset(area, area$fbi...1. !='na')
colnames(area)<-'Area'
str(area) # Checking Strusture 

#select only the rows that have percent change, ie the rates
fbi<-subset(fbi, fbi$Year %in% c('Percent change'))

#replace the Area column that has all na with the Area column from above created area dataframe
fbi$Area<-area$Area
#select the columns you want for rates (rate or rate per 100k)
fbi<-fbi[,c(1,4:23)] # All Rates
fbi<-fbi[,c(1,2,4,6,8,10,12,14,16,18,20)] # Just Regular Rates

#write the file out with columns horizontal
write.csv(fbi, "C:/Users/Malik/Documents/GitHub/Data-Visualization-Data502/Dataset/fbi geographic columns.csv")

#check structure b/c need the rate columns to be numeric in order to melt
str(fbi) # Eveyting seems CHR type
fbi2<- fbi

#for a factor to be converted to numeric, need first to convert to character, and then to numeric
fbi2[,2:11]<- sapply(fbi2[,2:11], as.character)
str(fbi2)
fbi2[,2:11]<- sapply(fbi2[,2:11], as.numeric)
str(fbi2)

#melt the dataframe to transpose the data
fbi_t<- reshape2::melt(fbi2, id.vars="Area", measure.vars=c(2:ncol(fbi2)), value.name="Value")

#write it out and use in excel for map :)
write.csv(fbi_t,"C:/Users/Malik/Documents/GitHub/Data-Visualization-Data502/Dataset/FBI Crime Geographically transposed.csv")
#write.csv(fbi_t, "D:/stats stuff - Copy/DataVis/Homework Assignments/HW 4/fbi geographic transposed.csv")

