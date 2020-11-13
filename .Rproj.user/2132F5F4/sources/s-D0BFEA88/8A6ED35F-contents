library(cowplot)
library(ggplot2)
library(scales)
library(reshape2)
library(xlsx)
#############################################################################

data <-read.csv("C:/Users/Malik/Documents/GitHub/Data-Visualization-Data502/exam1part2/exam 1 dataset.csv",
                na.strings = c("-"))

# Structure of data
str(data)

# Changing Column Names
colnames(data)
colnames(data)<- c("Sex", "Race", "Age", "Stage", "Rate Type", "Year", "Rate")

# Converting Rate to Numeric
# not needed this b/c rate was treated as char due to "-" was used for nulls.
# I fixed that in file load
# No need for conversion
data[,"Rate"] = sapply(data[,"Rate"], as.character)
data[,"Rate"] = sapply(data[,"Rate"],  as.numeric)

str(data)

# a.	comparison/boxplots of cancer rates between males and females 
# ('all races' and 'all ages')
data2 = subset(data,data$Sex == c("Female","Male"))
data3 = subset(data2,data2$Race == "All Races (includes Hispanic)")
data_a = subset(data3, data3$Age == "All Ages")


# plot a
ggplot(data_a[,c(1,7)], aes(x=Sex, y=Rate)) +  geom_boxplot() +
  xlab("Sex (Male/Female)") + ylab("Rate") +
  ggtitle("Cancer Rate Sex(Male/Female) \nAll Races | All Ages")

#b.	comparison/boxplots of cancer rates for 'both sexes' and 'all ages' 
#across the ethnic groups (all and individual)

data2 = subset(data,data$Sex == c("Both Sexes"))
#data3 = subset(data2,data2$Race == c("All Races (includes Hispanic)","")) All are Already Present.
data_b = subset(data2, data2$Age == "All Ages")

ggplot(data_b[,c(2,7)], aes(x=Race, y=Rate)) + geom_boxplot() +
  xlab("Race") + ylab("Rate") +
  ggtitle("Cancer Rate wrt Races  \n All Ages and Both Sexes") +
  theme(axis.text.x = element_text(angle = 90,size = 7))
# for better view zoom this plot

# c.	comparison/boxplots of cancer for 'both sexes' and 'all races' 
# across the age groups (all and individual age groups)
data2 = subset(data,data$Sex == c("Both Sexes"))
data_c = subset(data2,data2$Race == "All Races (includes Hispanic)")
# data_a = subset(data3, data3$Age == "All Ages") All are Already 

ggplot(data_c[,c(3,7)], aes(x=Age, y=Rate)) + geom_boxplot() +
  xlab("Age Group") + ylab("Rate") +
  ggtitle("Cancer Rate wrt Age Groups  \n All Races and Both Sexes") +
  theme(axis.text.x = element_text(angle = 90,size = 7))


####### Line Plots #########

ggplot(data_a[,c(1,6,7)], aes(x=Year, y=Rate, color=Sex)) + geom_line() +
  xlab("Year") + ylab("Rate") +
  ggtitle("Cancer Rate Sex(Male/Female) \nAll Races | All Ages over time")
ggplot(data_b[,c(2,6,7)], aes(x=Year, y=Rate, color=Race)) + geom_line() +
  xlab("Year") + ylab("Rate") +
  ggtitle("Cancer Rate wrt Races  \n All Ages and Both Sexes over time")
ggplot(data_c[,c(3,6,7)], aes(x=Year, y=Rate,color=Age)) + geom_line() +
  xlab("Year") + ylab("Rate") +
  ggtitle("Cancer Rate wrt Age Groups  \n All Races and Both Sexes over time")



# Conclusions/Observations
#a
# In general males have higher rate of cancer than females on overall ages and race.
#b
# American Indian/Alaska Native, Asians and Hispanics have low cancer rates as compared to
# white , non-hispanic white and people. which have higher rates.
#c
# age 1 t0 40 have very less cancer rate. 40 to 65 higher then that. 65 to 80 age have very
# high cancer rates.

#a
# male cancer rate has dropped significantly over time. However female cancer rate almost remans same
#b
# For all the ethincities and races there is fall in cancer rate over the given period.
#c
# there is very slow fall in cancer rate of 65+ age groups. However, extremely slow decline in 
# middle ages caner rate can also be observed. and young ages has almost lienar rates.



################# Transforming Covid Data #################

library(reshape2)
library(xlsx)
covid <-read.xlsx("C:/Users/Malik/Documents/GitHub/Data-Visualization-Data502/exam1part2/covid.xlsx",1)
covid_melted = reshape2::melt(covid,id.vars="State", measure.vars=c(4,5), value.name="value")
write.csv(covid_melted,"C:/Users/Malik/Documents/GitHub/Data-Visualization-Data502/exam1part2/covid.csv")

        