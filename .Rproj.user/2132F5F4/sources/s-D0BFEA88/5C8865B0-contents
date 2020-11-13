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

# 1.	Construct a time plot of ‘Both Sexes’, ‘All Races (Includes Hispanics)’ 
# across all and individual ages done on the exam. 

dat = subset(data,data$Sex ==c("Both Sexes"))
dat = subset(dat,dat$Race == "All Races (includes Hispanic)")


#####################################################################################
# plotting long format data
#####################################################################################

# Time Plot
ggplot(dat, aes(x=Year, y=Rate,color=Age)) + geom_line() +
  xlab("Year") + ylab("Rate") +
  ggtitle("Cancer Rate wrt Age Groups  \n All Races and Both Sexes over time")

#####################################################################################
# Converting long to wide format data
#####################################################################################

data_wide <- dcast(dat, Year ~ Age, value.var="Rate")
colnames(data_wide)

#####################################################################################
# Plotting wide format data
#####################################################################################
# get() gets the value name of the variable. As my varibales has spaces I am not able 
# to use it directly that's why i am using get()
ggplot(data_wide, aes(Year)) + 
  geom_line(aes(y = get("Ages < 20"), colour = "Ages < 20")) +
  geom_line(aes(y = get("Ages < 50"), colour = "Ages < 50")) +
  geom_line(aes(y = get("Ages <15") , colour ="Ages <15")) +
  geom_line(aes(y = get("Ages 15-39"), colour = "Ages 15-39")) +
  geom_line(aes(y = get('Ages 40-64'), colour ="Ages 40-64")) +
  geom_line(aes(y = get("Ages 50-64") , colour ="Ages 50-64")) +
  geom_line(aes(y = get("Ages 65-74"), colour ="Ages 65-74")) +
  geom_line(aes(y = get("Ages 65+"), colour ="Ages 65+")) +
  geom_line(aes(y = get("Ages 75+"), colour ="Ages 75+")) +
  geom_line(aes(y = get("All Ages") , colour = "All Ages")) +
  xlab("Year") + ylab("Rate") +
  ggtitle("Cancer Rate wrt Age Groups  \n All Races and Both Sexes over time")

#####################################################################################
# Converting wide to long format data
#####################################################################################

data_long_back = melt(data_wide, id.vars="Year" ,
                      variable.name="Age",
                      value.name="Rate")

#####################################################################################
# References
# https://stackoverflow.com/questions/3777174/plotting-two-variables-as-lines-using-ggplot2-on-the-same-graph
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
#####################################################################################


