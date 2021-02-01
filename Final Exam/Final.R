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

FILE_PATH = "Final Exam/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv"
data1 <-read.csv(FILE_PATH)
str(data1) 
dim(data1) # 18822    15
#############################################################################
# converting chr to date for submission date
data1$submission_date = as.POSIXct( strptime(x = as.character(data1$submission_date), format="%m/%d/%Y"))
#############################################################################
# melting  data
# only intersed columns
data1_m= reshape2::melt(data1[c(1,2,3,6)], id.vars=c('submission_date',"state"),
                         variable.name="total_new",
                         value.name="Value")
data1_m$total_new <- factor(data1_m$total_new, levels = c("tot_cases", "new_case"), 
                                        labels = c("Total Cases", "New Cases"))
dim(data1_m) # 37644     4
#############################################################################
# 1
#############################################################################
# sub setting data only for new jersey
NJ_data_from_melted = data1_m[data1_m["state"] == "NJ",,drop=F]
dim(NJ_data_from_melted) # 628   4
# plotting 
scale_it = scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
mP  <- ggplot(NJ_data_from_melted, aes_string(x="submission_date", y="Value", fill="total_new")) +
  geom_line() +
  xlab("Submission Date") + ylab("No of. Cases") +
  ggtitle("NJ COVID Total and New Cases") 
mp = mP +  facet_wrap(~ total_new, nrow=2, scales="free",
             strip.position="right")
mp
# I have also modified the scientific notion to regular for total cases:
mp + scale_it

#############################################################################
# 2
#############################################################################
unique(data1_m['state'])
states_to_consider = c('NJ', "NY" ,'NYC','PA', 'CT', 
    'MA', 'VT', 'ME', 'WA', 'CA')

# subsetting
other_states_data_from_m = data1_m[data1_m$state %in% states_to_consider,]
dim(other_states_data_from_m)

# plotting all stats
mP2  <- ggplot(other_states_data_from_m, aes_string(x="submission_date", y="Value", fill="total_new", col="state")) +
  geom_line() +
  xlab("Submission Date") + ylab("No of. Cases") +
  ggtitle("Seleted States Total and New Cases") 

mp2 = mP2 +  facet_wrap(~ total_new, nrow=2, scales="free", strip.position="right")
mp2
mp2 + scale_it

# Marks: Lines 
# Channel: Color Hue (to distinguish between states) 
# Unusual behavior: Some states has negative numbers of new Cases.
# Why is that? 
#  1st: Maybe Because of the people has recovered.
# But another thing I found weird is that before going negative there is a positive spike in the plot (all most every time)
# 2nd: It could be people getting tested and those results of negative our weight the number of positive tests done that day. 

# 3rd: the negative no. of new cases has subtracted from total cases. I think there is miss calculation of the new cases at previous date that has been fixed in subsequent dates. 
# That could be the reasons we have seen sudden increase and then later instant decrease at various data points in the plot.

# I will go with my third hypothesis.   

#############################################################################
# 3: Tableau
#############################################################################
# visit tableau  or report
# Observation: Even though some states has less number of total cases as compared to other sates yet they have high death rates. 
# For example: NJ and NYC

#############################################################################
# 4: Continuing with original dataset
#############################################################################
# Mutating Date

other_states_data_from_m_mutated = other_states_data_from_m %>% mutate(year = year(submission_date),
                       month = month(submission_date, label=F),
                       day = day(submission_date))
dim(other_states_data_from_m_mutated)
# no need to replace month names with month numbers. Just set label parameter to F
#############################################################################
head(other_states_data_from_m_mutated,1)
head(other_states_data_from_m_mutated[c(2,3,4,6)])
agg = aggregate(other_states_data_from_m_mutated[c(2,3,4,6)],
                by = list(other_states_data_from_m_mutated$month
                          ,other_states_data_from_m_mutated$state
                          ,other_states_data_from_m_mutated$total_new),
                FUN = mean)
head(agg)

agg2 = agg[c(1,2,3,6)]
head(agg2,5)
names(agg2) = c("Month","State","CaseType","Value")

# Note: agg2 contains value in scientific notation. 
# it not an issue when i plot i get them as regular notation

#############################################################################
### Plotting heat map

g1 <- ggplot(agg2[agg2["CaseType"] == "New Cases",,drop=F], aes(x = Month,State)) +
  geom_tile(aes(fill = Value)) +
  xlab("Months") + ylab("States") +
  ggtitle("Seleted States New Cases") +
  guides(fill=guide_legend(title="cases"))
g1


g2 <- ggplot(agg2[agg2["CaseType"] == "Total Cases",,drop=F], aes(x = Month,State)) +
  geom_tile(aes(fill = Value)) +
  xlab("Months") + ylab("States") +
  ggtitle("Seleted States Total Cases") +
  guides(fill=guide_legend(title="cases"))
g2

# Combined plot
plot_grid(g1, g2, labels = "AUTO")

# Problem with the plot in question:
# Both plots have a huge difference in values. plotted on same scale (i.e. the legend on right is same for both). 
# New cases are comparatively far less than the total cases. Which makes the new cases graph to much skewed. Hence, not representing the true insight of the data. 

# Plotting both separate and comparing them has solved the issue. 

#############################################################################
# 5:
#############################################################################
NJ_mutated_data = other_states_data_from_m_mutated[other_states_data_from_m_mutated["state"] == "NJ",,drop=F]
temp = NJ_mutated_data[NJ_mutated_data["total_new"] == "New Cases",,drop=F]
g2 <- ggplot(temp, aes(x = month,day)) +
  geom_tile(aes(fill = Value)) +
  xlab("Months") + ylab("Days") +
  ggtitle("NJ New Cases") +
  guides(fill=guide_legend(title="Cases"))
g2

# Observation: Same as the line plot in question 1. 
# No number of cases in up to the 3rd week of January. 
# Then a few cases started slowly. 
# By the end of march cases began rising and in April it reached the peak then gradually getting down in May. 
# Very few cases from July to Oct. Again, cases start rising in October up until November.  

# In short, April, May, Oct and Nov was hit badly.

#############################################################################
# 6:
#############################################################################
# I used original data set. 
# month(submission_date, label=F)
# label = F gives the month number instead of word. Hence, no need to replace month names in that cases.
