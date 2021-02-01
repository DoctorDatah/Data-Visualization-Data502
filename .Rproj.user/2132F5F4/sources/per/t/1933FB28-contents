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


#############################################################################
# Reading data
#############################################################################

data <- read.xlsx("C:/Users/Malik/Documents/GitHub/Data-Visualization-Data502/Home Works/HW 9/Copy of Dynamic Baseline Stacked Column Chart Excel.xls",
                  sheetIndex = 1,
                  rowIndex =4:7 , colIndex = 2:15)
summary(data)
head(data)

#############################################################################
# Melting Data
#############################################################################
datal= reshape2::melt(data, id.vars=c("Index","Region"),
               variable.name="Months",
               value.name="Value")
head(datal)

#############################################################################
# Stacked bar chart
#############################################################################

p91 = ggplot(datal, aes(x=Months,y=Value,fill=Region)) + geom_bar(stat = "identity")
ggplotly(p91)

#############################################################################
# facet_grid bar chart
#############################################################################

ggplot(datal, aes(x=Months,y=Value,fill=Region)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45)) +
  facet_grid(~Region)

#############################################################################
# trelliscope bar chart
#############################################################################

ggplot(datal, aes(x=Months,y=Value,fill=Region)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle=45)) +
  scale_fill_brewer(palette = "Paired")+
  facet_trelliscope(~Region,nrow = 1,ncol = 3)



