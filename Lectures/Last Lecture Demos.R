
#https://nipaonulak.weebly.com/
#https://www.r-graph-gallery.com/index.html
#https://ggplot2.tidyverse.org/reference/


#BARPLOT WITH LINES FOR TREND

library(ggplot2)

df<-read.csv("D:/stats stuff - Copy/DataVis/Homework Assignments/HW 8 and 9/sales data.csv")

str(df)
df<- reshape2::melt(df, id.vars="Region", measure.vars=c(3:ncol(df)), value.name="Sales")
colnames(df)[2]<-"Month"

ggplot(df, aes(x=Month, y=Sales, fill=Region)) + 
  geom_bar(stat="identity") + 
  scale_fill_brewer()+theme_bw()

south<- subset(df, df$Region %in% c("South"))

ggplot(south, aes(x=Month, y=Sales, fill=Region)) + 
  geom_bar(stat="identity") + 
  scale_fill_brewer()+theme_bw()+
  geom_hline(yintercept=15, linetype="dashed", color = "red")

ggplot(south, aes(x=Month, y=Sales, fill=Region)) + 
  geom_bar(stat="identity") + 
  scale_fill_brewer()+theme_bw()+
  geom_line(aes(x = Month, y = Sales), size = 1.5, color="red", group = 1)

ggplot(south) + 
  geom_col(aes(x = Month, y = Sales), size = 1, color = "darkblue", fill = "white") +
  geom_line(aes(x = Month, y = Sales), size = 1.5, color="red", group = 1)

sp<-ggplot(south) + 
  geom_col(aes(x = Month, y = Sales), size = 1, color = "darkblue", fill = "white") +
  geom_line(aes(x = Month, y = Sales), size = 1.5, color="red", group = 1)

library(grid)
# Create a text
grob <- grobTree(textGrob("HELLO", x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
sp + annotation_custom(grob)

sp+ annotate("text", x=3, y =20, label="HELLO")
sp+ annotate("text", x=3:12, y =20, label="HELLO")
sp+ annotate("text", x = c(2,4,6), y = 25, label = "HELLO")
sp+ annotate("text", x = 1.5, y = 30, label = "italic(R) ^ 2 == 0.75",
             parse = TRUE)

sp+annotate("rect", xmin = 2.5, xmax = 5.5, ymin = 0, ymax = 21,
            alpha = .2)

sp+labs(title = "Sales for South",
        subtitle = "Sales reflected on a monthly basis",
        caption = "Data source: Random Site")

#################################################################
#LINEAR REGRESSION APPLIED TO VISUAL (STATS)

require(stats)
reg<-lm(mpg ~ wt, data = mtcars)
reg
sp <- ggplot(data=mtcars, aes(x=wt, y=mpg)) + geom_point()
sp + geom_abline(intercept = 37, slope = -5, color="red", 
                 linetype="dashed", size=1.5)

coeff=coefficients(reg)
# Equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))

sp + geom_abline(intercept = 37, slope = -5, color="red", 
                 linetype="dashed", size=1.5) +
  ggtitle(eq)
###############################################################
#PIE CHART
ggplot(iris, aes(x="", y="", fill=Species)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

ggplot(iris, aes(x="", y="", fill=Species)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()

pie(table(iris$Species), main = "Pie Chart of the Iris data set Species", 
    col = c("orange1", "chocolate", "coral"), radius = 1)
#############################################################
#PLOTLY AND WEBPAGE
library(lubridate)
library(dplyr)
library(viridis)
library(plotly)

df<- read.csv("D:/stats stuff - Copy/DataVis/Homework Assignments/HW 7/ext_data.csv")
#df<- data.frame(ext_data$SampleTime)
str(df)
df<-df[, c(2:ncol(df))]

df$value<- c(1:nrow(df))

df$SampleTime<- strptime(x= as.character( df$SampleTime ), format = "%m/%d/%Y %H:%M")
str(df)

df$SampleTime<-as.POSIXct(df$SampleTime)

df$date<-df$SampleTime


#need date in POSIXct format for this mtutate to work
df <- df %>% mutate(year = year(date),
                    month = month(date, label=TRUE),
                    day = day(date),
                    hour = hour(date))

#write.csv(df, "D:/stats stuff - Copy/DataVis/Homework Assignments/HW 7/example_dataset.csv")

ggplot(df, aes(x=day, y=hour, fill=value))+
  geom_tile(color= "white",size=0.1) +
  ggtitle("Hourly Temepratures for the Month of June")+
  scale_fill_viridis()

p<-
  ggplot(df, aes(x=day, y=hour, fill=value))+
  geom_tile(color= "white",size=0.1) +
  ggtitle("Hourly Temepratures for the Month of June")+
  scale_fill_viridis()

x<-ggplotly(p)
x

# save the widget at .html format
library(htmlwidgets)
saveWidget(x, file="heatmap.html")

#png(file="saving_plot2.png",width=600, height=350)
#x
#dev.off()
########################################################################
#INTERACTIVITY
# create a dataset:
data <- data_frame(
  from=c("A", "A", "B", "D"),
  to=c("B", "E", "F", "A")
)

# Plot
library(igraph)
library(networkD3)
p <- simpleNetwork(data, height="100px", width="100px")

# save the widget at .html format
library(htmlwidgets)
saveWidget(p, file="simpleNetwork.html")

p

#library(antaresViz)
#savePlotAsPng(p, file = "test.png")


#png(filename="test.png")
#p
#dev.off()

# Save at .png
#library(webshot)
#webshot::install_phantomjs()
#webshot("simpleNetwork.html" , "output.png", delay = 0.2 , cliprect = c(440, 0, 1000, 10))
######################################################################
#3D... INTERACTIVITY
setosa<- subset(iris, iris$Species == "setosa")
ggplot(setosa, aes(Sepal.Length, Sepal.Width)) + geom_point(aes(color=Petal.Width))
library(car)
library('plot3D')
scatter3D(setosa$Sepal.Length, setosa$Sepal.Width, setosa$Petal.Width, phi=0,pch = 19, cex = 1,bty = "g", ticktype="detailed")

data <- iris

# Add a new column with color
mycolors <- c('royalblue1', 'darkcyan', 'oldlace')
data$color <- mycolors[ as.numeric(data$Species) ]

library(rgl)

# Plot
par(mar=c(0,0,0,0))
plot3d( 
  x=data$`Sepal.Length`, y=data$`Sepal.Width`, z=data$`Petal.Length`, 
  col = data$color, 
  type = 's', 
  radius = .1,
  xlab="Sepal Length", ylab="Sepal Width", zlab="Petal Length")

writeWebGL( filename="IRIS3dscatter.html" ,  width=600, height=600)


plot3d(rnorm(100), rnorm(100), rnorm(100), type = "s", col = "red")
# This writes a copy into temporary directory 'webGL', and then displays it
filename <- writeWebGL(dir = file.path(tempdir(), "webGL"), 
                       width = 500, reuse = TRUE)
# Display the "reuse" attribute
attr(filename, "reuse")

# Display the scene in a browser
if (interactive())
  browseURL(paste0("file://", filename))


#library(ggpubr)
ggplot(iris, aes(x=iris$Species, y=iris$Sepal.Length, color=iris$Species))+ geom_boxplot()+
  stat_compare_means(method = "anova")

##############################################################
#INTERACTIVE TREE
# Load library
# install.packages("collapsibleTree")
library(collapsibleTree) 

# input data must be a nested data frame:
head(warpbreaks)

# Represent this tree:
p <- collapsibleTree( warpbreaks, c("wool", "tension", "breaks"))
p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file="dendrogram_interactive.html")

#############################################################
#POPULATION PYRAMID
library(XML)
library(reshape2)
library(ggplot2)
library(plyr)
library(scales)

#make this example reproducible
set.seed(1)

#create data frame
data <- data.frame(age = rep(1:100, 2), gender = rep(c("M", "F"), each = 100))

#add population variable
data$population <- 1/sqrt(data$age) * runif(200, 10000, 15000)

#convert population variable to percentage
data$population <- data$population / sum(data$population) * 100

#view first six rows of dataset
head(data)

ggplot(data, aes(x = age, fill = gender,
                 y = ifelse(test = gender == "M",
                            yes = -population, no = population))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(data$population) * c(-1,1)) +
  coord_flip()

ggplot(data, aes(x = age, fill = gender,
                 y = ifelse(test = gender == "M",
                            yes = -population, no = population))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(data$population) * c(-1,1)) +
  labs(title = "Population Pyramid", x = "Age", y = "Percent of population") +
  coord_flip()

ggplot(data, aes(x = age, fill = gender,
                 y = ifelse(test = gender == "M",
                            yes = -population, no = population))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(data$population) * c(-1,1)) +
  labs(title = "Population Pyramid", x = "Age", y = "Percent of population") +
  scale_colour_manual(values = c("pink", "steelblue"),
                      aesthetics = c("colour", "fill")) +
  coord_flip()



# same application on different dataset
pop <- data.frame(
  sex = sort(rep(c("Female", "Male"), 6))
  , age = c("0-9", "10-19", "20-29", "30-39", "40-49", "50+", 
            "0-9", "10-19", "20-29", "30-39", "40-49", "50+")
  , pop = c(256L, 335L, 278L, 155L, 103L, 88L, 266L, 317L, 286L, 145L, 118L, 87L)
  , frac = c(0.11, 0.14, 0.11, 0.06, 0.04, 0.04, 0.11, 0.13, 0.12, 
             0.06, 0.05, 0.04)
)


ggplot(pop, aes(x = age, fill = sex,
                 y = ifelse(test = sex == "Male",
                            yes = -pop, no = pop))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(pop$pop) * c(-1,1)) +
  coord_flip()

ggsave(filename = "pyramid.pdf")



############################################################################
library( rgl )
library(magick)

# Let's use the iris dataset
# iris


colors <- c("royalblue1", "darkcyan", "oldlace")
iris$color <- colors[ as.numeric( as.factor(iris$Species) ) ]

# Static chart
plot3d( iris[,1], iris[,2], iris[,3], col = iris$color, type = "s", radius = .2 )

# We can indicate the axis and the rotation velocity
play3d( spin3d( axis = c(0, 0, 1), rpm = 20), duration = 10 )

# Save like gif
movie3d(
  movie="3dAnimatedScatterplot", 
  spin3d( axis = c(0, 0, 1), rpm = 7),
  duration = 10, 
  dir = "C:/Users/nipa.onulak/Documents/",
  type = "gif", 
  clean = TRUE
)



#################################################################
#NOT WORKING :(
# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)
library(gifski)
library(av)

# Make a ggplot, but add frame=year: one image per year
p<-ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Save at gif:
#anim_save("271-ggplot2-animated-gif-chart-with-gganimate1.gif")




######################################################################
iris
library(caTools)

dataset = iris[3:4]
# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = dataset, centers = 3)
y_kmeans = kmeans$cluster

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_kmeans,
         lines = 0,
         #shade = TRUE,
         color = TRUE,
         #labels = 2,
         #plotchar = FALSE,
         #span = TRUE,
         main = paste('K-Means Clustering of Iris Species Petal.Length vs Petal.Width'),
         xlab = 'Petal Length',
         ylab = 'Petal Width')

#######################################
library(grid)
# Create a text
grob <- grobTree(textGrob("HELLO", x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
sp2 + annotation_custom(grob)
#########################################


library(odbc)
#sort(unique(odbcListDrivers()[[1]]))
library(rstudioapi)
#library(dplyr)
#library(xlsx)
#library(tidyverse)
library('DBI')

con<-DBI::dbConnect(odbc::odbc(),
                    driver = "SQL Server",
                    server = "",
                    database = "",
                    UID = rstudioapi::askForPassword("Database user"),
                    PWD = rstudioapi::askForPassword("Database password"),
                    host = "",
                    port = 1433)

x = dbGetQuery(con, "select * from ")
