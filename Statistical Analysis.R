bike.share <- read.csv("C:/Users/rbbra/Documents/R/Capstone/DC-Bike-Share-Data/Bike-Sharing-Dataset/day.csv")
hourly <- read.csv("C:/Users/rbbra/Documents/R/Capstone/DC-Bike-Share-Data/Bike-Sharing-Dataset/hour.csv") ## Import dataset
names(bike.share) ##names of the columns

bike.share$dteday <- as.Date(bike.share$dteday)
bike.share$weathersit <- factor(bike.share$weathersit)
bike.share$yr <- factor(bike.share$yr)
bike.share$mnth <- factor(bike.share$mnth)
bike.share$holiday <- factor(bike.share$holiday)
bike.share$weekday <- factor(bike.share$weekday)
bike.share$workday <- factor(bike.share$workday)


library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggpmisc)

sum.casual <- sum(bike.share$casual)
sum.reg <- sum(bike.share$reg)
sum.total <- sum(bike.share$cnt)

################# VISUALIZE ##################

# Histogram showing the proportion of each weather event in each season
ggplot(bike.share, aes(x = as.factor(weathersit), fill = factor(season), stat="count"))+
  geom_bar(aes(y = ..count../sum(..count..))) +
  labs(x = "Weather Condition", y = "Proportion of Events")

# pie chart of weather events in each season
ggplot(bike.share, aes(x=1, fill = factor(weathersit)))+
  geom_bar()+
  coord_polar(theta = "y")+
  facet_grid(season~.)

#Scatter plot displaying total number of riders on each day, colored by temperature (blue)
ggplot(bike.share, aes(x=dteday, y=cnt, col = tempF))+
  geom_point()+
  labs(x="Date", y = "Total Riders")

#scatter plot breaking out number of riders by weather condition, including line of best fit with SE
ggplot(bike.share, aes(x=dteday, y=cnt, col = tempF))+
  geom_point()+
  labs(x="Date", y = "Total Riders")+
  scale_color_gradientn(colors = brewer.pal(9, "YlOrRd"))+
  facet_grid(~weathersit)+
  theme_minimal()+
  stat_smooth(method = "lm", formula =y~x)+
  theme(axis.title.x = element_text(angle = 90, hjust = 1))+
  stat_poly_eq(parse =T, aes(label= ..eq.label..), formula = y~x)
  

ggplot(bike.share, aes(x = as.factor(cnt), fill = factor(hour), stat="count"))+
  geom_bar(aes(y = ..count../sum(..count..))) +
  labs(x = "Weather Condition", y = "Proportion of Events")

ggplot(hourly, aes(x=hr, y=(cnt/1000), col = factor(yr)))+
       geom_histogram(bins = 24, stat="identity")+
       scale_x_continuous(labels = scales::comma)+
       labs(x = "Hour of Day", y = "Total Riders (in 1000s)")

################## STATISTICS ####################

# Non-stationary time series model

require(astsa)
