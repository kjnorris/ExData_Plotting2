# Coursera JHU Exploratory Data Analysis
# Project 2
library(dplyr)

# Load data sets
directory <- paste(getwd(), "Data", sep = "/")
figDirectory = paste(getwd(), "Figures", sep = "/")
pm25 <- readRDS(paste(directory, "summarySCC_PM25.rds", sep = "/"))
sourceClass <- readRDS(paste(directory, "Source_Classification_Code.rds",
                             sep = "/"))

# Question 1: Have total emissions from PM2.5 decreased in the United States
# from 1999 to 2008? Using the base plotting system, make a plot showing the
# total PM2.5 emission from all sources for each of the years 1999, 2002,
# 2005, and 2008.

# Sum the emissions by year
q1 <- pm25 %>%
    group_by(year) %>%
    summarise(sum(Emissions))

# Set the data frame names
names(q1) <- c("Year", "Emissions")

# Open the output png file. Plot the emissions by year, box the graph,
# add a red regression line to show the trend, label the axis ticks and
# axes
png(file = paste(figDirectory, "Question1.png", sep = "/"),
    bg = "transparent")
plot(q1$Year, q1$Emissions, pch = 19, col = "blue", axes = FALSE,
     xlab = NA, ylab = NA)
box()
abline(lm(Emissions ~ Year, q1), lwd = 2, col = "red")
axis(side = 1, at = c(1999, 2002, 2005, 2008))
axis(side = 2, at = c(4000000, 5500000, 7000000))
mtext(side = 1, "Year", line = 2)
mtext(side = 2, "the United States", line = 2)
mtext(side = 2, "PM2.5 Emissions in", line = 3)
dev.off()
