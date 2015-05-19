# Coursera JHU Exploratory Data Analysis
# Project 2
library(dplyr)
library(tidyr)

# Load data sets
directory <- paste(getwd(), "Data", sep = "/")
figDirectory = paste(getwd(), "Figures", sep = "/")
pm25 <- readRDS(paste(directory, "summarySCC_PM25.rds", sep = "/"))
sourceClass <- readRDS(paste(directory, "Source_Classification_Code.rds", sep = "/"))

# Question 1: Has total emissions from PM2.5 decreased in the US between
# 1999 and 2008? Use the base plotting system.

q1 <- pm25 %>%
    group_by(year) %>%
    summarise(sum(Emissions))
names(q1) <- c("Year", "Emissions")

png(file = paste(figDirectory, "Question1.png", sep = "/"),
    bg = "transparent")
plot(q1$Year, q1$Emissions, pch = 19, col = "blue", axes = FALSE,
     xlab = NA, ylab = NA)
box()
abline(lm(Emissions ~ Year, q1), lwd = 2)
axis(side = 1, at = c(1999, 2002, 2005, 2008))
axis(side = 2, at = c(4000000, 5500000, 7000000))
mtext(side = 1, "Year", line = 2)
mtext(side = 2, "PM2.5 Emissions", line = 2)
dev.off()

# Question 2: Have total emissions from PM2.5 decreased in the
# Baltimore City, Maryland ( fips == "24510" ) from 1999 to 2008?
# Use the base plotting system to make a plot answering this question.

q2 <- pm25 %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarise(sum(Emissions))
names(q2) <- c("Year", "Emissions")

png(file = paste(figDirectory, "Question2.png", sep = "/"),
    bg = "transparent")
plot(q2$Year, q2$Emissions, pch = 19, col = "blue", axes = FALSE,
     xlab = NA, ylab = NA)
box()
abline(lm(Emissions ~ Year, q2), lwd = 2)
axis(side = 1, at = c(1999, 2002, 2005, 2008))
axis(side = 2, at = c(4000000, 5500000, 7000000))
mtext(side = 1, "Year", line = 2)
mtext(side = 2, "PM2.5 Emissions", line = 2)
dev.off()

