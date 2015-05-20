# Coursera JHU Exploratory Data Analysis
# Project 2
library(dplyr)

# Load data sets
directory <- paste(getwd(), "Data", sep = "/")
figDirectory = paste(getwd(), "Figures", sep = "/")
pm25 <- readRDS(paste(directory, "summarySCC_PM25.rds", sep = "/"))
sourceClass <- readRDS(paste(directory, "Source_Classification_Code.rds",
                             sep = "/"))

# Question 2: Have total emissions from PM2.5 decreased in the
# Baltimore City, Maryland ( fips == "24510" ) from 1999 to 2008?
# Use the base plotting system to make a plot answering this question.

# Sum the emissions in Baltimore City, MD (fips - 24510) by year
q2 <- pm25 %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarise(sum(Emissions))

# Set the data frame names
names(q2) <- c("Year", "Emissions")

# Open the output png file. Plot the emissions by year, box the graph,
# add a red regression line to show the trend, label the axis ticks and
# axes
png(file = paste(figDirectory, "Question2.png", sep = "/"),
    bg = "transparent")
plot(q2$Year, q2$Emissions, pch = 19, col = "blue", axes = FALSE,
     xlab = NA, ylab = NA)
box()
abline(lm(Emissions ~ Year, q2), lwd = 2, col = "red")
axis(side = 1, at = c(1999, 2002, 2005, 2008))
axis(side = 2, at = c(2000, 2500, 3000))
mtext(side = 1, "Year", line = 2)
mtext(side = 2, "Baltimore City, MD", line = 2)
mtext(side = 2, "PM2.5 Emissions in", line = 3)
dev.off()

