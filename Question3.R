# Coursera JHU Exploratory Data Analysis
# Project 2
library(dplyr)
library(ggplot2)

# Load data sets
directory <- paste(getwd(), "Data", sep = "/")
figDirectory = paste(getwd(), "Figures", sep = "/")
pm25 <- readRDS(paste(directory, "summarySCC_PM25.rds", sep = "/"))
sourceClass <- readRDS(paste(directory, "Source_Classification_Code.rds", sep = "/"))

# Question 3: Of the four types of sources indicated by the type (point,
# nonpoint, onroad, nonroad) variable, which of these four sources have seen
# decreases in emissions from 1999–2008 for Baltimore City? Which have seen
# increases in emissions from 1999–2008? Use the ggplot2 plotting system to
# make a plot answer this question.

# Sum the emissions by year
q3 <- pm25 %>%
    filter(fips == "24510") %>%
    group_by(type, year) %>%
    summarise(sum(Emissions))

# Set the data frame names
names(q3) <- c("Type", "Year", "Emissions")

# Open the output png file. Start the ggplot: select data frame and
# aesthetics to graph (year and emissions), add the points colored by type,
# add a regression line to show the trend, facet by type for clarity,
# add the axis labels and title, and fix the axis ticks
png(file = paste(figDirectory, "Question3.png", sep = "/"),
    bg = "transparent")
ggplot(q3, aes(Year, Emissions)) +
    geom_point(aes(color = Type)) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_grid(Type ~ ., scales = "free_y") +
    xlab("Year") + ylab("PM2.5 Emissions in Baltimore City, MD") +
    ggtitle("PM2.5 Emissions by Type 1999 - 2008") +
    scale_x_continuous(breaks = c(1999, 2002, 2005, 2008))
dev.off()
