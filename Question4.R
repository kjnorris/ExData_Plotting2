# Coursera JHU Exploratory Data Analysis
# Project 2
library(dplyr)
library(ggplot2)

# Load data sets
directory <- paste(getwd(), "Data", sep = "/")
figDirectory = paste(getwd(), "Figures", sep = "/")
pm25 <- readRDS(paste(directory, "summarySCC_PM25.rds", sep = "/"))
sourceClass <- readRDS(paste(directory, "Source_Classification_Code.rds", sep = "/"))

# Question 4: Across the United States, how have emissions from coal
# combustion-related sources changed from 1999–2008?

# Select sources with SCC.Level.Three or SCC.Level.Four that contain the
# world coal - 230 SCC codes
coalSector <- filter(sourceClass,
                     grepl("Coal", sourceClass$SCC.Level.Three) |
                         grepl("Coal", sourceClass$SCC.Level.Four))

# Summarise emissions by year and SCC
tempQ4 <- pm25 %>%
    group_by(SCC, year) %>%
    summarise(sum(Emissions))

# Set the data frame names
names(tempQ4) <- c("SCC", "Year", "Emissions")

# Join the sector data with the emission summaries.
# Only include rows where the SCC was selected by sector and
# there is an emissions value
q4 <- inner_join(coalSector[1], tempQ4, by = "SCC")

# Set the data frame names
names(q4) <- c("SCC", "Year", "Emissions")

# Summarise emissions by year
q4 <- q4 %>% group_by(Year) %>% summarise(sum(Emissions))

# Set the data frame names
names(q4) <- c("Year", "Emissions")

# Open the output png file. Plot the emissions by year, box the graph,
# add a red regression line to show the trend, label the axis ticks and
# axes
png(file = paste(figDirectory, "Question4.png", sep = "/"),
    bg = "transparent")
plot(q4$Year, q4$Emissions, pch = 19, col = "blue", axes = FALSE,
     xlab = NA, ylab = NA)
box()
abline(lm(Emissions ~ Year, q4), lwd = 2, col = "red")
axis(side = 1, at = c(1999, 2002, 2005, 2008))
axis(side = 2, at = c(360000, 475000, 590000))
mtext(side = 1, "Year", line = 2)
mtext(side = 2, "Emissions in the United States", line = 2)
mtext(side = 2, "PM2.5 Coal Combustion", line = 3)
dev.off()
