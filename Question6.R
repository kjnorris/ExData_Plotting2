# Coursera JHU Exploratory Data Analysis
# Project 2
library(dplyr)
library(ggplot2)

# Load data sets
directory <- paste(getwd(), "Data", sep = "/")
figDirectory = paste(getwd(), "Figures", sep = "/")
pm25 <- readRDS(paste(directory, "summarySCC_PM25.rds", sep = "/"))
sourceClass <- readRDS(paste(directory, "Source_Classification_Code.rds",
                             sep = "/"))

# Question 6: Compare emissions from motor vehicle sources in Baltimore City
# with emissions from motor vehicle sources in Los Angeles County, California
# (fips == 06037). Which city has seen greater changes over time in motor
# vehicle emissions?

# Select sources with EI.Sector indicating motor vehicles - both on and
# off road vehicles. Do not include aircraft, marine vessels, etc.
motorVehicles <- filter(sourceClass,
                        grepl("^Mobile - On-Road", sourceClass$EI.Sector) |
                            grepl("^Mobile - Non-Road", sourceClass$EI.Sector))

# Summarise emissions by year and SCC for Baltimore City, MD
# and Los Angeles, CA readings
tempQ6 <- pm25 %>%
    filter(fips == "24510" | fips == "06037") %>%
    group_by(fips, SCC, year) %>%
    summarise(sum(Emissions))

# Convert fips to a factor with understandable labels
tempQ6$fips <- factor(tempQ6$fips, levels = c("06037", "24510"),
                      labels = c("Los Angeles, CA", "Baltimore City, MD"))

# Set the data frame names
names(tempQ6) <- c("fips", "SCC", "Year", "Emissions")

# Join the sector data with the emission summaries.
# Only include rows where the SCC was selected by sector and
# there is an emissions value
q6 <- inner_join(motorVehicles[1], tempQ6, by = "SCC")

# Summarise emissions by year
q6 <- q6 %>% group_by(fips, Year) %>% summarise(sum(Emissions))

# Set the data frame names
names(q6) <- c("fips", "Year", "Emissions")

# Open the output png file. Start the ggplot: select data frame and
# aesthetics to graph (year and emissions), add the points colored by fips,
# add a regression line to show the trend, facet by fips for clarity,
# add the axis labels and title, and fix the axis ticks
png(file = paste(figDirectory, "Question6.png", sep = "/"),
    bg = "transparent")
ggplot(q6, aes(Year, Emissions)) +
    geom_point(aes(color = fips), show_guide = FALSE) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_grid(fips ~ ., scales = "free_y") +
    xlab("Year") + ylab("PM2.5 Motor Vehicle Emissions") +
    ggtitle("PM2.5 Motor Vehicle Emissions by City 1999 - 2008") +
    scale_x_continuous(breaks = c(1999, 2002, 2005, 2008))
dev.off()
