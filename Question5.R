# Coursera JHU Exploratory Data Analysis
# Project 2
library(dplyr)

# Load data sets
directory <- paste(getwd(), "Data", sep = "/")
figDirectory = paste(getwd(), "Figures", sep = "/")
pm25 <- readRDS(paste(directory, "summarySCC_PM25.rds", sep = "/"))
sourceClass <- readRDS(paste(directory, "Source_Classification_Code.rds",
                             sep = "/"))

# Question 5: How have emissions from motor vehicle sources changed from
# 1999–2008 in Baltimore City?

# Select sources with EI.Sector indicating motor vehicles - both on and
# off road vehicles. Do not include aircraft, marine vessels, etc.
motorVehicles <- filter(sourceClass,
                        grepl("^Mobile - On-Road", sourceClass$EI.Sector) |
                            grepl("^Mobile - Non-Road", sourceClass$EI.Sector))

# Summarise emissions by year and SCC for Baltimore City, MD readings
tempQ5 <- pm25 %>%
    filter(fips == "24510") %>%
    group_by(SCC, year) %>%
    summarise(sum(Emissions))

# Set the data frame names
names(tempQ5) <- c("SCC", "Year", "Emissions")

# Join the sector data with the emission summaries.
# Only include rows where the SCC was selected by sector and
# there is an emissions value
q5 <- inner_join(motorVehicles[1], tempQ5, by = "SCC")

# Set the data frame names
names(q5) <- c("SCC", "Year", "Emissions")

# Summarise emissions by year
q5 <- q5 %>% group_by(Year) %>% summarise(sum(Emissions))

# Set the data frame names
names(q5) <- c("Year", "Emissions")

# Open the output png file. Plot the emissions by year, box the graph,
# add a red regression line to show the trend, label the axis ticks and
# axes
png(file = paste(figDirectory, "Question5.png", sep = "/"),
    bg = "transparent")
plot(q5$Year, q5$Emissions, pch = 19, col = "blue", axes = FALSE,
     xlab = NA, ylab = NA)
box()
abline(lm(Emissions ~ Year, q5), lwd = 2, col = "red")
axis(side = 1, at = c(1999, 2002, 2005, 2008))
axis(side = 2, at = c(150, 275, 400))
mtext(side = 1, "Year", line = 2)
mtext(side = 2, "Emissions in Baltimore City, MD", line = 2)
mtext(side = 2, "PM2.5 Motor Vehicle", line = 3)
dev.off()
