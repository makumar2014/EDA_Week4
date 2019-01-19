# Download the zip file from internet
fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
destFILE <- "../exdata_data_NEI_data.zip"

if(!file.exists(destFILE)) {
    download.file(fileURL, destFILE)
}

# Unzip the zip file
unzip(zipfile = destFILE, exdir = "..")

# Read the files
NEI <- readRDS("../summarySCC_PM25.rds")
SCC <- readRDS("../Source_Classification_Code.rds")

# Library files
library(dplyr)

# Q1: Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
png(file="plot1.png")
NEI %>% group_by(year) %>% summarize(sum(Emissions)) %>% 
    plot(pch=19, col="black", main="US PM2.5 Emissions", xlab = "Year", ylab = "Emissions")
dev.off()

# Q2: Have total emissions from PM2.5 decreased in the  Baltimore City (fips == 24510) from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

png(file="plot2.png")
subset(NEI, fips=="24510") %>% group_by(year) %>% summarize(sum(Emissions)) %>% 
    plot(pch=19, col="black", main="Baltimore City, MD PM2.5 Emissions", xlab = "Year", ylab = "Emissions")
dev.off()

# Q3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

library(ggplot2)
Baltimore <- subset(NEI, fips=="24510")
Baltimore <- transform(Baltimore, type = factor(type))
BGroup <- group_by(Baltimore, year, type)
BTable <- summarize (BGroup, Emission = sum(Emissions))

png(file="plot3.png")
qplot(year, Emission, data = BTable, facets = .~type)
dev.off()

# Q4: Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

SCC.Select <- SCC[grep("Coal", SCC$Short.Name), ]
NEI.Subset <- subset(NEI, NEI$SCC %in% SCC.Select$SCC)

png(file="plot4.png")
group_by(NEI.Subset, year) %>% summarize(sum(Emissions)) %>% 
    plot(pch=19, col="black", main="US emissions from coal combustion-related sources", xlab = "Year", ylab = "Emissions")
dev.off()

# Q5: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

SCC.Select <- SCC[grep("Veh", SCC$Short.Name), ]
Baltimore.Subset <- subset(NEI, NEI$fips=="24510" & NEI$SCC %in% SCC.Select$SCC)

png(file="plot5.png")
group_by(Baltimore.Subset, year) %>% summarize(sum(Emissions)) %>% 
    plot(pch=19, col="black", main="Baltimore City, MD emissions from Vehicles", xlab = "Year", ylab = "Emissions")
dev.off()

# Q6: Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in 
# Los Angeles County, California (fips==06037).
# Which city has seen greater changes over time in motor vehicle emissions?

SCC.Select <- SCC[grep("Veh", SCC$Short.Name), ]
Baltimore.Subset <- subset(NEI, NEI$fips=="24510" & NEI$SCC %in% SCC.Select$SCC)
LA.Subset <- subset(NEI, NEI$fips=="06037" & NEI$SCC %in% SCC.Select$SCC)

png(file="plot6.png")
par(mfrow = c(1,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
group_by(Baltimore.Subset, year) %>% summarize(sum(Emissions)) %>% 
    plot(pch=19, col="black", main="Baltimore Emissions", xlab = "Year", ylab = "Emissions")
group_by(LA.Subset, year) %>% summarize(sum(Emissions)) %>% 
    plot(pch=19, col="black", main="LA Emissions", xlab = "Year", ylab = "Emissions")
mtext("Trend of Vehicle Emissions in Baltimore and LA", outer = TRUE)
dev.off()
