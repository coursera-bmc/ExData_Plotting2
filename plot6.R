## Author: BMc
## Version: 1.0
## Date: 2015.01.25
## This R Module creates a plot, saved as plot6.png in the working directory,
## for Project 2 to answer the question: How have total emissions from a specific 
## source changed from 1999 - 2008 within Los Angeles and Baltimore City.
## The R File contains three functions: plot6Full(), producePlot6(), and plot6()
## plot6Full is a wrapper for producePlot5 which includes data loading
## producePlot6 accepts NEI and SCC data as an argument, skipping the loading 
## process
## Both functions will return summarized data, aiding in troubleshooting and 
## additional analysis.
## Plot6 produces the actual plot



plot6Full <- function()
{
  #This function covers loading of data as well as graphing
  library(plyr)
  library(dplyr)
  
  NEI <- tbl_df(readRDS("summarySCC_PM25.rds"))
  SCC <- tbl_df(readRDS("Source_Classification_Code.rds"))
  
  return(producePlot6(NEI, SCC, ".*Vehicle.*", c("24510","06037")))
}

producePlot6 <- function(NEI, SCC, pattern=".*Vehicle.*", 
                         fipsValue = c("24510","06037"))
{
  # Select only those records in SCC which have a specific pattern as part of 
  # the EI.Sector column
  subset_SCC <- SCC[grepl(pattern, SCC$EI.Sector, ignore.case=TRUE),]
  
  # Select only those records in NEI which belong to the specific city
  # This works on vectors of values
  subset_NEI <- NEI[NEI$fips %in% fipsValue,]
  
  joined_data <- inner_join(subset_NEI[,], 
                            subset_SCC[,c("SCC", "Short.Name")], by = c("SCC"))
  
  # Add Location names
  location_data <- data.frame(Location=c("Baltimore City, MD", "Los Angeles, CA"), 
                              fips=c("24510", "06037"))
  joined_data <- inner_join(joined_data, location_data, by="fips")
  return(plot6(joined_data, 
               "Emissions for Vehicles in Baltimore vs Los Angeles from 1999 - 2008"))
}

plot6 <- function(NEI_data, title)
{
  library(plyr)
  library(dplyr)
  library(ggplot2)
    
  png(filename = "plot6.png", width = 600, height = 480, units = "px")
  
  tidy_df <- aggregate(NEI_data[,"Emissions"], list(Location = NEI_data$Location, 
                                                  Year = NEI_data$year), sum)
  tidy_df <- tidy_df[order(tidy_df$Location,tidy_df$Year),]
  
  nei_plot <- qplot(tidy_df$Year, tidy_df$Emissions, data = tidy_df, 
                    xlab="Year", ylab="Emissions", 
                    main=title,
                    color=tidy_df$Location, type='l')
  nei_plot <- nei_plot + stat_smooth(method="lm", se=FALSE) + labs(color="Location")
  print(nei_plot)
  
  dev.off()
}
