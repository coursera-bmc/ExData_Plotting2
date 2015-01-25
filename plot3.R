## Author: BMc
## Version: 1.0
## Date: 2015.01.25
## This R Module creates a plot, saved as plot3.png in the working directory,
## for Project 2 to answer the question: How have total emissions in a specific 
## area changed from 1999 - 2008, broken out by the type of measurement taken.
## The R File contains two functions: plot3Full() and plot3()
## plot3Full is a wrapper for plot3 which includes data loading
## plot3 accepts NEI data as an argument, skipping the loading process
## Both functions will return summarized data, aiding in troubleshooting and 
## additional analysis.
## Plot3 has been designed to accept variables for fips and place names, making
## it extensible.  plot3Full() is hard coded for Baltimore City values

plot3Full <- function()
{
  #This function covers loading of data as well as graphing
  library(plyr)
  library(dplyr)
  
  NEI <- tbl_df(readRDS("summarySCC_PM25.rds"))
  return(plot3(NEI, "24510", "Baltimore City, MD"))
}

plot3 <- function(NEI_data, fipsValue, place)
{
  # This function was written for easy reuse by simply changing the fips value
  # This function will graph already loaded NEI data
  # This function is more useful when having to do a lot of different graphs - 
  # The loading of NEI data is a bottleneck that is best avoided
  
  # Load libraries for better tables and graphs (assume not loaded)
  
  library(plyr)
  library(dplyr)
  library(ggplot2)
  
  # Slice data frame on the fips value, sum each year's emissions
  loc_df <- NEI_data[NEI_data$fips == fipsValue,]
  # Aggregation done on Emissions column by summing unique records with same
  # Type of Emission (POINT, NONPOINT, ONROAD, NONROAD) and Year
  tidy_df <- aggregate(loc_df[,"Emissions"], list(Type = loc_df$type, 
                                                  Year = loc_df$year), sum)
  
  # Order by Type and then Year
  tidy_df <- tidy_df[order(tidy_df$Type,tidy_df$Year),]
  
  png(filename = "plot3.png", width = 800, height = 600, units = "px")
  
  # Create plot (x-y scatter)
  nei_plot <- qplot(tidy_df$Year, tidy_df$Emissions, data = tidy_df, 
        xlab="Year", ylab="Emissions", 
        main=paste0("Emissions for ", place, " from 1999 - 2008 by Type"))
  
  #Separate plot into 4 subplots, each graphing the yearly trend for a different 
  #type of emissions measurement.  Add Linear Regression lines to each subplot
  #Print to png file  
  nei_plot <- nei_plot + stat_smooth(method="lm", se=FALSE) + facet_grid(. ~ Type)
  print(nei_plot)
  dev.off()
  
  return(tidy_df)
}
