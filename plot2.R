## Author: BMc
## Version: 1.0
## Date: 2015.01.25
## This R Module creates a plot, saved as plot2.png in the working directory,
## for Project 2 to answer the question: How have total emissions in a specific
## area changed from 1999 - 2008
## The R File contains two functions: plot2Full() and plot2()
## plot2Full is a wrapper for plot2 which includes data loading
## plot2 accepts NEI data as an argument, skipping the loading process
## Both functions will return summarized data, aiding in troubleshooting and 
## additional analysis.
## Plot2 has been designed to accept variables for fips and place names, making
## it extensible.  plot2Full() is hard coded for Baltimore City values


plot2Full <- function()
{
  #This function covers loading of data as well as graphing
  library(plyr)
  library(dplyr)
  
  NEI <- tbl_df(readRDS("summarySCC_PM25.rds"))
  return(plot2(NEI, "24510", "Baltimore City, MD"))
}

plot2 <- function(NEI_data, fipsValue, place)
{
  # This function was written for easy reuse by simply changing the fips value
  # This function will graph already loaded NEI data
  # This function is more useful when having to do a lot of different graphs - 
  # The loading of NEI data is a bottleneck that is best avoided
  
  # Load libraries for better tables (assume not loaded)
  library(plyr)
  library(dplyr)
  
  # Slice data frame on the fips value, sum each year's emissions
  locationSlice <- NEI_data[NEI_data$fips == fipsValue,]
  summarized <- ddply(locationSlice, .(year), summarize, 
                      TotalEmissions=sum(Emissions))
  
  
  png(filename = "plot2.png", width = 600, height = 480, units = "px")
  
  #Plot the Year vs Total Emissions to a png file
  plot(summarized$year, summarized$TotalEmissions, 
       type='b', 
       main=paste0("Total Emissions in ", place, " from 1999 - 2008"), 
       xlab="Year",
       ylab="Total Emission level", ylim=c(0,3500), xlim=c(1999,2008))
  
  #Add a linear regression line in order to easily see the trend

  abline(lm(summarized$TotalEmissions ~ summarized$year), 
       lty="dotted", col="red", lwd=2)
  dev.off()
  return(summarized)
}
