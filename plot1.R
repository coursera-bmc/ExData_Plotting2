## Author: BMc
## Version: 1.0
## Date: 2015.01.25
## This R Module creates a plot, saved as plot1.png in the working directory,
## for Project 2 to answer the question: How have total emissions in the USA 
## changed from 1999 - 2008
## The R File contains two functions: plot1Full() and plot1()
## plot1Full is a wrapper for plot1 which includes data loading
## plot1 accepts NEI data as an argument, skipping the loading process
## Both functions will return summarized data, aiding in troubleshooting and 
## additional analysis.

plot1Full <- function()
{
  #This function covers loading of data as well as graphing
  library(plyr)
  library(dplyr)
  
  NEI <- tbl_df(readRDS("summarySCC_PM25.rds"))
  return(plot1(NEI))
}

plot1 <- function(NEI_data)
{
  # This function will graph already loaded NEI data
  # This function is more useful when having to do a lot of different graphs - 
  # The loading of NEI data is a bottleneck that is best avoided
  
  # Load libraries for better tables (assume not loaded)
  library(plyr)
  library(dplyr)
  
  #Data frame created as a summary of NEI data.  Emissions are summed for each
  # Year
  
  summarized <- ddply(NEI_data, .(year), summarize, 
                      TotalEmissions=sum(Emissions))
  
  
  png(filename = "plot1.png", width = 600, height = 480, units = "px")
  
  #Plot the Year vs Total Emissions to a png file
  plot(summarized$year, summarized$TotalEmissions, 
       type='b', 
       main="Total Emissions in the US from 1999 - 2008", 
       xlab="Year",
       ylab="Total Emission level", 
       ylim=c(0,10000000), 
       xlim=c(1999,2008))
  
  #Add a linear regression line in order to easily see the trend
  abline(lm(summarized$TotalEmissions ~ summarized$year), 
         lty="dotted", col="red", lwd=2)
  
  dev.off()
  return(summarized)
}
