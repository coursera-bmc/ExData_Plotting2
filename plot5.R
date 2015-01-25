## Author: BMc
## Version: 1.0
## Date: 2015.01.25
## This R Module creates a plot, saved as plot5.png in the working directory,
## for Project 2 to answer the question: How have total emissions from a specific 
## source changed from 1999 - 2008 within a specifc city.
## The R File contains three functions: plot5Full(), producePlot5(), and plot5()
## plot5Full is a wrapper for producePlot5 which includes data loading
## producePlot5 accepts NEI and SCC data as an argument, skipping the loading 
## process
## Both functions will return summarized data, aiding in troubleshooting and 
## additional analysis.
## Plot5 produces the actual plot

plot5Full <- function()
{
  #This function covers loading of data as well as graphing
  library(plyr)
  library(dplyr)
  
  NEI <- tbl_df(readRDS("summarySCC_PM25.rds"))
  SCC <- tbl_df(readRDS("Source_Classification_Code.rds"))
  
  return(producePlot5(NEI, SCC, ".*Vehicle.*", "24510"))
}


producePlot5 <- function(NEI, SCC, pattern=".*Vehicle.*", fipsValue = "24510")
{
  # This function takes NEI data as an argument, skipping the long loading process
  # Defaults to Vehicle Data relating to the city of Baltimore
  
  # Select only those records in SCC which have a specific pattern as part of 
  # the EI.Sector column
  subset_SCC <- SCC[grepl(pattern, SCC$EI.Sector, ignore.case=TRUE), ]
  
  # Select only those records in NEI which belong to the specific city
  # This works on vectors of values
  subset_NEI <- NEI[NEI$fips %in% fipsValue,]
  
  # The SCC column is used in both the NEI and SCC data tables as an identifier
  # for what is being measured.  Performing an inner join removes all records
  # from NEI and SCC which do not share a value in the SCC column
  joined_data <- inner_join(subset_NEI[,], 
                            subset_SCC[,c("SCC", "Short.Name")], by = c("SCC"))
  return(plot5(joined_data, 
        "Emissions from Motor Vehicle Sources in Baltimore City from 1999-2008"))
}


plot5 <- function(NEI_data, title)
{
  # Assuming fully joined information, can skip the bottleneck of doing an inner
  # join
  # Load libraries if not present already
  library(plyr)
  library(dplyr)
  
  # Summarize the emissions by year
  summarized <- ddply(NEI_data, .(year), summarize, 
                      TotalEmissions=sum(Emissions))
  
  
  png(filename = "plot5.png", width = 600, height = 480, units = "px")
  
  #Plot the Year vs Total Emissions to a png file
  plot(summarized$year, summarized$TotalEmissions, 
       type='b', 
       main=title, 
       xlab="Year",
       ylab="Total Emission level")
  
  #Add a linear regression line in order to easily see the trend
  abline(lm(summarized$TotalEmissions ~ summarized$year), 
         lty="dotted", col="red", lwd=2)
  
  dev.off()
  return(summarized)
}
