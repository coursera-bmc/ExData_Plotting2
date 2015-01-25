## Author: BMc
## Version: 1.0
## Date: 2015.01.25
## This R Module creates a plot, saved as plot4.png in the working directory,
## for Project 2 to answer the question: How have total emissions from a specific 
## source changed from 1999 - 2008 throughout the entire USA.
## The R File contains three functions: plot4Full(), producePlot4(), and plot4()
## plot4Full is a wrapper for producePlot4 which includes data loading
## producePlot4 accepts NEI and SCC data as an argument, skipping the loading 
## process
## Both functions will return summarized data, aiding in troubleshooting and 
## additional analysis.
## Plot4 produces the actual plot

plot4Full <- function()
{
  #This function covers loading of data as well as graphing
  library(plyr)
  library(dplyr)
  
  NEI <- tbl_df(readRDS("summarySCC_PM25.rds"))
  SCC <- tbl_df(readRDS("Source_Classification_Code.rds"))
  
  return(producePlot4(NEI, SCC, ".*Coal.*"))
}

producePlot4 <- function(NEI, SCC, pattern=".*Coal.*")
{
  #Because loading of NEI takes a while, this function takes the data frame as
  # an argument, moving the bottleneck to a different part of the process
  
  # Select only those records in SCC which have a specific pattern as part of 
  # the EI.Sector column
  subset_SCC <- SCC[grepl(pattern, SCC$EI.Sector, ignore.case=TRUE),]

  # The SCC column is used in both the NEI and SCC data tables as an identifier
  # for what is being measured.  Performing an inner join removes all records
  # from NEI and SCC which do not share a value in the SCC column
  # Note - has to be a more efficient way to do this.  Takes most time by far.
  
  joined_data <- inner_join(NEI[,], subset_SCC[,c("SCC", "EI.Sector")], by = c("SCC"))

  return(plot4(joined_data, 
               "Coal Combustion based Emissions in the US from 1999-2008"))
}

plot4 <- function(NEI_data, title)
{
  # Assuming fully joined information, can skip the bottleneck of doing an inner
  # join
  # Load libraries if not present already
  library(plyr)
  library(dplyr)
  
  # Summarize the emissions by year
  summarized <- ddply(NEI_data, .(year), summarize, 
                      TotalEmissions=sum(Emissions))
  
  
  png(filename = "plot4.png", width = 600, height = 480, units = "px")
  
  #Plot the Year vs Total Emissions to a png file
  plot(summarized$year, summarized$TotalEmissions, 
       type='b', 
       main=title, 
       xlab="Year",
       ylab="Total Emission level", 
       ylim=c(0,700000), xlim=c(1999,2008))
  
  #Add a linear regression line in order to easily see the trend
  abline(lm(summarized$TotalEmissions ~ summarized$year), 
         lty="dotted", col="red", lwd=2)
  
  dev.off()
  return(summarized)
}
