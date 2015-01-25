loadNEI <- function()
{
  library(dplyr)
  return(loadData("summarySCC_PM25.rds"))
}

loadSCC <- function()
{
  library(dplyr)
  return(loadData("Source_Classification_Code.rds"))
}

loadData <- function(filename)
{
  return(tbl_df(readRDS(filename)))
}

