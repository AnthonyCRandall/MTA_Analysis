# This file will be used to get us started for our MTA questions

library(tidyverse)
library(lubridate)
library(tidyr)

daily_df <- read.csv("Daily_Traffic_on_MTA_Bridges___Tunnels_20241028.csv")

updatedColNames <- c("Date", "ID", "Drct", "EZPass", "Cash" )
colnames(daily_df) <- updatedColNames

getID <- function(id){
  newID = case_when (
    id == 1 | id == 21 ~ "TBX",
    id == 2 | id == 22 ~ "TBM",
    id == 3 | id == 23 ~ "BWB",
    id == 4 | id == 24 ~ "HHB",
    id == 5 | id == 25 ~ "MPB",
    id == 6 | id == 26 ~ "CBB",
    id == 7 | id == 27 ~ "QMT",
    id == 8 | id == 28 ~ "HLC",
    id == 9 | id == 29 ~ "TNB",
    id == 11 | id == 30 ~ "VNB",
    TRUE ~ "NAC")
  return (newID)
}
