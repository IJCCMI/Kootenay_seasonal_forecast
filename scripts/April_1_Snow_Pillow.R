# -------------------------------------------------------------------------
# Date created:      2024-04-22
# Last modified:     2024-04-25
# Authors:           K. Alexander
# Description:       This script will compile the April 1 SWE data &
#                    compute hydrological freshet calculations
#
#
# Note(s): Master script, to change the stations used in the SWE estimate see the Plot_Inflow_vs_Basin_SWE.R script
#
# -------------------------------------------------------------------------


library(lubridate)
library(dbplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(grwat)
library(tidyhydat)
library(tidyhydat.ws)
library(ggpubr)

user <- "AlexanderK" # ***Needs to be changed depending on the user !!***


Script_Location <- sprintf("C:/Users/%s/R Projects/BC_Hydrometric_Conditions", user) # Location of BC_Hydrometric_Conditions GitHub repository
Basin_Location <- "C:/Basins" # path to Boundary Waters / BC Boards plots folder

setwd(Basin_Location)

year <- year(Sys.Date()) # Sets year to current year
water_year <- ifelse(month(Sys.Date()) >= 10,year+1,year)

dir.create(sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow", year)) # creates plots folder in current year if needed


#Compile the SWE data
source(sprintf("%s/Kootenay/Snow Pillow Analysis/Compile_April_1_SWE_data.R", Script_Location))

#Compile the hydrometric data, requires the Fortis data sheets
source(sprintf("%s/Kootenay/Snow Pillow Analysis/Compile_hydrometric_data.R", Script_Location))

#Correlation and Plots
source(sprintf("%s/Kootenay/Snow Pillow Analysis/Plot_Inflow_vs_Basin_SWE.R", Script_Location))
