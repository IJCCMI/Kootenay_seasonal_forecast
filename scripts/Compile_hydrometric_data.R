# -------------------------------------------------------------------------
# Date created:      2024-04-22
# Last modified:     2024-06-06
# Authors:           K. Alexander
# Description:       This script compiles the Kootenay Lake Inflow data.
#
# Scripts Utilized:  None
#
# Required input(s):  Historical_FortisBC_Kootenay_Lake_Data_1928-%s.csv
#
#
#
# Outputs:        Historical_Apr1toJul31_total_KL_Inflow_volume.csv
#
#
#
# Note(s): Run this script if you are doing the analysis for the first time each year. Requires information from Fortis BC.
# Being able to access historical inflow data from Fortis is a current issue, one solution is to be sent the file or run the BC scripts.*** will fix this soon.
#
# -------------------------------------------------------------------------


library(lubridate)
library(dbplyr)
library(tidyr)
library(tidyverse)
library(tidyhydat)
library(tidyhydat.ws)

Script_Location <- sprintf("C:/Users/%s/R Projects/BC_Hydrometric_Conditions", user) # Location of BC_Hydrometric_Conditions GitHub repository
Basin_Location <- "C:/Basins" # path to Boundary Waters / BC Boards plots folder

setwd(Basin_Location)

year <- year(Sys.Date()) # Sets year to current year
water_year <- ifelse(month(Sys.Date()) >= 10,year+1,year)

dir.create(sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow", year)) # creates plots folder in current year if needed



##### Hydrometric Calculations ##########################################################


#######Historical Kootenay Lake Inflow ############
KL_data <- read.csv(file = sprintf("IJC Kootenay Board/%s/Data Computations/R_Data_Historical/Historical_FortisBC_Kootenay_Lake_Data_1928-%s.csv", year, year-1))

lake_inflow <- KL_data %>% mutate(KL.Inflow.cms = KL.Inflow.kcfs*(0.0283168*1000))

AprtoJul_flow <- lake_inflow %>% filter(month(Date)>=4 & month(Date)<=7)%>%
  mutate(Total_Vol.m3 = KL.Inflow.cms*86400, Year = year(Date)) #daily water volume that is not associated with base flow

##Calculate the yearly April-July total volume (excluding baseflow) of water passing through the tributary
Year_Summary <- AprtoJul_flow %>%
  group_by(Year)%>%
  summarise(Year = Year[1],Total_Volume.km3 = sum(Total_Vol.m3)/10^9 ) %>%
  arrange(Year)

write.csv(Year_Summary, sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/Data_Computations/Historical_Apr1toJul31_total_KL_Inflow_volume.csv", year), row.names = FALSE)

##Not required, but a visualization of the historical yearly inflows
# ggplot(Year_Summary, aes(Year, Total_Volume.km3)) +
#   theme_bw()+
#   geom_bar(stat="identity")+
#   scale_x_continuous(name = NULL) +
#   scale_y_continuous( name = expression(`April to July Inflow `~(km^3)), expand = expansion(c(0.01,0.05),c(1,1)), labels = scales::comma_format(accuracy = 1), breaks = seq(0,24, by=2))


##### Current Year Inflow ##############

## Run if the update has not been run this year, requires the Fortis Data Sheets
source(sprintf("%s/Kootenay/Code/Kootenay Lake/Compile_Kootenay_Current_Data.R", Script_Location))

Curr_KL_data <- read.csv(file = sprintf("IJC Kootenay Board/%s/Data Computations/R_Data_CY_DM/%s_FortisBC_Kootenay_Lake_Data.csv", year, year))

Curr_lake_inflow <- Curr_KL_data %>% mutate(KL.Inflow.cms = KL.Inflow.kcfs*(0.0283168*1000))

Curr_AprtoJul_flow <- Curr_lake_inflow %>% filter(month(Date)>=4 & month(Date)<=7)%>%
  mutate(Total_Vol.m3 = KL.Inflow.cms*86400, Year = year(Date)) #daily water

Curr_inflow_Mm3 <- sum(Curr_AprtoJul_flow$Total_Vol.m3)/(10^6)


### Option to use an unregulated tributary ### NOT coded into the plots

## need temp and precip for the advanced analysis.
## Go to https://tsamsonov.github.io/grwat/articles/grwat.html#additional-features for more information


# ## Input flow data
# #Tributary <- readline("Enter the WSC station name (use _ for any spaces): ") #Input the name of the station being analyzed
# Tributary <- "Redfish_Creek"
# Tributary_name <- "Redfish Creek"
#
# #If there are no previous saved to your computer use the following code:
# #stn_id <- readline("Enter the station ID (e.g 08NJ061): ")
# stn_id <- "08NJ061"
#
# # Pull gauge data - approved values
# hist_flow <- hy_daily(station_number = stn_id) %>%
#   mutate(agency = "WSC", QA = 'A') %>%
#   select(agency, STATION_NUMBER, Date, Parameter, Value, QA) %>%
#   rename(site_no. = STATION_NUMBER) %>%
#   filter(Parameter == "Flow" & year(Date) <= year-1) %>% rename(discharge.cms = Value)
#
# ## May want to add in a download so this can be run independently of someone's cdrive files
#
# #Redfish Creek 08NJ061
# hist_flow <- read.csv(file =  sprintf("IJC Kootenay Board/%s/Data Computations/R_Data_Historical/Historical_WSC_Redfish_Creek_Discharge_Data_1967-2023.csv", year, year-1)) %>% #may have to download data if using another staion
#   mutate(Date = as.Date(Date))
#
# baseflowdata = hist_flow %>%
#   mutate(BaseFlow.cms = gr_baseflow(discharge.cms, method = 'chapman', a = 0.98), #other method options are 'chapman', 'jakeman' and 'lynehollick'
#          Otherflow.cms = discharge.cms - BaseFlow.cms) #rename
#
# # Visualization, not necessary, change dates
# bfp <- ggplot(baseflowdata) +
#   geom_area(aes(Date, discharge.cms), fill = 'steelblue', color = 'black') +
#   geom_area(aes(Date, BaseFlow.cms), fill = 'orangered', color = 'black') +
#   scale_x_date(limits = c(ymd(20020101), ymd(20031231)))
# bfp
#
# ggsave(sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/BaseFlow_%s_Plot.png", year, Tributary), bfp, width = 12, height = 12)
#
#
# ## Filter for the April 1 and July 31 flow
# AprtoJul_flow <- baseflowdata %>% filter(month(Date)>=4 & month(Date)<=6)%>%
#   mutate(Total_Vol.m3 = Otherflow.cms*86400, Year = year(Date)) #daily flow volume that is not associated with base flow
#
# ##Calculate the yearly April-July total volume (excluding base flow) of water passing through the tributary
# Year_Summary <- AprtoJul_flow %>%
#   group_by(Year)%>%
#   summarise(Year = Year[1],Total_Volume.m3 = sum(Total_Vol.m3)) %>%
#   arrange(Year)
#
# write.csv(Year_Summary, sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/Historical_%s_AprtoJul_total_volume.csv", year, Tributary), row.names = FALSE)
#
# ggplot(Year_Summary, aes(Year, Total_Volume.m3)) +
#   geom_bar(stat="identity")

