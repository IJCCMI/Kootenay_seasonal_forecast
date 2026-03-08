# -------------------------------------------------------------------------
# Date created:      2024-04-22
# Last modified:     2024-06-06
# Authors:           K. Alexander
# Description:       This script compiles the April 1st snow pillow data from the Kootenay Basin.
#
# Scripts Utilized:  None
#
# Required input(s):http://www.env.gov.bc.ca/wsd/data_searches/snow/asws/data/SW_DailyArchive.csv
#                   http://www.env.gov.bc.ca/wsd/data_searches/snow/asws/data/SW.csv
#                   https://www.env.gov.bc.ca/wsd/data_searches/snow/asws/data/allmss_archive.csv
#                   https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customWaterYearGroupByMonthReport/daily/start_of_period/787:MT:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END:M%7C4,D%7C1/WTEQ::value
#                   https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customWaterYearGroupByMonthReport/daily/start_of_period/516:MT:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END:M%7C4,D%7C1/WTEQ::value
#                   https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customWaterYearGroupByMonthReport/daily/start_of_period/918:MT:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END:M%7C4,D%7C1/WTEQ::value
#                   https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customWaterYearGroupByMonthReport/daily/start_of_period/311:MT:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END:M%7C4,D%7C1/WTEQ::value
#                   https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customWaterYearGroupByMonthReport/daily/start_of_period/1053:ID:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END:M%7C4,D%7C1/WTEQ::value
#                   https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customWaterYearGroupByMonthReport/daily/start_of_period/988:ID:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END:M%7C4,D%7C1/WTEQ::value
#
#
# Outputs:        IJC Full_Kootenay_April_1_SWE.csv
#                 Full_Kootenay_April_1_SWE_stats.csv
#
#
# Note(s): Run this script if you are doing the analysis for the first time (every year). Requires a webservice account. May add elevation to the outputs.
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


##### SWE DATA ###############################################

##### BC Automated Snow pillows ###################

##Load in the station identifiers, comment out any that are not needed

## Date Selection
mdates <- "01-Apr"


## Automatic station identifiers
stations <- c(
  "X2C10P", # Moyie Mountain
  "X2C14P", # Floe Lake
  "X2C09Q", # Morrissey Ridge
  "X2D14P", # Redfish Creek
  "X2D10P", # Gray Creek Upper
  "X2D08P", # East Creek
  "X2D07AP", # Duncan Lake Dam
  "X2B08P", # St. Leon Creek
  "X2B06P" # Barns Creek
)


## import current and historical daily SWE (to Sept 30 of previous year; water year)
hist_swe_url <- "http://www.env.gov.bc.ca/wsd/data_searches/snow/asws/data/SW_DailyArchive.csv"
curr_swe_url <- "http://www.env.gov.bc.ca/wsd/data_searches/snow/asws/data/SW.csv"


## Creating the automated snow pillow data frame with the select stations
K_swe_hist <- read.csv(hist_swe_url, stringsAsFactors = FALSE) %>%
  pivot_longer(cols = -DATE.UTC.,
               names_to = "STATION", values_to = "SWE",
               values_drop_na = TRUE) %>%
  mutate(STATION = str_replace(STATION, "\\.", "|")) %>%
  separate(col = STATION, into = c("ID", "NAME"), sep = "\\|") %>%
  filter(ID %in% stations) %>%
  rename("Date" = "DATE.UTC.") %>%
  mutate(Date = as.POSIXct(Date, tz = "UTC"),
         NAME = str_replace_all(NAME, "\\.", " "),
         Date = as.Date(Date)) %>%
  arrange(NAME)

swe_curr <- read.csv(curr_swe_url, stringsAsFactors = FALSE) %>%
  rename(DATE_TIME = DATE.UTC.) %>%
  mutate(DATE_TIME = as.POSIXct(DATE_TIME, tz = "UTC"))

K_swe_curr <- swe_curr %>%
  pivot_longer(cols = -DATE_TIME,
               names_to = "STATION",
               values_to = "SWE") %>%
  mutate(STATION = str_replace(STATION, "\\.", "|")) %>%
  separate(col = STATION, into = c("ID", "NAME"), sep = "\\|") %>%
  mutate(NAME = str_replace_all(NAME, "\\.", " ")) %>%
  filter(ID %in% stations) %>%
  mutate(Date = as.Date(DATE_TIME)) %>%
  group_by(Date,ID) %>%
  na.omit() %>%
  summarise(Date = Date[1], ID = ID[1], NAME = NAME[1], SWE = mean(SWE, na.rm = TRUE)) %>%
  # filter(Date < Sys.Date()) %>%
  arrange(Date) %>% arrange(NAME)

K_swe_hist <- K_swe_hist %>%
  rbind(K_swe_curr)

## Selecting the data from April 1
april_1_data <- K_swe_hist %>%
  filter(format(Date, "%m-%d") == "04-01")%>%
  reframe(Year = year(Date), ID , NAME , SWE , Type = "BC Automated Pillow")%>%
  arrange(NAME)


#write.csv(april_1_data, sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/Data_Computations/Automatic_BC_Kootenay_April_1_SWE_Data.csv", water_year), row.names = FALSE)


# ## compute historical SWE quantiles by calendar day of year
K_swe_normals <- april_1_data %>%
  group_by(ID) %>%
  summarise(ID = ID[1], NAME = NAME[1], year.start = min(Year), year.end = max(Year), units = "mm", mean.SWE = mean(SWE),
            min.SWE = min(SWE), max.SWE = max(SWE), SWE.90th = quantile(SWE, 0.90), SWE.75th = quantile(SWE, 0.75),
            SWE.25th = quantile(SWE, 0.25), SWE.10th = quantile(SWE, 0.10)) %>%
  arrange(NAME)

# write.csv(K_swe_normals, sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/Data_Computations/Historical_Percentiles_BC_Kootenay_April_1_SWE.csv", water_year, year(min(K_swe_hist$Date)), year(max(K_swe_hist$Date))), row.names = FALSE)



########## BC Snow Course Stations ###################

##Manual sation identifiers
mstations <- c(
  "2C17", # Thunder Creek
  "2C15", # Mount Assiniboine
  "2C04", # Sullivan Mine
  "2C11", # Kimberley (Upper)
  "2C12", # Kimberley (Middle)
  "2D06", # Char Creek
  "2D04", # Nelson
  "2D18", # Purcell
  "2D17", # Lost Ledge
  "2D03", # Sandon
  "2D07A",# Duncan Lake No. 2
  "2D09", # Mount Templeton
  "2D02", # Ferguson
  "2B09", # Record Mountain
  "2B02A", # Farron
  "2B07", # Koch Creek
  "2B05" #Whatshan Upper
)


manual_hist_swe_url <- "https://www.env.gov.bc.ca/wsd/data_searches/snow/asws/data/allmss_archive.csv"


## Reading in the full manual csv file (large)
K_swe_hist_manual <- read.csv(manual_hist_swe_url, stringsAsFactors = FALSE) #%>%

## Selecting the stations and dates from the full snow course file, also removing any years that no measurements were made.
manual_april_1_data <- K_swe_hist_manual %>%
  filter(Number %in% mstations, Survey.Period %in% mdates, Survey.Code != "N")%>%
  reframe(Year = year(Date.of.Survey), ID = Number, NAME = Snow.Course.Name, SWE = Water.Equiv..mm, Type = "BC Snow Course")


#write.csv(manual_april_1_data, sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/Historical_BC_Kootenay_April_1_Snow_Course_SWE_Data.csv", water_year), row.names = FALSE)

##
K_manual_swe_normals <- manual_april_1_data %>%
  group_by(ID) %>%
  summarise(ID = ID[1], NAME = NAME[1], year.start = min(Year), year.end = max(Year), units = "mm", mean.SWE = mean(SWE),
            min.SWE = min(SWE), max.SWE = max(SWE), SWE.90th = quantile(SWE, 0.90), SWE.75th = quantile(SWE, 0.75),
            SWE.25th = quantile(SWE, 0.25), SWE.10th = quantile(SWE, 0.10)) %>%
  arrange(NAME)

# write.csv(K_manual_swe_normals, sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/Historical_Percentiles_BC_Kootenay_April_1_Snow_Course_SWE.csv", water_year, year(min(K_swe_hist$Date)), year(max(K_swe_hist$Date))), row.names = FALSE)


## Combining the BC Automated and Snow Course dataframes for easier reference
BC_Bind <- rbind(april_1_data, manual_april_1_data)
BC_Normals <- rbind(K_swe_normals, K_manual_swe_normals)



######## Snotel Sites ##############
### For April 1, may have to check link to see if it is up to date.
## Check if sites are loading, if not wait to run.
StahlPeakurl <-   "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customWaterYearGroupByMonthReport/daily/start_of_period/787:MT:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END:M%7C4,D%7C1/WTEQ::value"
HawkinsLakeurl <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customWaterYearGroupByMonthReport/daily/start_of_period/516:MT:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END:M%7C4,D%7C1/WTEQ::value"
GarverCreekurl <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customWaterYearGroupByMonthReport/daily/start_of_period/918:MT:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END:M%7C4,D%7C1/WTEQ::value"
BanfeildMountainurl <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customWaterYearGroupByMonthReport/daily/start_of_period/311:MT:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END:M%7C4,D%7C1/WTEQ::value"
MyrtleCreekurl <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customWaterYearGroupByMonthReport/daily/start_of_period/1053:ID:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END:M%7C4,D%7C1/WTEQ::value"
HiddenLakeurl <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customWaterYearGroupByMonthReport/daily/start_of_period/988:ID:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END:M%7C4,D%7C1/WTEQ::value"

## Read off the values from the csv files, change them to mm

#Stahl Peak MT, 6030ft
StahlPeak <- readLines(StahlPeakurl)
StahlPeak <- StahlPeak[-(1:60)]
StahlPeak <- as.data.frame(StahlPeak, stringsAsFactors = TRUE) %>%
  separate(col = StahlPeak, c("Year", "Day", "SWE_Inches"), sep = ",") %>%
  mutate(ID = "787", Name = "Stahl Peak",Year = year(as.Date(Year, "%Y")), Elevation.ft = 6030, Elevation.m = Elevation.ft*0.3048, SWE_mm = as.numeric(SWE_Inches)*25.4) %>%
  reframe(Name, ID, Elevation.m, Year, SWE_mm)

#Hawkins Lake MT, 6450ft
HawkinsLake <- readLines(HawkinsLakeurl)
HawkinsLake <- HawkinsLake[-(1:60)]
HawkinsLake <- as.data.frame(HawkinsLake, stringsAsFactors = TRUE) %>%
  separate(col = HawkinsLake, c("Year", "Day", "SWE_Inches"), sep = ",") %>%
  mutate(ID = "516", Name = "Hawkins Lake", Year = year(as.Date(Year, "%Y")), Elevation.ft = 6450, Elevation.m = Elevation.ft*0.3048, SWE_mm = as.numeric(SWE_Inches)*25.4) %>%
  reframe(Name, ID,  Elevation.m, Year, SWE_mm)

#Banfeild Mountain MT, 5600ft
BanfeildMountain <- readLines(BanfeildMountainurl)
BanfeildMountain <- BanfeildMountain[-(1:60)]
BanfeildMountain <- as.data.frame(BanfeildMountain, stringsAsFactors = TRUE) %>%
  separate(col = BanfeildMountain, c("Year", "Day", "SWE_Inches"), sep = ",") %>%
  mutate(ID = "311",Name = "Banfeild Mountain", Year = year(as.Date(Year, "%Y")), Elevation.ft = 5600, Elevation.m = Elevation.ft*0.3048, SWE_mm = as.numeric(SWE_Inches)*25.4) %>%
  reframe(Name, ID, Elevation.m, Year, SWE_mm)

#Garver Creek MT, 4250ft
GarverCreek <- readLines(GarverCreekurl)
GarverCreek <- GarverCreek[-(1:60)]
GarverCreek <- as.data.frame(GarverCreek, stringsAsFactors = TRUE) %>%
  separate(col = GarverCreek, c("Year", "Day", "SWE_Inches"), sep = ",") %>%
  mutate(ID = "918", Name = "Garver Creek", Year = year(as.Date(Year, "%Y")), Elevation.ft = 4250, Elevation.m = Elevation.ft*0.3048, SWE_mm = as.numeric(SWE_Inches)*25.4) %>%
  reframe(Name, ID, Elevation.m, Year, SWE_mm)

# Myrtle Creek ID, 3250ft
MyrtleCreek <- readLines(MyrtleCreekurl)
MyrtleCreek <- MyrtleCreek[-(1:60)]
MyrtleCreek <-  as.data.frame(MyrtleCreek, stringsAsFactors = TRUE) %>%
  separate(col = MyrtleCreek, c("Year", "Day", "SWE_Inches"), sep = ",")%>%
  mutate(ID = "1053", Name = "Myrtle Creek", Year = year(as.Date(Year, "%Y")), Elevation.ft = 3250, Elevation.m = Elevation.ft*0.3048, SWE_mm = as.numeric(SWE_Inches)*25.4) %>%
  reframe(Name, ID, Elevation.m, Year, SWE_mm)

# Hidden Lake ID, 5040ft
HiddenLake <- readLines(HiddenLakeurl)
HiddenLake <- HiddenLake[-(1:60)]
HiddenLake <-  as.data.frame(HiddenLake, stringsAsFactors = TRUE) %>%
  separate(col = HiddenLake, c("Year", "Day", "SWE_Inches"), sep = ",")%>%
  mutate(ID = "988", Name = "Hidden Lake", Year = year(as.Date(Year, "%Y")), Elevation.ft = 5040, Elevation.m = Elevation.ft*0.3048, SWE_mm = as.numeric(SWE_Inches)*25.4) %>%
  reframe(Name, ID, Elevation.m, Year, SWE_mm)

#Puts all of the snotel dataframes together, add/delete the names as required.
Snotel_Apr1_hist <- rbind(StahlPeak, HawkinsLake, BanfeildMountain, GarverCreek, HiddenLake, MyrtleCreek)%>%
  reframe( Year , ID ,NAME = Name, SWE = SWE_mm, Type = "Snotel")


#write.csv(Snotel_Apr1_hist, sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/Historical_Kootenay_April_1_Snotel_SWE_Data.csv", water_year), row.names = FALSE)

#Compute the historical statistics
Snotel_Apr1_normals <- Snotel_Apr1_hist %>%
  group_by(ID) %>%
  summarise(ID = ID[1], NAME = NAME[1], year.start = min(Year), year.end = max(Year), units = "mm", mean.SWE = mean(SWE),
            min.SWE = min(SWE), max.SWE = max(SWE), SWE.90th = quantile(SWE, 0.90), SWE.75th = quantile(SWE, 0.75),
            SWE.25th = quantile(SWE, 0.25), SWE.10th = quantile(SWE, 0.10)) %>%
  arrange(NAME)

# write.csv(Snotel_Apr1_normals, sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/Historical_Percentiles_Kootenay_April_1_Snotel_SWE.csv", water_year), row.names = FALSE)



#Binding the three seperate dataframes
Full_April_1 <- rbind(april_1_data, manual_april_1_data, Snotel_Apr1_hist)%>% arrange(Year)
Full_April_1_normals <- rbind(K_swe_normals, K_manual_swe_normals, Snotel_Apr1_normals)%>% arrange(NAME)

write.csv(Full_April_1, sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/Data_Computations/Full_Kootenay_April_1_SWE.csv", year), row.names = FALSE)
write.csv(Full_April_1_normals, sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/Data_Computations/Full_Kootenay_April_1_SWE_stats.csv", year), row.names = FALSE)

