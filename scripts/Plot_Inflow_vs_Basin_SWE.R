# -------------------------------------------------------------------------
# Date created:      2024-04-22
# Last modified:     2024-06-06
# Authors:           K. Alexander
# Description:       This script estimates the total SWE on April 1st for the Kootenay Basin.
#
# Scripts Utilized:  None
#
# Required input(s):  Full_Kootenay_April_1_SWE.csv
#                     Historical_Apr1toJul31_total_KL_Inflow_volume.csv
#                     %s_FortisBC_Kootenay_Lake_Data.csv
#
#
# Outputs:
#
#
#
# Note(s): Run this script if you have already run the compile data scripts, make sure the years are up to date and the stations are the ones you want to select.
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

Script_Location <- sprintf("C:/Users/%s/R Projects/BC_Hydrometric_Conditions", user) # Location of BC_Hydrometric_Conditions GitHub repository
Basin_Location <- "C:/Basins" # path to Boundary Waters / BC Boards plots folder

setwd(Basin_Location)

year <- year(Sys.Date()) # Sets year to current year
water_year <- ifelse(month(Sys.Date()) >= 10,year+1,year)


## Read in full snow pillow data
Full_April_1 <- read.csv(file = sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/Full_Kootenay_April_1_SWE.csv", year))
Year_Summary <- read.csv(file = sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/Data_Computations/Historical_Apr1toJul31_total_KL_Inflow_volume.csv", year))
Curr_KL_data <- read.csv(file = sprintf("IJC Kootenay Board/%s/Data Computations/R_Data_CY_DM/%s_FortisBC_Kootenay_Lake_Data.csv", year, year))
KL_data <- read.csv(file = sprintf("IJC Kootenay Board/%s/Data Computations/R_Data_Historical/Historical_FortisBC_Kootenay_Lake_Data_1928-%s.csv", year, year-1))


####### Estimating Basin Snowpack #############

## Selected stations #######
##Zone 1 (500-999)
Stn1 <- "NELSON"
Stn2 <- "DUNCAN LAKE NO. 2"
Stn3 <- "Myrtle Creek"

##Zone 2 (1000-1499)
Stn4 <- "CHAR CREEK"
Stn5 <- "Garver Creek"
#Stn6 <-

##Zone 3 (1500-1999)
Stn7 <- "Hawkins Lake"
Stn8 <- "Hidden Lake"
Stn9 <- "Moyie Mountain"

##Zone 4 (2000-2499)
Stn10 <- "Redfish Creek"
Stn11 <- "Floe Lake"


#Zone 5 (2500+) - None


##Zone 1 (500-999)
stn1_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn1), Year >= "1999", Year <= "2023")%>%
  rename(year = Year)
stn2_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn2), Year >= "1999", Year <= "2023")%>%
  rename(year = Year)
stn3_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn3), Year >= "1999", Year <= "2023")%>%
  rename(year = Year)

zone_1 <- rbind(stn1_SWE, stn2_SWE, stn3_SWE)

##Zone 2 (1000-1499)
stn4_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn4), Year >= "1999", Year <= "2023")%>%
  rename(year = Year)
stn5_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn5), Year >= "1999", Year <= "2023")%>%
  rename(year = Year)

zone_2 <- rbind(stn4_SWE, stn5_SWE)

##Zone 3 (1500-1999)
stn7_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn7), Year >= "1999", Year <= "2023")%>%
  rename(year = Year)
stn8_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn8), Year >= "1999", Year <= "2023")%>%
  rename(year = Year)
stn9_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn9), Year >= "1999", Year <= "2023")%>%
  rename(year = Year)

zone_3 <- rbind(stn7_SWE, stn8_SWE, stn9_SWE)


##Zone 4 (2000-2499)
stn10_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn10), Year >= "1999", Year <= "2023")%>%
  rename(year = Year)
stn11_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn11), Year >= "1999", Year <= "2023")%>%
  rename(year = Year)


zone_4 <- rbind(stn10_SWE, stn11_SWE)

#Zone 5 (2500+) - None


## Area from GIS
Z1_area_km2 = 7359.70  # 500-999
Z2_area_km2 = 14527.06  # 1000-1499
Z3_area_km2 = 15394.50  # 1500-1999
Z4_area_km2 = 8028.75 + 1050.25 # 2000+
Z5_area_km2 = 1050.25

#Just Corra Linn Basin
# Z1_area_km2 = 4162.53  # 500-999
# Z2_area_km2 = 7769.26 # 1000-1499
# Z3_area_km2 = 6684.30 # 1500-1999
# Z4_area_km2 = 1898.51 + 153.75 # 2000-2500
# Z5_area_km2 = 153.75 #left out of future calculations



# ##Pie chart to show the percentages of the basin (THIS IS FOR OSOYOOS, edit colours and percentages)
# data <- data.frame(
#   Elevation_meters=c("0-499", "500-999", "1000-1499", "1500-1999", "2000-2499"),
#   percent=c(9, 18, 39, 30, 4)
# )
#
# # Basic piechart
# pg<-ggplot(data, aes(x="", y=percent, fill=Elevation_meters)) +
#   geom_bar(stat="identity", width=1, color="white") +
#   coord_polar("y", start=0) +
#   geom_text(aes(label = paste0(percent, "%")),
#             position = position_stack(vjust = 0.5)) +
#   labs(fill = "Elevation (m)", title = "Percentage of Basin Area in 500m Elevation Bands")+
#   scale_fill_manual(values = c("0-499"='#045a8d',"500-999"='#2b8cbe',"1000-1499"='#74a9cf',"1500-1999"='#a6bddb',"2000-2499"='#d0d1e6'),
#                     breaks = c("0-499", "500-999", "1000-1499", "1500-1999", "2000-2499"))+
#   theme_void() # remove background, grid, numeric labels
#
# pg


Z1 <- zone_1 %>%
  group_by(year) %>%
  summarise(Elevation = "500m-999m" , mean.SWE.mm = mean(SWE))%>%
  mutate(Total.VOL.km3 = Z1_area_km2*(mean.SWE.mm/10^6))%>% #add in the total volume
  complete(year = 1999:2023)
Z2 <- zone_2 %>%
  group_by(year) %>%
  summarise(Elevation = "1000m-1499m" , mean.SWE.mm = mean(SWE))%>%
  mutate(Total.VOL.km3 = Z2_area_km2*(mean.SWE.mm/10^6))%>%
  complete(year = 1999:2023)
Z3 <- zone_3 %>%
  group_by(year) %>%
  summarise(Elevation = "1500m-1999m" , mean.SWE.mm = mean(SWE))%>%
  mutate(Total.VOL.km3 = Z3_area_km2*(mean.SWE.mm/10^6))%>%
  complete(year = 1999:2023)
Z4 <- zone_4 %>%
  group_by(year) %>%
  summarise(Elevation = "2000m-2499m" , mean.SWE.mm = mean(SWE))%>%
  mutate(Total.VOL.km3 = Z4_area_km2*(mean.SWE.mm/10^6))%>%
  complete(year = 1999:2023)

Total_Basin_SWE <- rbind(Z1, Z2, Z3, Z4)
Total_Basin_SWE<- Total_Basin_SWE %>%
  group_by(year) %>%
  summarise(Total.SWE.km3 = sum(Total.VOL.km3))


plot_data <- Year_Summary %>% filter(Year >= min(Total_Basin_SWE$year)) %>%
  cbind(Total_Basin_SWE)%>%
  select(-year)




##Current Year Data ######

##Zone 1 (500-999)

stn1_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn1), Year =="2024")%>%
  rename(year = Year)
stn2_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn2), Year =="2024")%>%
  rename(year = Year)
stn3_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn3), Year =="2024")%>%
  rename(year = Year)

curr_zone_1 <- rbind(stn1_SWE, stn2_SWE, stn3_SWE)


##Zone 2 (1000-1499)

stn4_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn4), Year =="2024")%>%
  rename(year = Year)
stn5_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn5), Year =="2024")%>%
  rename(year = Year)

curr_zone_2 <- rbind(stn4_SWE, stn5_SWE)

##Zone 3 (1500-1999)
stn7_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn7), Year =="2024")%>%
  rename(year = Year)
stn8_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn8), Year =="2024")%>%
  rename(year = Year)
stn9_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn9), Year =="2024")%>%
  rename(year = Year)

curr_zone_3 <- rbind(stn7_SWE, stn8_SWE, stn9_SWE)

##Zone 4 (2000-2499)"

stn10_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn10), Year =="2024")%>%
  rename(year = Year)
stn11_SWE <- Full_April_1 %>%
  filter(NAME == sprintf( "%s", Stn11), Year =="2024")%>%
  rename(year = Year)

curr_zone_4 <- rbind(stn10_SWE, stn11_SWE)


Curr_Z1 <- curr_zone_1 %>%
  group_by(year) %>%
  summarise(Elevation = "500m-999m" , mean.SWE.mm = mean(SWE))%>%
  mutate(Total.VOL.km3 = Z1_area_km2*(mean.SWE.mm/10^6))

Curr_Z2 <- curr_zone_2 %>%
  group_by(year) %>%
  summarise(Elevation = "1000m-1499m" , mean.SWE.mm = mean(SWE))%>%
  mutate(Total.VOL.km3 = Z2_area_km2*(mean.SWE.mm/10^6))

Curr_Z3 <- curr_zone_3 %>%
  group_by(year) %>%
  summarise(Elevation = "1500m-1999m" , mean.SWE.mm = mean(SWE))%>%
  mutate(Total.VOL.km3 = Z3_area_km2*(mean.SWE.mm/10^6))

Curr_Z4 <- curr_zone_4 %>%
  group_by(year) %>%
  summarise(Elevation = "2000m-2499m" , mean.SWE.mm = mean(SWE))%>%
  mutate(Total.VOL.km3 = Z4_area_km2*(mean.SWE.mm/10^6))

Curr_Basin_SWE <- rbind(Curr_Z1, Curr_Z2, Curr_Z3, Curr_Z4)
Curr_Total_Basin_SWE<- Curr_Basin_SWE %>%
  summarise(year = "2024", Total.SWE.km3 = sum(Total.VOL.km3))


#### Current Lake Inflow ######
Curr_lake_inflow <- Curr_KL_data %>% mutate(KL.Inflow.cms = KL.Inflow.kcfs*28.3168)

Curr_AprtoJul_flow <- Curr_lake_inflow %>% filter(month(Date)>=4 & month(Date)<=7)%>%
  mutate(Total_Vol.m3 = KL.Inflow.cms*86400, Year = year(Date)) #daily water

Curr_inflow_Mm3 <- sum(Curr_AprtoJul_flow$Total_Vol.m3)/(10^6)


curr_plot_data <- data.frame(Year = "2024", Total_Volume.km3 = Curr_inflow_Mm3/1000, Total.SWE.km3 = Curr_Total_Basin_SWE$Total.SWE.km3)

plot_data2 <- rbind(plot_data, curr_plot_data)%>%
  mutate(percent_runoff = (Total_Volume.km3/Total.SWE.km3)*100)


######## Plots ###################################################################

ggplot(Total_Basin_SWE, aes(year, Total.SWE.km3 )) +
  geom_bar(stat="identity")


#### Prediction

model <- lm(Total_Volume.km3 ~ Total.SWE.km3, data = plot_data)

prediction_intervals <- predict(
  model,
  newdata = plot_data,
  interval = "prediction",
  level = 0.95
)

# Print the prediction interval
head(prediction_intervals)


full_res <- cbind(plot_data, prediction_intervals)

head(full_res)

full_res |>
  ggplot(aes(x = Total.SWE.km3, y = Total_Volume.km3)) +
  geom_point() +
  geom_point(aes(y = fit), col = "steelblue", size = 2.5) +
  geom_line(aes(y = fit)) +
  geom_line(aes(y = lwr), linetype = "dashed", col = "red") +
  geom_line(aes(y = upr), linetype = "dashed", col = "red") +
  theme_minimal() +
  labs(
    title = "Total.SWE.km3 ~ Total_Volume.km3, data = plot_data",
    subtitle = "With Prediction Intervals"
  )


### Main plot with out the current year

Comp1 <- ggplot(NULL, aes(x = Total.SWE.km3, y = Total_Volume.km3))+
  geom_point(data = plot_data, size = 3, shape = 8, aes(colour = "Historical (1999-2023)")) +
  stat_cor(data = plot_data, aes(x = Total.SWE.km3, y = Total_Volume.km3), p.accuracy = 0.001, r.accuracy = 0.01, label.x=min(Total_Basin_SWE), label.y=14)+
  #stat_regline_equation(data = plot_data, aes(x = Total.SWE.km3, y = Total_Volume.km3), label.x=11.5, label.y=0.017) +
  geom_smooth(data = plot_data, method=lm , aes(colour= "Best Fit Line & Confidence Interval"), se=TRUE) +
  geom_segment(data = data.frame(y = -Inf, x = Curr_Total_Basin_SWE$Total.SWE.km3,
                                 xend = Curr_Total_Basin_SWE$Total.SWE.km3,  yend = 3),
               aes(x, y, xend = xend, yend = yend, colour = "2024 Estimated Total April 1 SWE"))+
  geom_line(data = plot_data, aes(y = full_res$lwr, colour = "Lower Prediction Interval"), linetype = "dashed") +
  geom_line(data = plot_data, aes(y = full_res$upr, colour = "Upper Prediction Interval"), linetype = "dashed") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, size = 18),
                          axis.title = element_text(size = 15),
                          axis.title.y = element_text(angle = 90, vjust = 0.5),
                          legend.title = element_text(size = 14), legend.text = element_text(size = 12),
                          legend.justification = c(0,1), legend.position = c(0,1), legend.margin = margin(5,0,-21,5),
                          legend.background = element_blank(), legend.key = element_blank(), legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.55, "cm"))+
  scale_x_continuous(name = expression(`Estimated total April 1 SWE `~(km^3)), breaks = seq(12,38, by=2)) +
  scale_y_continuous( name = expression(`April to July Inflow `~(km^3)), expand = expansion(c(0.01,0.05),c(1,1)), labels = scales::comma_format(accuracy = 1), breaks = seq(0,24, by=2)) +
  labs(y = "Total April - July Inflow (km3)", x = "Total Basin SWE on April 1 (km3)") +
  scale_colour_manual(
    "April-July Inflow vs April 1 SWE",
    breaks = c("Historical (1999-2023)","2024","2024 Estimated Total April 1 SWE", "Best Fit Line & Confidence Interval",  "Upper Prediction Interval", "Lower Prediction Interval"),
    values = c("Historical (1999-2023)" = "black", "Best Fit Line & Confidence Interval" = "turquoise", "2024 Estimated Total April 1 SWE" = "orangered2", "Upper Prediction Interval" = "springgreen3","Lower Prediction Interval" = "blue"))

Comp1
ggsave(sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/Snow_vs_Inflow_Plot_April1.png", year), Comp1, width = 9, height = 6)



## total SWE plot
data <- rbind(Total_Basin_SWE, Curr_Total_Basin_SWE)
ggplot(data, aes(year, Total.SWE.km3 )) +
  theme_bw()+
  geom_bar(stat="identity")+
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = expression(`Estimated total April 1 SWE `~(km^3)), expand = expansion(c(0.01,0.05),c(1,1)), labels = scales::comma_format(accuracy = 1), breaks = seq(0,40, by=2))





# Main plot with current year - run after August 1, will have to edit the years
if (month(Sys.Date()) >= 8) {

Comp2 <- ggplot(NULL, aes(x = Total.SWE.km3, y = Total_Volume.km3))+
  geom_point(data = plot_data, size = 3, shape = 8, aes(colour = "Historical (1999-2023)")) +
  geom_point(data = curr_plot_data, size = 3, shape = 8, aes(colour = "2024")) +
  stat_cor(data = plot_data, aes(x = Total.SWE.km3, y = Total_Volume.km3), p.accuracy = 0.001, r.accuracy = 0.01, label.x=min(Total_Basin_SWE), label.y=14)+
  #stat_regline_equation(data = plot_data, aes(x = Total.SWE.km3, y = Total_Volume.km3), label.x=11.5, label.y=0.017) +
  geom_smooth(data = plot_data, method=lm , aes(colour= "Best Fit Line & Confidence Interval"), se=TRUE) +
  geom_line(data = plot_data, aes(y = full_res$lwr, colour = "Lower Prediction Interval"), linetype = "dashed") +
  geom_line(data = plot_data, aes(y = full_res$upr, colour = "Upper Prediction Interval"), linetype = "dashed") +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, size = 18),
                          axis.title = element_text(size = 15),
                          axis.title.y = element_text(angle = 90, vjust = 0.5),
                          legend.title = element_text(size = 14), legend.text = element_text(size = 12),
                          legend.justification = c(0,1), legend.position = c(0,1), legend.margin = margin(5,0,-21,5),
                          legend.background = element_blank(), legend.key = element_blank(), legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.55, "cm"))+
  scale_x_continuous(name = expression(`Estimated total April 1 SWE `~(km^3)), breaks = seq(12,38, by=2)) +
  scale_y_continuous( name = expression(`April to July Inflow `~(km^3)), expand = expansion(c(0.01,0.05),c(1,1)), labels = scales::comma_format(accuracy = 1), breaks = seq(0,24, by=2)) +
  scale_colour_manual(
     "April-July Inflow vs April 1 SWE",
     breaks = c("Historical (1999-2023)","2024", "Best Fit Line & Confidence Interval",  "Upper Prediction Interval", "Lower Prediction Interval"),
     values = c("Historical (1999-2023)" = "black", "2024"= "orangered2", "Best Fit Line & Confidence Interval" = "turquoise", "Upper Prediction Interval" = "springgreen3","Lower Prediction Interval" = "blue"))

Comp2
ggsave(sprintf("IJC Kootenay Board/%s/Data Computations/SWE_Inflow/Snow_vs_Inflow_Plot_April1_withCurrent.png", year), Comp2, width = 9, height = 6)



Comp3 <- ggplot(NULL, aes(x = Total.SWE.km3, y = Total_Volume.km3))+
  geom_point(data = plot_data2, size = 3, shape = 8, aes(colour = "Historical (1999-2024)")) +
  stat_cor(data = plot_data2, aes(x = Total.SWE.km3, y = Total_Volume.km3), p.accuracy = 0.001, r.accuracy = 0.01, label.x= min(Total_Basin_SWE), label.y=0.5)+
  #stat_regline_equation(data = plot_data2, aes(x = Total.SWE.km3, y = Total_Volume.km3), label.x=11.5, label.y=0.017) +
  geom_smooth(data = plot_data2, method=lm , aes(colour= "Best Fit Line & Confidence Interval"), se=TRUE) +
  geom_abline(slope=1, intercept=0)+
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, size = 18),
                          axis.title = element_text(size = 15),
                          axis.title.y = element_text(angle = 90, vjust = 0.5),
                          legend.title = element_text(size = 14), legend.text = element_text(size = 12),
                          legend.justification = c(0,1), legend.position = c(0,1), legend.margin = margin(5,0,-21,5),
                          legend.background = element_blank(), legend.key = element_blank(), legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.55, "cm"))+
  scale_x_continuous(name = expression(`Estimated total April 1 SWE `~(km^3)), breaks = seq(12,38, by=2)) +
  scale_y_continuous( name = expression(`April to July Inflow `~(km^3)), expand = expansion(c(0.01,0.05),c(1,1)), labels = scales::comma_format(accuracy = 1), breaks = seq(0,24, by=2)) +
  scale_colour_manual(
    "April-July Inflow vs April 1 SWE",
    breaks = c("Historical (1999-2024)", "Best Fit Line & Confidence Interval"),
    values = c("Historical (1999-2024)" = "black", "Best Fit Line & Confidence Interval" = "turquoise"))

Comp3

}

Pt4 <- ggplot(NULL, aes(x = Total.SWE.km3, y = percent_runoff)) +
  geom_point(data = plot_data2, size = 3)+
  stat_cor(data = plot_data2, p.accuracy = 0.001, r.accuracy = 0.01, label.x= min(Total_Basin_SWE), label.y= 45)+
  theme_classic()+
  scale_x_continuous(name = expression(`Estimated total April 1 SWE `~(km^3)), n.breaks = 10)+
  scale_y_continuous(name = expression(`Percent Runoff (%)`, n.breaks = 10))

Pt4



## Kootenay Lake Inflow, Discharge and Level Comparison

yrs <- c("2001", "2012")

In_Plot_Data <- KL_data %>%
  select(Date, Year, KL.Inflow.kcfs)%>%
  filter(year(Date) %in% yrs)%>%
  mutate(month.day = paste(month(Date), day(Date), sep = "-"))

In_Plot_Data <- In_Plot_Data %>%
  mutate(Date = as.Date(paste(year, In_Plot_Data$month.day, sep = "-"))) %>%
  filter(month(Date)>=4 & month(Date)<=7)


KI_cp <- ggplot() +
  theme_classic() +
  geom_line(data = In_Plot_Data , aes(x = Date, y = KL.Inflow.kcfs, colour = paste(Year)), size = 1.0)+
  scale_x_date(name = NULL, date_breaks = "1 month",date_labels = "%b", expand = expansion(0,c(0,1))) +
  scale_y_continuous( name = expression(`Inflow in Thousands`~(ft^3/s)), expand = expansion(c(0.01,0.05),c(1,1)), labels = scales::comma_format(accuracy = 1), breaks = seq(0,200, by=10),
                      sec.axis = sec_axis(~./35.314666212661, labels = scales::comma_format(accuracy = 0.1), breaks = seq(0,4, by=0.2),name = expression(`Inflow in Thousands`~(m^3/s)))) +
  theme(axis.title = element_text(size = 13), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        legend.justification = c(0,1), legend.position = c(0,1), legend.margin = margin(5,5,-21,5),
        legend.background = element_blank(), legend.key = element_blank(), legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.55, "cm")) +
  guides(fill = guide_legend(order = 1), linetype = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  labs(fill = "KOOTENAY LAKE INFLOW (FortisBC)", linetype = NULL, colour = NULL)

KI_cp


Out_Plot_Data <- KL_data %>%
  select(Date, Year, KL.Discharge.kcfs)%>%
  filter(year(Date) %in% yrs)%>%
  mutate(month.day = paste(month(Date), day(Date), sep = "-"))

Out_Plot_Data <- Out_Plot_Data %>%
  mutate(Date = as.Date(paste(year, Out_Plot_Data$month.day, sep = "-"))) %>%
  filter(month(Date)>=4 & month(Date)<=7)


KO_p <- ggplot() +
  theme_classic() +
  geom_line(data = Out_Plot_Data , aes(x = Date, y = KL.Discharge.kcfs, colour = paste(Year)), size = 1.0)+
  scale_x_date(name = NULL, date_breaks = "1 month",date_labels = "%b", expand = expansion(0,c(0,1))) +
  scale_y_continuous( name = expression(`Discharge in Thousands`~(ft^3/s)), expand = expansion(c(0.01,0.05),c(1,1)), labels = scales::comma_format(accuracy = 1), breaks = seq(0,200, by=10),
                      sec.axis = sec_axis(~./35.314666212661, labels = scales::comma_format(accuracy = 0.1), breaks = seq(0,4, by=0.2),name = expression(`Discharge in Thousands`~(m^3/s)))) +
  theme(axis.title = element_text(size = 13), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        legend.justification = c(0,1), legend.position = c(0,1), legend.margin = margin(5,5,-21,5),
        legend.background = element_blank(), legend.key = element_blank(), legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.55, "cm")) +
  guides(fill = guide_legend(order = 1), linetype = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  labs(fill = "KOOTENAY LAKE Discharge (FortisBC)", linetype = NULL, colour = NULL)

KO_p


QB_Plot_Data <- KL_data %>%
  select(Date, Year, KL.at.Queens.Bay.ft)%>%
  filter(year(Date) %in% yrs)%>%
  mutate(month.day = paste(month(Date), day(Date), sep = "-"))

QB_Plot_Data <- QB_Plot_Data %>%
  mutate(Date = as.Date(paste(year, month.day, sep = "-"))) %>%
  filter(month(Date)>=4 & month(Date)<=7)


QB_cp <- ggplot() +
  theme_classic() +
  geom_line(data = QB_Plot_Data , aes(x = Date, y = KL.at.Queens.Bay.ft, colour = paste(Year)), size = 1.0)+
  scale_x_date(name = NULL, date_breaks = "1 month",date_labels = "%b", expand = expansion(0,c(0,1))) +
  scale_y_continuous( name = expression(`Elevation `~(ft)), expand = expansion(c(0.01,0.05),c(1,1)), labels = scales::comma_format(accuracy = 1), breaks = seq(0,1780, by=2),
                      sec.axis = sec_axis(~.*0.3048, labels = scales::comma_format(accuracy = 0.1), breaks = seq(500,600, by=1),name = expression(`Elevation`~(m)))) +
  theme(axis.title = element_text(size = 13), legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        legend.justification = c(0,1), legend.position = c(0,1), legend.margin = margin(5,5,-21,5),
        legend.background = element_blank(), legend.key = element_blank(), legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.55, "cm")) +
  guides(fill = guide_legend(order = 1), linetype = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  labs(fill = "KOOTENAY QUEENS BAY ELEVATION (FortisBC)", linetype = NULL, colour = NULL)

QB_cp

