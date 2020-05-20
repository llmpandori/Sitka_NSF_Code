################################################################
# Title: Sitka NSF Water Chemistry Data Assembly
# Purpose: Assemble TA, nutrient, and pH data for Sitka NSF seasonal & experimental data
# Created by: L Pandori
# Created: 05/18/2020
# Last edited: 05/20/2020
################################################################
##### Package upload #####

# Load libraries
library(tidyverse)
library(viridis)
library(lubridate)
##### Master datasheet upload & tidy #####
# added day/night column before upload
# data upload from csv
master <- read_csv('Sitka_NSF_WaterChem_Data_R_Friendly_Columns_05182020.csv', 
        # format date and time columns
        col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
        Time_water_collected = col_time(format = "%H:%M"),
        TA_Notes = col_character()))

# tidy
master <- master %>%
  # add on/off column (clarify sept on and off)
  mutate(On_Off = ifelse(Date == '2019-09-21',
                  'ON', 
                 ifelse(Date == '2019-09-19',
                  'ON',
                 ifelse(Date == '2019-09-22',
                  'OFF',
                 ifelse(Date == '2019-09-25',
                  'OFF', 
                  NA))))) %>%
  # add month and year columns
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>%
  mutate(Year = year(Date))
         
##### TA data upload & tidy ######
ta <- read_csv('SitkaTidepoolTA_EOB.csv')
  # Important note: there aren't sept on/off TA data yet. When available, change code to add sept on/off labels like master!!!! 

  ta <- ta %>%
    # select relevant columns to match to master
    select(Month, Year, Day_Night, TimeStep, Pool, TA, CorrectedTA, `Sample Notes`, `Series Notes`) %>%
    # rename day/night and timept columns to match master
    rename(Day_night = Day_Night, 
           Time_point = TimeStep, 
           Sample_notes = `Sample Notes`, 
           Series_notes = `Series Notes`,
           TA_Corrected = CorrectedTA) %>%
    # make single column for notes (fix NAs later if needed)
    mutate(TA_Notes = paste(Sample_notes, Series_notes, sep = ','))
  
##### Merge TA & master data #####
  # left join to keep all master columns & add matches from ta
  master <- left_join(master, 
                      # keep all columns from master, add ta
                      select(ta, Month, Year, Day_night, 
                      Time_point, Pool, TA, 
                      TA_Corrected, TA_Notes),
                      by = c('Month', 'Year', 'Day_night', 'Time_point', 'Pool'))
  
  # get rid of extra columns from merge
  master <- master %>%
    mutate(TA_Corrected = TA_Corrected.y) %>%
    mutate(TA = TA.y) %>%
    mutate(TA_Notes = TA_Notes.y) %>%
    select(-c(TA_Notes.x, TA_Notes.y, TA_Corrected.x, TA_Corrected.y, TA.x, TA.y))

  # remove dataset from environment
  remove(ta)
  
##### Nutrient data upload & tidy #####
# upload data 
nutrients<-read_csv("Data_WaterChemistry_NSF_Sitka_05182020.csv", # set column types for dates
               col_types = cols(
                 Season = col_date(format = "%m/%d/%Y"), 
                 Season2 = col_date(format = "%m/%d/%Y"), 
                 Season3 = col_date(format = "%m/%d/%Y")))
  
# tidy data 
  nutrients <- nutrients %>% 
    # remove duplicate ID columns
    select(-c(Season2:Sample_ID2, Season3:Sample_ID3)) %>%
    # make day/night names consistent
    mutate(Day_night = ifelse(Day_night == 'D', 'Day', 
                       ifelse(Day_night == 'N', 'Night',
                       Day_night))) %>%
    # take away 'T' from time point column (to // master)
    mutate(Time_point = parse_number(substr(Time_point, 2,2))) %>%
    # make Month and Year columns to // master
    mutate(Year = year(Season)) %>%
    # add column to differentiate Sept ON/OFF
    mutate(On_Off = ifelse(month(Season) == 9 
                          & year(Season) == 2019, 
        Sample_ID, NA)) %>%
    mutate(Month = month(Season, label = TRUE, abbr = FALSE)) %>%
    mutate(On_Off = ifelse(On_Off == 'SeptemberOFF', 'OFF',
                    ifelse(On_Off == 'SeptemberON', 'ON', NA)))%>%
    # calculate Nitrate from N + N
    mutate(Nitrate_LaChat = Nitrate_and_Nitrite_LaChat - 
             # if Nitrite is -, use 0, if not, use listed value
             ifelse(Nitrite_LaChat >= 0, Nitrite_LaChat, 0)) %>%
    # put all notes in one column (comma separated)
    mutate(Nutrients_Notes = paste(NN_Notes, Phosphate_Notes,
                            Ammonium_Notes, sep = ', ' )) %>%
    select(-c(Sample_ID, NN_Notes, 
              Phosphate_Notes, Ammonium_Notes, Season))

##### Merge nutrient & master data #####
  
  # merge datasets by key columns
  master <- left_join(master, 
                      # keep all columns from master, add ta
                      nutrients,
                      by = c('Month', 'Year', 
                             'Day_night', 'Time_point', 
                             'Pool', 'On_Off'))
  
  # tidy merged datasets
  master <- master %>%
    # match column names to master
    mutate(Corrected_NN = Nitrate_and_Nitrite_LaChat) %>%
    mutate(Corrected_Nitrate = Nitrate_LaChat) %>%
    mutate(Corrected_Nitrite = Nitrite_LaChat) %>%
    mutate(Ammonium = Ammonium.y) %>%
    mutate(Phosphate = Phosphate_LaChat) %>%
    mutate(Nutrients_Notes = Nutrients_Notes.y) %>%
    # remove excess columns
    select(-c(Nitrate_and_Nitrite_LaChat, Nitrate_LaChat, Nitrite_LaChat, Ammonium.x, Ammonium.y, Phosphate_LaChat, Nutrients_Notes.x, Nutrients_Notes.y))

# detach nutrient data
  remove(nutrients)

##### Hanna pH calculation #####
# Source: Rph calculations.R created by N. Silbiger - in NSF Sitka Google Drive Folder

  # Load calibration data
  calib <- read_csv("Hannah_TRIS_Calibration_05202020.csv", 
           col_types = cols(
             Date = col_date(format = "%m/%d/%Y")))
  
 
  calib <- calib %>% 
            # order by date
            arrange(Date) %>%
            # change column names to match N. Silbiger code
            mutate(TTris = Tris_Temp, 
                   mVTris = Tris_mV)
  
  # get list of calibration dates
  dates <- unique(calib$Date)
  
# Make output tibble 
  Hanna_pH_df <- tibble()
  
  # loop calibration code
for (i in 1:length(dates)){
# subset relevant data
  # calibration date
  calib_date <- filter(calib, Date == dates[i])
  
  # field data 
  
  # if not final date
  if(dates[i+1] <= length(dates)) {
    # get field dates b/w calibration [i] and next [i+1]
    field_date <- filter(master, 
                  Date >= dates[i] & Date < dates[i+1])
  } else {
    # otherwise, get field dates + 5 from calibration
    field_date <- filter(master, 
                  Date >= dates[i] & Date < (dates[i] + 5))
  }
  
  # create linear regression + store p-value and r2 value
  mVTris_t<-lm(mVTris~TTris, data = calib_date)
  adjR2 = round(summary(mVTris_t)$adj.r.squared,digits = 2)
  pvalue = round(summary(mVTris_t)$coefficients[2,4], digits = 5)
  
  # plot regression w/ p-value and r2 and save
  ggplot(data = calib_date,
         mapping = aes(x = TTris, y = mVTris)) + 
    geom_point() + 
    geom_smooth(method = 'lm') +
    xlab('Tris Temperature (°C)') + 
    ylab('Hanna Reading (mV)') +
    ggtitle(paste('Calibration', dates[i])) + 
    geom_text(mapping = aes(x = min(TTris), y = max(mVTris), label = paste("R2 =",adjR2, ' P =', pvalue)), hjust = 0) + 
    theme_bw() 
    
  # save plot for later inspection
  filename = toString(paste('Calibration_Plot_',dates[i],'.png',sep = ''))
  ggsave(filename, width = 4, height = 4)
  
  # Make input file names match
  field_date <- field_date %>%
    mutate(Tin = Temp, 
           mV = Hanna_pH)

# Field Hanna pH calculation from mV (from N. Silbiger)
  
  # constants
  R <- 8.31451 # gas constant
  Far <- 96485.309 # Faraday constant
  
  # calculate the pH of the tris (Dickson A. G., Sabine C. L. and Christian J. R., SOP 6a)
  mvTris <- field_date$Tin * mVTris_t$coefficients[2] + 
            mVTris_t$coefficients[1]
  
  # Tris salinity (add column in calib datasheet)
  STris<-34.5
  
  # Calculate pH of Tris acros Temps
  phTris<- (11911.08-18.2499 * STris - 0.039336 * STris^2) * 
    (1/(field_date$Tin+273.15)) - 366.27059 + 0.53993607*
     STris + 0.00016329 * STris^2 +
    (64.52243-0.084041 * STris) * 
    log(field_date$Tin + 273.15) - 0.11149858 * 
    (field_date$Tin + 273.15)
  
  # calculate pH error
  # linear model of temp x Tris pH
  Tris<-lm(phTris~field_date$Tin) 
  # calculate pH @ 25*C
  TrisCalc<-25 * Tris$coefficients[2] + Tris$coefficients[1] 
  # % error of probe measurement
  field_date$pH_Error<-((TrisCalc - 8.0835)/8.0835) * 100 
  
  
  # Calculate the pH of your samples
  field_date$Hanna_pH_calculated <- phTris + 
    (mvTris/1000 - field_date$mV/1000) / 
    (R * (field_date$Tin + 273.15) * log(10)/Far)

# Select columns of interest and add to intermediate tibble
  # (to be merged with master)
  field_date <- select(field_date, SampleID,pH_Error, Hanna_pH_calculated)

  # merge with intermediate tibble
  Hanna_pH_df <- rbind(Hanna_pH_df, field_date)
}
  
##### Merge hanna pH & master data #####
  
  # join by sample ID, keeping all columns in master
  master <- left_join(master, Hanna_pH_df, by = 'SampleID')
  
##### Clean up environment #####
remove(calib, calib_date, field_date, Hanna_pH_df, mVTris_t, Tris, adjR2, dates, Far, filename, i, mvTris, phTris, pvalue, R, STris, TrisCalc)

##### Spec pH data upload & tidy #####
  
  