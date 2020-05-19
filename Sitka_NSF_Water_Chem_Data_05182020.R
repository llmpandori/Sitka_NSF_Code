################################################################
# Title: Sitka NSF Water Chemistry Data Assembly
# Purpose: Assemble TA, nutrient, and pH data for Sitka NSF seasonal & experimental data
# Created by: L Pandori
# Created: 05/18/2020
# Last edited: 05/18/2020
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
  
##### Merge TA & Master Data #####
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

##### Merge Nutrient & Master Data #####
  
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

##### 
  