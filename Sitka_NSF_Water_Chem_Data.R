################################################################
# Title: Sitka NSF Water Chemistry Data Assembly
# Purpose: Assemble TA, nutrient, and pH data for Sitka NSF seasonal & experimental data
# Created by: L Pandori
# Created: 05/18/2020
# Last edited: 6/4/2020
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
    mutate(TA_Notes = paste(
              ifelse(is.na(Sample_notes), '', Sample_notes),
              ifelse(is.na(Series_notes), '', Series_notes))) %>%
    # fix ocean a's and b's (make ocean1 and ocean2)
    mutate(Pool = ifelse(Pool == 'oceanA', 'ocean1',
                  ifelse(Pool == 'oceanB', 'ocean2',
                  ifelse(Pool == 'Ocean1', 'ocean1',
                  ifelse(Pool == 'Ocean2', 'ocean2', 
                  ifelse(Pool == 'Ocean3', 'ocean3', Pool))))))
    
##### Merge TA & master data #####
  # make master ocean column match (no caps)
  master <- mutate(master, Pool = ifelse(Pool == 'Ocean', 'ocean',Pool))
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
    mutate(Nutrients_Notes = paste(
      ifelse(is.na(NN_Notes),'', NN_Notes),
      ifelse(is.na(Phosphate_Notes), '', Phosphate_Notes),
      ifelse(is.na(Ammonium_Notes), '', Ammonium_Notes), 
      sep = "")) %>%
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
  if((i+1) <= length(dates)) {
    # get field dates b/w calibration [i] and next [i+1]
    field_date <- filter(master, 
                  Date >= dates[i] & Date < dates[i+1])
  } else {
    # if last date, run all following dates
    field_date <- filter(master, 
                  Date >= dates[i])
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
  
  # save CSV for CS (5/21/2020)
  write_csv(master, 'master_asof_05212020.csv')
  
##### Clean up environment #####
remove(calib, calib_date, field_date, Hanna_pH_df, mVTris_t, Tris, adjR2, dates, Far, filename, i, mvTris, phTris, pvalue, R, STris, TrisCalc)

##### Spec pH data upload & tidy #####
# load data (files correspond to batches of cresol)
  batch2 <- read_csv("Spec_pH_2020_batch2.csv", 
              col_types = cols(Sample_run_date = 
                                 col_date(format = "%m/%d/%Y"), 
                               Sample_run_time = 
                                 col_time(format = "%H:%M")))
  
  batch1 <- read_csv("Spec_pH_Pre2020_LP.csv", 
                col_types = cols(Sample_run_date =
                                  col_date(format = "%m/%d/%Y"), 
                                 Sample_run_time = 
                                   col_time(format = "%H:%M")))
  
# tidy
  # add batch ID column
  batch1$batch_ID <- 1
  batch2$batch_ID <- 2
  
  # test if column names are the same 
    # If TRUE, proceed
    # if TRUE & FALSE, or FALSE, check datasets before proceeding
  unique(colnames(batch1) == colnames(batch2))
  
# join datsets with rbind, remove individual sets
  specph <- rbind(batch1, batch2)
  remove(batch1, batch2)
  
# create key columns in spec data for joining to master
  specph <- specph %>%
    # get time point as number without "T"
    mutate(Time_point = substr(Time_point,2,2)) %>%
    # spell out day and night
    mutate(Day_night = ifelse(Day_night %in% c('D', 'D(OFF)',
                            'D(ON)'), 'Day', 'Night')) %>%
    # make month, year, and ON/OFF (for sept 2019) columns
    mutate(Month = ifelse(Sampling == '18-Jun', 'June',
                   ifelse(Sampling == '18-Sep', 'September',
                   ifelse(Sampling == '19-Jan', 'January',
                   ifelse(Sampling == 'September 2019 OFF',
                          'September',
                   ifelse(Sampling == 'Spring (May) 2019', 
                          'March',
                   ifelse(Sampling == 'Sept OFF', 'September',
                   ifelse(Sampling == 'Sept ON', 'September', 
                          NA)))))))) %>%
    # year
    mutate(Year = ifelse(Sampling == '18-Jun', 2018,
                   ifelse(Sampling == '18-Sep', 2018,
                    ifelse(Sampling == '19-Jan', 2019,
                    ifelse(Sampling == 'September 2019 OFF', 
                           2019,
                    ifelse(Sampling == 'Spring (May) 2019', 2019,
                    ifelse(Sampling == 'Sept OFF', 2019,
                    ifelse(Sampling == 'Sept ON', 2019, 
                           NA)))))))) %>%
    # of/on (only for Sept 2019)
    mutate(On_Off = ifelse(Sampling == 'September 2019 OFF', 
                           'OFF',
                    ifelse(Sampling == 'Sept OFF', 'OFF',
                    ifelse(Sampling == 'Sept ON', 'ON', NA))))%>%
    # make sure time point is numeric, and I's are 1's
    mutate(Time_point = 
          parse_number(ifelse(Time_point == 'I',
                              1, Time_point))) %>%
    # make sure ocean naming is consistent among datasets
    mutate(Pool = 
          ifelse(Pool == 'Ocean', 'ocean',
          ifelse(Pool == 'Ocean1', 'ocean1',
          ifelse(Pool == 'Ocean2', 'ocean2', Pool)))) %>%
    # name sample temp column (not confused w. ocean temp)
    mutate(Spec_temp = Temp)
    
    specph$Temp <- NULL
  
##### Calculate spec pH #####
# all equations come from batch correction excel files
    
  # isolate relevant water data from master
  watermaster <- select(master, 
                        Time_point, Pool, Day_night, On_Off, 
                        Month, Year, Temp, Salinity, DO)
    
  # merge relevant parts of master w/ spec ph dataset (not all b/c spec ph data are replicates (A,B,C), master is not)
  specph <- left_join(specph, watermaster, by = c("Time_point", "Day_night", "Pool", "Month", "Year", "On_Off"))
  remove(watermaster)
  
# upload batch correction info
  batch <- read_csv("Dye_Batch_Corrections_06012020.csv")
  
# name extinction coefficient ratios (copied from Kroeker lab excel calculation file)
  r1 <- 0.0069
  r2 <- 2.222
  r3 <- 0.133
  
# create output file
  calcph <- specph[0,]
  
# loop across batches
  for (i in 1:length(batch)){
    
    onebatch <- filter(batch, Dye_Batch == i)
  
# calculations
  specph2 <- specph %>%
    # filter data from dye batch
    filter(batch_ID == i) %>%
    # get dye - no dye difference for each wavelength
    mutate(dif_WL434 = D_WL434 - ND_WL434,
           dif_WL578 = D_WL578 - ND_WL578,
           dif_WL730 = D_WL730 - ND_WL730) %>%
    # get baseline shift for 434 and 578
    mutate(A2 = dif_WL434 - dif_WL730,
           A1 = dif_WL578 - dif_WL730) %>%
    # get A1/A2 and corrected A1/A2 
    mutate(A1_div_A2 = A1/A2) %>%
    mutate(A1_div_A2_cor = A1_div_A2 + onebatch$Dye_Volume_cm3 *
             (onebatch$`Y-Int` + onebatch$Slope * (A1_div_A2))) %>%
    # calculate pK2
    mutate(pK2 = ((1245.69)/(273.15+Spec_temp)+3.8275+0.00211*(35-Salinity))) %>%
    # calculate corrected pH @ 25*C
    mutate(pH_corr = pK2 + log10((A1_div_A2_cor - r1)/(r2 - (A1_div_A2_cor*r3))))
  
  calcph <- rbind(calcph, specph2)
  }
  
  # save file to check against excel spreadsheet
  write_csv(calcph, 'Calculated_pH_06012020.csv')
  
# Clean up environment
  remove(batch, onebatch, specph2, i, r1, r2, r3, specph)

##### QC Spec pH data #####
  # non-dye samples must be within 0.001 for 3 wavelengths
  # dye samples must be within 0.005 for 3 wavelenghts
  
  # major question - what if 3 samples (a,b,c), and a+b match, b+c match, but not a+C? 
  
  # make key column for sample names
  calcph <- mutate(calcph,
                   key = paste(Month, Year, '-', On_Off, 
                               '-T', Time_point, '-', Day_night,
                               '-P', Pool, sep = ''),
                   spec_ph_qc_note = NA,
                   replicate = Replicate)
  
  # make output data frame (key, replicate)
  qc_list <- calcph[0,]
  qc_list <- qc_list %>%
    select(key, replicate = Replicate)
  
# the following code is used to test the loop
  # comment out when not developing
  # qc <- read_csv('wl_qc_testing_06022020.csv')
  # samplelist <- unique(qc$key) 
  # qc_list <- select(qc[0,], key, replicate)

# make a list of samples with NAs, 0's and Nans exclude from list
  excludelist1 <- calcph %>%
    # filter out samples with pH < 5.4
    filter(pH_corr < 5.4 | 
             # or NAs or NaNs
             is.na(pH_corr) | is.nan(pH_corr)) %>%
    # get keys that meet criteria
    group_by(key) %>% 
    select(key) %>%
    t %>% c %>% unique
  
  # exclude samples with length < 3
  excludelist2 <- calcph %>%
    group_by(key) %>% 
    filter(length(pH_corr) < 3) %>%
    select(key) %>%
    t %>% c %>% unique
  
  # put together exclude lists, and get unique values
  excludelist <- unique(c(excludelist1, excludelist2))
  
# get list of sample names to loop calculations
  samplelist <- calcph %>% 
    filter(!key %in% excludelist) %>%
    select(key) %>%
    t %>% c %>% unique
  
# trust the wild for loop
for(i in 1:length(samplelist)){
  
  qcdata <- calcph %>%
    filter(key == samplelist[i]) %>%
    filter(!is.na(pH_corr)) %>%
    mutate(replicate = Replicate)
  
# Dye - 434 
  # find abs values of differences b/w replicates
  combos <- as_tibble(t(combn(qcdata$D_WL434, 2)))
  rows <- as_tibble(t(combn(qcdata$replicate, 2)))
  colnames(combos) <- c('r1', 'r2')
  
  d434combos <- combos %>%
    mutate(name = paste(rows$V1, rows$V2),
           dif = abs(combos$r1 - combos$r2),
          # label 1 if acceptable dif, 0 if not
           d434note = ifelse(dif < 0.006, 1, 0))
  
# Dye - 730
  combos <- as_tibble(t(combn(qcdata$D_WL730, 2)))
  rows <- as_tibble(t(combn(qcdata$replicate, 2)))
  colnames(combos) <- c('r1', 'r2')
  
  d730combos <- combos %>%
    mutate(name = paste(rows$V1, rows$V2),
           dif = abs(combos$r1 - combos$r2),
           # label 'good' if acceptable dif, 'bad' if not
           d730note = ifelse(dif < 0.006, 1, 0))
  # filter out 'good' notes and add back to sheet

# Dye - 578
  combos <- as_tibble(t(combn(qcdata$D_WL578, 2)))
  rows <- as_tibble(t(combn(qcdata$replicate, 2)))
  colnames(combos) <- c('r1', 'r2')
  
  d578combos <- combos %>%
    mutate(name = paste(rows$V1, rows$V2),
           dif = abs(combos$r1 - combos$r2),
           # label 'good' if acceptable dif, 'bad' if not
           d578note = ifelse(dif < 0.006, 1, 0))
  
# No Dye - 730
  combos <- as_tibble(t(combn(qcdata$ND_WL730, 2)))
  rows <- as_tibble(t(combn(qcdata$replicate, 2)))
  colnames(combos) <- c('r1', 'r2')
  
  nd730combos <- combos %>%
    mutate(name = paste(rows$V1, rows$V2),
           dif = abs(combos$r1 - combos$r2),
           # label 'good' if acceptable dif, 'bad' if not
           nd730note = ifelse(dif < 0.002, 1, 0))

# No Dye - 578
  combos <- as_tibble(t(combn(qcdata$ND_WL578, 2)))
  rows <- as_tibble(t(combn(qcdata$replicate, 2)))
  colnames(combos) <- c('r1', 'r2')
  
  nd578combos <- combos %>%
    mutate(name = paste(rows$V1, rows$V2),
           dif = abs(combos$r1 - combos$r2),
           # label 'good' if acceptable dif, 'bad' if not
           nd578note = ifelse(dif < 0.002, 1, 0))
  
# No Dye - 434
  combos <- as_tibble(t(combn(qcdata$ND_WL434, 2)))
  rows <- as_tibble(t(combn(qcdata$replicate, 2)))
  colnames(combos) <- c('r1', 'r2')
  
  nd434combos <- combos %>%
    mutate(name = paste(rows$V1, rows$V2),
           dif = abs(combos$r1 - combos$r2),
           # label 'good' if acceptable dif, 'bad' if not
           nd434note = ifelse(dif < 0.002, 1, 0))

# put all notes together
  qc_df <- cbind(nd730combos$nd730note, nd578combos$nd578note,
                 nd434combos$nd434note, d730combos$d730note,
                 d578combos$d578note, d434combos$d434note)
  qc_df <- as_tibble(qc_df)
  qc_df$replicates <- paste(rows$V1, rows$V2)
  colnames(qc_df) <- c('nd730', 'nd578', 'nd434', 'd730', 'd578',
                       'd434', 'replicates')
  
# assign final value of good/bad (if one bad, all bad)
  qc_df$rowsums <- qc_df %>%
    select(nd730:d434) %>%
    rowSums()
  
  qc_df$goodbad <- ifelse(qc_df$rowsums < 6, 'bad', 'good')
  
# get letters from good pairs
  goodletters <- qc_df %>%
    # get good replicate pairs
    filter(goodbad == 'good') %>%
    # select only replicates column
    select(replicates) %>%
    # split into individual letters
    mutate(r1 = substr(replicates, 1,1),
           r2 = substr(replicates, 3,3)) %>%
    select(r1,r2) %>%
    # get unique entries
    t %>% c %>% unique

# put back into original data
  qcdata$goodbad <- ifelse(qcdata$replicate %in% goodletters,'good','bad')
  
# get only good/bad and join to list
  qc_list <- rbind(qc_list, select(qcdata, key, replicate, goodbad))
  
}
  
# merge w/ original dataset (to retain replicates)
qc <- left_join(calcph, qc_list, by = c('key', 'replicate'))

# save dataframe
write_csv(qc, 'qc_phdata_check.csv')

##### Summarize & merge spec pH data with master #####
# summary data to merge with master (avg ph across acceptable replicates)
qc1 <- qc %>%
  group_by(key) %>%
  # get acceptable replicates
  filter(goodbad == 'good') %>%
  summarize(avg_pH_corr = mean(pH_corr))

# make // key column in master
master <- mutate(master,
                 key = paste(Month, Year, '-', On_Off, 
                             '-T', Time_point, '-', Day_night,
                             '-P', Pool, sep = ''))

# join ph data to master
master <- left_join(master, qc1, by = 'key')

# clean environment
remove(excludelist, goodletters, i, rows, qcdata, qc_df, nd730combos, nd578combos, nd434combos, d730combos, d578combos, d434combos, combos)

# add notes with telling various situations
master$Spec_pH_Notes <-  
  # if sample excluded (NA, NaN, or 0 for calculated pH, or < 3 replicates)...
  ifelse(master$key %in% excludelist1, 
         'calculated pH value was NA, NaN or 0', 
  # if sample excluded for < 3 replicates...
  ifelse(master$key %in% excludelist2,
         '< 3 replicates run for sample',
  # if at least 2 samples match...
  ifelse(master$key %in% samplelist, 
         'sample run & QCed by LP code',
  # if 3 samples run, but all rejected...
  ifelse(master$key %in% samplelist & is.na(master$avg_pH_corr), 
         'sample run, all 3 replicates rejected', 
  # if none of these condition matched, sample not run
  'sample not run')))) 

remove(qc, qc_list, qc1, excludelist1, excludelist2, samplelist)

##### Organize master datasheet #####
master1 <- master %>%
  # make ph columns match master columns
  mutate(Hannah_pH_Total_Scale = Hanna_pH_calculated,
         Spec_pH_Total_Scale = avg_pH_corr) %>%
  # select columns from master excel sheet
  select("SampleID", "Date", "Time_point", "Pool", 'Time_water_collected', "Temp", 'Hanna_pH', 'Salinity', 'DO', 'Light', 'Field_Notes', 'blank1', 'Hannah_pH_Total_Scale', 'blank2', 'Spec_pH_abs', 'Spec_pH_mV', 'Spec_pH_Total_Scale', 'Spec_pH_Notes', 'blank3', 'TA', 'TA_Corrected', 'TA_Notes', 'blank4', 'Corrected_NN', 'Corrected_Nitrite', 'Corrected_Nitrate', 'Phosphate', 'Ammonium', 'Nutrients_Notes') 

write_csv(master1, 'waterchem_data_complete_06042020.csv')

# make own master to work with (extra columsn for day/night and on/off etc.)
master2 <- master %>%
  # make ph columns match master columns
  mutate(Hannah_pH_Total_Scale = Hanna_pH_calculated,
         Spec_pH_Total_Scale = avg_pH_corr) %>%
  # select columns from master excel sheet
  select("SampleID", "Date", "Time_point", "Pool", 'Day_night', 'Month', 'Year', 'On_Off', 'key', 'Salinity', 'DO', 'Spec_pH_Total_Scale', 'Spec_pH_Notes', 'TA', 'TA_Corrected', 'TA_Notes') 

###### Get list of completed pH and TA samples #####
# which bottles can be emptied?

# conservative appraoch (no TA or spec pH notes, both present)
conservative_goodsamples <- master2 %>%
    filter(Spec_pH_Notes == 'sample run & QCed by LP code' &
           TA_Notes == 'NA,NA' 
           & !is.na(Spec_pH_Total_Scale) 
           & !is.na(TA)) %>%
    # isolate keys for good samples
    select(SampleID:key)

# export as csv
write_csv(conservative_goodsamples, 'conservative_bottles_OK_empty_06042020.csv')


# not conservative approach (both present - spec notes filter only)
not_conservative_goodsamples <- master2 %>%
  filter(Spec_pH_Notes == 'sample run & QCed by LP code'
         & !is.na(Spec_pH_Total_Scale) 
         & !is.na(TA)) %>%
  # isolate keys for good samples
  select(SampleID:key)

# export as csv
write_csv(not_conservative_goodsamples, 'NOT_conservative_bottles_OK_empty_06042020.csv')

##### Plots for pH QC #####
# make pool an ordered factor
master2$Pool <- ordered(master2$Pool, levels = c(
  '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36', 'ocean', 'ocean1', 'ocean2'))

# start with January 2019

jan19 <- master2 %>%
  filter(Month == 'January' & Year == '2019') %>%
  select('key', 'Month', 'Year', 'On_Off', 'Time_point', 'Pool',
         'Day_night', 'Spec_pH_Total_Scale', 'Spec_pH_Notes') %>%
  # make pools all #'s (ocean = pool 37, ocean1 = 38, ocean2 =39)
  mutate(Pool2 = ifelse(Pool %in% c(1:36), Pool,
                 ifelse(Pool == 'ocean1', 38,
                 ifelse(Pool == 'ocean2', 39,
                 ifelse(Pool == 'ocean', 37, NA))))) %>%
  # add column to wrap by (limit to 5-7 pools/plot)
  mutate(poolgroup = ifelse(Pool %in% c(1:5), 'Pools 1-5',
                     ifelse(Pool %in% c(6:10), 'Pools 6-10',
                     ifelse(Pool %in% c(11:15), 'Pools 11-15',
                     ifelse(Pool %in% c(16:20), 'Pools 16-20',
                     ifelse(Pool %in% c(21:25), 'Pools 21-25',
                     ifelse(Pool %in% c(26:30), 'Pools 26-30',
                     ifelse(Pool %in% c(31:36, 'ocean', 'ocean1', 'ocean2'), 'Pools 31-36 & Ocean', 
                            NA)))))))) %>%
  filter(!is.na(poolgroup))



ggplot(data = jan19, 
       mapping = aes(x = as_factor(Time_point), 
                     y = Spec_pH_Total_Scale, group = Pool)) + 
  geom_point(mapping = aes(color = Pool)) + 
  geom_line (mapping = aes(color = Pool)) +
  facet_wrap(~Day_night) + 
  xlab('Time Point') + 
  ylab('Spec pH') +
  ggtitle('January 2019 Sampling') + 
  theme_bw() +
  theme(legend.position = 'bottom') +
guides(fill=guide_legend(nrow=2, byrow=TRUE))

  ggsave('Jan19_DayNight_pH.png', width = 6, height = 6, unit = 'in')
  

spring19 <- master2 %>%
  filter(Month == 'March' & Year == '2019') %>%
  select('key', 'Month', 'Year', 'On_Off', 'Time_point', 'Pool',
         'Day_night', 'Spec_pH_Total_Scale', 'Spec_pH_Notes') %>%
  # make pools all #'s (ocean = pool 37, ocean1 = 38, ocean2 =39)
  mutate(Pool2 = ifelse(Pool %in% c(1:36), Pool,
                ifelse(Pool == 'ocean1', 38,
                ifelse(Pool == 'ocean2', 39,
                ifelse(Pool == 'ocean', 37, NA))))) %>%
  # add column to wrap by (limit to 5-7 pools/plot)
  mutate(poolgroup = ifelse(Pool %in% c(1:5), 'Pools 1-5',
                    ifelse(Pool %in% c(6:10), 'Pools 6-10',
                    ifelse(Pool %in% c(11:15), 'Pools 11-15',
                    ifelse(Pool %in% c(16:20), 'Pools 16-20',
                    ifelse(Pool %in% c(21:25), 'Pools 21-25',
                    ifelse(Pool %in% c(26:30), 'Pools 26-30',                        ifelse(Pool %in% c(31:36, 'ocean', 'ocean1',
                    'ocean2'), 'Pools 31-36 & Ocean', 
                    NA)))))))) %>%
  filter(!is.na(poolgroup))



ggplot(data = spring19, 
       mapping = aes(x = as_factor(Time_point), 
                     y = Spec_pH_Total_Scale, group = Pool)) + 
  geom_point(mapping = aes(color = Pool)) + 
  geom_line (mapping = aes(color = Pool)) +
  facet_wrap(~Day_night) + 
  xlab('Time Point') + 
  ylab('Spec pH') +
  ggtitle('Spring (March) 2019 Sampling') + 
  theme_bw() +
  theme(legend.position = 'bottom') +
  guides(fill=guide_legend(nrow=2, byrow=TRUE))


ggsave('Spring19_DayNight_pH.png', width = 6, height = 6, unit = 'in')


  
  
  