################################################################
# Title: Sitka NSF Water Chemistry Data Assembly
# Purpose: Assemble and plot nitrate, nitrite, phosphate and ammonium data
# Created by: L Pandori
# Created: 06/25/2020
# Last edited: 10/05/2020
################################################################
###### How the code works #####
# This is a user-friendly version of the Sitka NSF Water Chemistry Figure generator

# Step 1 - Run package upload (G's computer has all of these packages. If you don't have a package on your computer, go to "packages" in the lower right hand corner of your R studio windows and type in the package you want to install it)

# Step 2 - Change the name of the file you want to input - it must be a CSV of nutrient data. For an example of what the data format should be, check the Sitka NSF folder and select Data - Nutrients, then pull up the CSV in the folder "Nutrients_June2020". You can use this file as a template, just "save as" a different name on the computer

# Step 3 - Upload the file by running the read_csv line...it should pop up in the "environment" window

# Step 4 - Highlight and run the remainder of the code. If something is wrong, it is likely that your data are in a different format than the CSV template. Check capitalzation, naming conventions, etc.

# Step 5 - Once code has successfully run, your figures should pop up in your working directory (to locate this folder, look at the pathway in the lower right hand window under the "files" tab)

# Step 6 - If desired, copy and paste figures into a power point for QA/QC 

##### Package upload #####

# Load libraries
library(tidyverse)
library(viridis) # good continuous color palettes
library(patchwork) # arrange multiple plot display
library(ggrepel) # nice point labels w/o crowding

##### INSTRUCTIONS #####

# change name of file to one you want to upload
chem <- read_csv("September2020_Nutrients_SitkaNSF_10022020.csv")

# create ordered factors for pool, IDR and station

# pool 
chem$Pool <- ordered(chem$Pool, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36', 'ocean', 'ocean1', 'ocean2'))

# IDR
chem$Timepoint <- ordered(chem$Timepoint, levels = unique(chem$Timepoint))

# station
chem$DayNight <- ordered(chem$DayNight, levels = unique(chem$DayNight))

##### Calculations #####

# calculate Nitrate from N+N
chem <- chem %>%
  mutate(Nitrate = ifelse(Nitrite > 0, 
                          NN - Nitrite, NN)) %>%
  filter(!is.na(DayNight))

##### Plots #####

# Goal: plot ammonium, nitrite, nitrate and phosphate across time points for each pool & station

# Make list of variables over which to plot
varlist <- c('Ammonium', 'Nitrate', 'Nitrite', 'Phosphate')

# Make list of stations over which to plot
stationlist <- unique(chem$DayNight)

# Set custom theme (ll theme)
lltheme <- theme_set(
  theme_bw() + 
    theme(text = element_text(size = 12),
          # no background to wrap panels
          strip.background = element_blank(),
          # panel labels outside x axis labels
          strip.placement = 'outside',
          # tilt and adjust position of x axis labels
          axis.text.y = element_text(size = 12, color = 'black'),
          axis.text.x = element_text(angle = 45, 
                                     hjust = 1, size = 12, 
                                     color = 'black'),
          strip.text.x = element_text(size = 12, 
                                      color = 'black'), 
          # no legends
          legend.position = 'none',
          plot.caption = element_text(family = 'serif', size = 12, color = 'black', hjust = 0))
)

# Loop plot across variable list

for(i in 1:length(varlist)) {
  
  # select relevant data
  chem1 <- chem %>% 
    select(DayNight, Pool, Timepoint, newcol = varlist[i])
  
  # create overview plot
  ggplot(data = chem1, mapping = aes(x = Timepoint, y = newcol, group = Pool)) + 
    geom_point(mapping = aes(color = Pool)) + 
    geom_line(mapping = aes(color = Pool)) + 
    geom_text_repel(data = filter(chem1, Timepoint == 'T3'),
                    mapping = aes(label = Pool)) +
    facet_wrap(~DayNight) +
    xlab('Time Point') + 
    ylab (varlist[i]) +
    ggtitle(paste(varlist[i], 'Overview:', 
                  chem$Site[1],chem$Month[1], chem$Year[1])) 
  
  # save overview plot
  plotname <- paste(varlist[i], 'Overview', chem$Site[1],chem$Month[1], chem$Year[1], Sys.Date(), '.png', sep = '_')
  
  ggsave(plotname, height = 4, width = 6, units = 'in')
  
# create pool-specific plots (day and night on same panel)
  ggplot(data = chem1,
         mapping = aes(x = as_factor(Timepoint), 
                       y = newcol, group = DayNight)) + 
    geom_point(mapping = aes(color = DayNight)) + 
    geom_line (mapping = aes(color = DayNight)) +
    facet_wrap(~Pool) +
    xlab('Time Point') + 
    ylab (varlist[i]) +
    scale_color_manual(values = c('firebrick1', 'dodgerblue4')) +
    theme_bw() +
    theme(legend.position = 'bottom') +
      ggtitle(paste(varlist[i], 'Detail:', chem$Site[1],
                    chem$Month[1], chem$Year[1])) 
    
    # save pool-specific plots
    plotname <- paste(varlist[i], 'Detail', chem$Site[1],
                      chem$Month[1], chem$Year[1], Sys.Date(), '.png', sep = '_')
    
    ggsave(plotname, height = 8, width = 8, units = 'in')
}
