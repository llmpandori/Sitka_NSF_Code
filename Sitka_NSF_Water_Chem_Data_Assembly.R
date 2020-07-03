################################################################
# Title: Sitka NSF Water Chemistry Data Assembly
# Purpose: Assemble and plot nitrate, nitrite, phosphate and ammonium data
# Created by: L Pandori
# Created: 06/25/2020
# Last edited: 07/02/2020
################################################################
##### Package upload #####

# Load libraries
library(tidyverse)
library(viridis) # good continuous color palettes
library(patchwork) # arrange multiple plot display
library(ggrepel) # nice point labels w/o crowding

##### Master data sheet upload & tidy #####

# change name of file to one you want to upload
chem <- read_csv("June2019_Nutrients_SitkaNSF_06252020.csv")

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
  # while we're here, get rid of rejected values (keep = 'No')
  filter(Keep == 'y')

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
