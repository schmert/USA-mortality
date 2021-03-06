#---------------------------------------------
# Carl Schmertmann
# created 30 Aug 18
# altered 30 Aug 18
#
# adult mortality trends by Census Division
#---------------------------------------------

rm(list=ls())
library(tidyverse)
library(viridis)

graphics.off()
if (.Platform$OS.type == 'windows') windows(record=TRUE)

#--------------------------
# Life tables are in a big (50MB) zipped file from the USMDB 
# Extract the division-level, both-sex tables if necessary

need.to.build.df = !exists('division.df')

if (need.to.build.df) {
  file_list = paste0('Divisions/Div',1:9,'/Div',1:9,'_bltper_1x1.csv')
  unzip(zipfile='lifetables.zip', files=file_list, junkpaths=TRUE)  
  division.df = data.frame()
  for (this.div in 1:9) {
    this.file = paste0('Div',this.div,'_bltper_1x1.csv')
    this.df = read.csv(this.file, stringsAsFactors = FALSE)
    this.df$Age = 0:110 # make numeric
    division.df = rbind( division.df, this.df)
  }

  pretty_division_names = c('New England', 'Mid Atl',
                            'E North Centr', 'W North Centr',
                            'S Atlantic', 'E South Centr',
                            'W South Centr', 'Mountain', 
                            'Pacific')
    
    
  division.df$division_name = factor(division.df$PopName,
                                     levels = paste0('Div',1:9),
                                     labels = pretty_division_names)
} # if need.to.build

#--------------------------
# calculate the prob of death
# between ages 25 and 65, conditional
# on survival to 25, for each 
# division and year
#--------------------------

adult.mortality.df = 
    division.df %>%
      filter( Age %in% c(25,65)) %>%
      group_by(division_name,Year) %>% 
      summarize( q2565 = 1- lx[2]/lx[1])
  

ggplot(data=adult.mortality.df, aes(x=Year, y=q2565, 
                                    group=division_name, color=division_name)) +
        geom_point(size=1) +
        geom_smooth(lwd=1.2, se=FALSE ) +
        scale_x_continuous(breaks=seq(1960,2015,5), limits=c(1959,2025)) +
        scale_color_viridis(discrete=TRUE, option='magma') +
        theme_bw() +
        theme( text = element_text(face='bold', size=14)) +
        labs(title='Prob of Death Between Ages 25-65\n(40q25, both sexes combined)',
             caption='Source: US Mortality Database http://usa.mortality.org') +
        geom_text(data=filter(adult.mortality.df, Year==max(Year)),
                  aes(x=2016, y=q2565, label=division_name),hjust= 0, vjust=2,
                  size=4, face='bold') +
        guides(color=FALSE)  
      
ggsave('division-level-adult-mortality-both-sexes.png') 
      
      
      
      

