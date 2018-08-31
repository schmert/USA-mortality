#---------------------------------------------
# Carl Schmertmann
# created 31 Aug 18
# altered 31 Aug 18
#
# adult survival trends for selected states
#---------------------------------------------

rm(list=ls())
library(tidyverse)
library(viridis)

graphics.off()
if (.Platform$OS.type == 'windows') windows(record=TRUE)

#--------------------------
# Life tables are in a big (50MB) zipped file from the USMDB 
# Extract the division-level, both-sex tables if necessary

need.to.build.df = !exists('state.df')

included_states = state.abb   # (intentionally) excludes DC

if (need.to.build.df) {
  file_list = paste0('States/',included_states,'/',
                     included_states,'_bltper_1x1.csv')
  unzip(zipfile='lifetables.zip', files=file_list, junkpaths=TRUE)  
  state.df = data.frame()
  for (this.state in included_states) {
    this.file = paste0(this.state,'_bltper_1x1.csv')
    this.df = read.csv(this.file, stringsAsFactors = FALSE)
    this.df$Age = 0:110 # make numeric
    state.df = rbind( state.df, this.df)
    
    file.remove(this.file)
  }
  

} # if need.to.build

#--------------------------
# calculate the prob of death
# between ages 25 and 65, conditional
# on survival to 25, for each 
# division and year
#--------------------------

adult.surv.df = 
    state.df %>%
      filter( Age %in% c(25,65)) %>%
      group_by(PopName,Year) %>% 
      summarize( psurv = lx[2]/lx[1])




plot.df = adult.surv.df %>% 
           filter(PopName %in% c('CA','KY','MN','MS','WV','OH'))

G = ggplot(data=plot.df, aes(x=Year, y=psurv, 
                                    group=PopName, 
                                    color=PopName)) +
        geom_point(size=1) +
        geom_smooth(lwd=1.2, se=FALSE, span=.30 ) +
        scale_x_continuous(breaks=seq(1960,2015,5), limits=c(1959,2020)) +
        scale_y_continuous(breaks=seq(.70,.90,.05), limits=c(.70,.90)) +  
        theme_bw() +
        theme( text = element_text(face='bold', size=14)) +
        labs(title='Prob of Surviving from Age 25 to 65\n(40p25, both sexes combined)',
             caption='Source: US Mortality Database http://usa.mortality.org',
             y='Prob of Survival') +
        geom_text(data=filter(plot.df, Year==max(Year)),
                  aes(x=2016, y=psurv, label=PopName),
                  hjust= 0, vjust=0,
                  size=5, face='bold') +
        guides(color=FALSE)  

print(G)



ggsave('state-level-adult-survival-both-sexes.png') 
      
      
      
      

