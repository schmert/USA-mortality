#---------------------------------------------
# Carl Schmertmann
# created 5 Sep 18
# altered 5 Sep 18
#---------------------------------------------

rm(list=ls())
library(tidyverse)

graphics.off()
if (.Platform$OS.type == 'windows') windows(record=TRUE)

rm(list=ls())
library(tidyverse)
library(viridis)

graphics.off()
if (.Platform$OS.type == 'windows') windows(record=TRUE)

#--------------------------
# Life tables are in a big (50MB) zipped file from the USMDB 
# Extract the state-level, female tables if necessary

need.to.build.df = !exists('state.df')

included_states = state.abb   # (intentionally) excludes DC

if (need.to.build.df) {
  file_list = paste0('States/',included_states,'/',
                     included_states,'_mltper_1x1.csv')
  unzip(zipfile='lifetables.zip', files=file_list, junkpaths=TRUE)  
  state.df = data.frame()
  for (this.state in included_states) {
    this.file = paste0(this.state,'_mltper_1x1.csv')
    this.df = read.csv(this.file, stringsAsFactors = FALSE)
    this.df$Age = 0:110 # make numeric
    state.df = rbind( state.df, this.df)
    
    file.remove(this.file)
  }
  
  
} # if need.to.build

#--------------------------

L = seq(55,75,5)
H = L+5


keep.df = state.df %>%
           filter( PopName %in% c('NY','FL','OH','IL'),
                   Age %in% c(L,H)) %>%
           group_by(PopName, Year) %>% 
           summarize( p1 = lx[Age==H[1] ] / lx[Age==L[1] ],
                      p2 = lx[Age==H[2] ] / lx[Age==L[2] ],
                      p3 = lx[Age==H[3] ] / lx[Age==L[3] ],
                      p4 = lx[Age==H[4] ] / lx[Age==L[4] ],
                      p5 = lx[Age==H[5] ] / lx[Age==L[5] ]) %>%
           gather(key='var', value='prob', -PopName, -Year) %>%
           mutate( var = factor(var, 
                                levels=paste0('p',1:5),
                                labels=paste0('Survival from\n',
                                              L,'-',H)))

ggplot( data=keep.df, aes(x=Year, y=prob, group=PopName, color=PopName)) +
         geom_point(size=0.6) +
          geom_smooth(se=FALSE, lwd=1, alpha=.80) +
         geom_hline( yintercept = 0) +
         scale_y_continuous(limits=c(.60,1)) +
         facet_grid(~var) +
         theme_bw() +
         labs(title='Probability of Survival over age intervals\nUS Males 1959-2015',
              y="Probability of Survival (Males)",
              color='State',
              caption='Source: US Mortality Database      http://usa.mortality.org')


#ggsave(file='JPN vs USA age-specific survival time series.png',
#       width=13, height=6)

