#---------------------------------------------
# Carl Schmertmann
# created 16 Oct 18
# altered 16 Oct 18
#
# q(5) time series for selected states, animated
#---------------------------------------------
# devtools::install_github('thomasp85/gganimate')

rm(list=ls())
library(tidyverse)
library(gganimate)

need.to.build.df = !exists('state.df')

included_states = c('CO','MN','MS','SC')

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

#-----------------------------------
df = state.df %>%
      group_by(PopName,Year) %>% 
      summarize( q5 = 1000 * (1-lx[Age==5]/lx[Age==0]))

G = ggplot( data=df, aes(x=Year, y=q5, color=PopName)) +
    geom_line(lwd=2) +
    scale_x_continuous(limits=c(1958,2020)) +
    guides(color=FALSE) +
    geom_text(x=df$Year+3, 
              y=df$q5, 
              label=df$PopName, size=5) +
    labs(title = 'Deaths before Age 5, per 1000 newborns\nMississippi, South Carolina, Colorado, and Minnesota',
         x='Year', y='1000 q(5)',
         caption='Source: US Mortality Database http://usa.mortality.org') +
    theme_bw() +
    theme(text=element_text(face='bold', size=12)) +
    transition_reveal(id=PopName,along=Year)

movie = animate(G, fps=5)

anim_save('q5-state.gif', animation=movie )
