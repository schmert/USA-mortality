#--------------------------------------------
# Carl Schmertmann
# created 13 Oct 18
# altered 13 Oct 18
#
# (period) probability of surviving from birth to age 5, by
# state and year
#---------------------------------------------

rm(list=ls())
library(tidyverse)
library(gganimate)

graphics.off()
if (.Platform$OS.type == 'windows') windows(record=TRUE)

#--------------------------
# Life tables are in a big (50MB) zipped file from the USMDB 
# Extract the division-level, both-sex tables if necessary

need.to.build.df = !exists('state.df')

#included_states = c('MN','UT','SC','MS')
#included_states = state.abb
included_states = c('MN','CO','SC','MS')

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
# between ages 0 and 5
#--------------------------

q5.df = 
    state.df %>%
      group_by(PopName,Year) %>% 
      summarize( q5 = 1000 * (1 - lx[6]/lx[1])) %>%
      ungroup()

#--------------------------
# function to returns a 
# partial data frame, with
# data up to and including
# the cutoff year, but not 
# after
#--------------------------

partial_timeseries = function(this.year) {
  q5.df %>% 
    filter( Year <= this.year) %>%
    mutate(cutoff  = this.year,
           stateabb = ifelse(Year==this.year, PopName,NA))
}


# single long data frame with alternative dx columns conditional on
# surv to different reference ages
df = do.call( 'rbind', map( 1959:2015, partial_timeseries))

#######################################

G = ggplot( data=df, aes(x=Year, y=q5, color=PopName)) +
   geom_line(lwd=2) + 
#   geom_text( x=df$cutoff+2, y=df$q5, 
#              hjust='left',
#              label=df$stateabb, size=5) +
   geom_text( x=2000, y=40, size=12, color='grey',
              hjust='left', label=paste(df$cutoff)) +
   labs(title = 'Deaths before Age 5, Per 1000 Newborns of Both Sexes',
        y='1000 q(5)',
        caption='Source: US Mortality Database http://usa.mortality.org') +
  transition_time(df$cutoff) +
  theme_bw() +
  theme(text=element_text(face='bold')) +
  scale_x_continuous(limits=c(1957,2019)) +
  guides(color=FALSE)

movie = animate(G, fps=5)

anim_save('state-level-q5-both-sexes.gif', 
          animation=movie )

