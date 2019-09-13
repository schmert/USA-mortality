#---------------------------------------------
# Carl Schmertmann
# created 13 Sep 19
# altered 13 Sep 19
#
# time trends in quantiles of female e(50)
# across states
#---------------------------------------------

rm(list=ls())
library(tidyverse)
library(viridis)

graphics.off()
if (.Platform$OS.type == 'windows') windows(record=TRUE)

included_states = sort( c(state.abb, 'DC') )

file_list = paste0('States/',included_states,'/',
                   included_states,'_fltper_1x1.csv')
unzip(zipfile='lifetables.zip', files=file_list, junkpaths=TRUE)  

state.df = data.frame()

for (this.state in included_states) {
  this.file = paste0(this.state,'_fltper_1x1.csv')
  this.df = read.csv(this.file, stringsAsFactors = FALSE)
  this.df$Age = 0:110 # make numeric
  state.df = rbind( state.df, this.df)
  
  file.remove(this.file)
}

df = state.df %>%
       filter(Age==50) %>%
       group_by(Year) %>%
       summarize(L=min(ex), 
                 H=max(ex),
                 Lstate = PopName[which.min(ex)], 
                 Hstate = PopName[which.max(ex)]) %>%
       mutate(gap = H-L)

ggplot(data=df) +
  aes(x=Year, y=L, label=Lstate) +
  geom_text(size=3) +
  geom_text( aes(x=Year,y=H, label=Hstate), color='black',size=3) +
  geom_segment(aes(x=Year,xend=Year, y=L, yend=H), lwd=1, color='red', 
               alpha=.40) +
  scale_x_continuous(breaks=seq(1960,2015,5)) +
  annotate('label', x=min(df$Year), y=(head(df$L,1)+head(df$H,1))/2,
           label=paste(head(df$gap,1), 'Yrs'), size=4)+
  annotate('label', x=max(df$Year), y=(tail(df$L,1)+tail(df$H,1))/2,
           label=paste(tail(df$gap,1), 'Yrs'), size=4)+
  theme_bw() +
  labs(y='e(50)',
       title='Range of Female Life Expectancy at Age 50 across US States',
       caption='Source: USA Mortality Database http://usa.mortality.org') +
  theme(axis.title = element_text(face='bold', size=14),
        axis.text   = element_text(face='bold', size=12))

ggsave(file='e50-extremes.png', width=10, height=7, units='in', dpi=300)

