#---------------------------------------------
# Carl Schmertmann
# created 31 Aug 18
# altered 31 Aug 18
#
# differences in e(x) by age between MS and MN
#---------------------------------------------

rm(list=ls())
library(tidyverse)
library(viridis)

graphics.off()
if (.Platform$OS.type == 'windows') windows(record=TRUE)

included_states = c('MN','MS')


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
  
keep.df = state.df %>%
           filter( Year %in% c(1965,2015)) %>%
           group_by(Age, Year) %>% 
           summarize( MNadv = ex[PopName=='MN'] - ex[PopName=='MS'])

ggplot( data=keep.df, aes(x=Age, y=MNadv, group=Year, color=factor(Year))) +
         geom_point() +
         geom_line() +
         geom_hline( yintercept = 0) +
         scale_color_manual(values=c('red','blue')) +
         scale_x_continuous(breaks=seq(0,110,10)) +
         scale_y_continuous(breaks=seq(-1.5,6,0.5), limits=c(-1.3,6.2)) +
         theme_bw() +
         labs(title='How Much Longer Will Minnesotans live than Mississippians?\nby Current Age',
              x='Current Age', 
              y='Expected Years of Extra Life for Minnesotans',
              caption='ex[MN]-ex[MS], both sexes combined, from US Mortality Database http://usa.mortality.org') +
         guides(color=FALSE) +
         geom_segment(aes( x=40 , xend= 40, y=2.5, yend=4.5), arrow=arrow(), lwd=1.5, color='black') +
         geom_text( aes(x=42, y=3.5), hjust=0,label='Increasing\nAdvantage\nfor MN', size=3, color='black') +
         geom_text( aes(x=15, y=6.1), label='2015', size=6, col='blue') +
         geom_text( aes(x=15, y=3.2), label='1965', size=6, col='red')

ggsave(file='MN versus MS.png')
