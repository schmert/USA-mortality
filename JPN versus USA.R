#---------------------------------------------
# Carl Schmertmann
# created 4 Sep 18
# altered 4 Sep 18
#
# differences in female e(x) by age between Japan and USA
#---------------------------------------------

rm(list=ls())
library(tidyverse)

graphics.off()
if (.Platform$OS.type == 'windows') windows(record=TRUE)

JPN = read.table('JPN_fltper_1x1.txt', skip=2, stringsAsFactors = FALSE,
                  header=TRUE)
JPN$Age = 0:110  # make Age numeric
JPN$PopName = 'JPN'

USA = read.table('USA_fltper_1x1.txt', skip=2, stringsAsFactors = FALSE,
                  header=TRUE)
USA$Age = 0:110  # make Age numeric
USA$PopName = 'USA'

keep.df = rbind(JPN,USA) %>%
           filter( Year %in% c(1965,1990,2015)) %>%
           group_by(Age, Year) %>% 
           summarize( JPNadv = ex[PopName=='JPN'] - ex[PopName=='USA'])


ggplot( data=keep.df, aes(x=Age, y=JPNadv, group=Year, color=factor(Year))) +
         geom_point() +
         geom_line() +
         geom_hline( yintercept = 0) +
         scale_color_manual(values=c('red','royalblue','darkgreen')) +
         scale_x_continuous(breaks=seq(0,110,10)) +
         scale_y_continuous(breaks=seq(-2,6,0.5), limits=c(-2,6.2)) +
         theme_bw() +
         labs(title='How Much Longer can Japanese Women expect to live\nthan American Women?\nby Age',
              x='Age', 
              y='Expected Years of Extra Life for Japanese',
              caption='ex[JPN females]-ex[USA females]       http://mortality.org') +
         guides(color=FALSE) +
         geom_text( aes(x=15, y=5.8), label='in 2015', size=6, col='darkgreen') +
         geom_text( aes(x=15, y=3.0), label='in 1990', size=6, col='royalblue') +
         geom_text( aes(x=15, y=-1.0), label='in 1965', size=6, col='red') 

ggsave(file='JPN vs USA.png')
