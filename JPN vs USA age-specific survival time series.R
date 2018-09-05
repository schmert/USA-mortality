#---------------------------------------------
# Carl Schmertmann
# created 5 Sep 18
# altered 5 Sep 18
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
           filter( Age %in% c(0,20,40,60,80,100), Year > 1946) %>%
           group_by(PopName, Year) %>% 
           summarize( p0 = lx[Age==20] / lx[Age==0],
                      p20 = lx[Age==40] / lx[Age==20],
                      p40 = lx[Age==60] / lx[Age==40],
                      p60 = lx[Age==80] / lx[Age==60],
                      p80 = lx[Age==100] / lx[Age==80]) %>%
           gather(key='var', value='prob', -PopName, -Year) %>%
           mutate( var = factor(var, 
                                levels=paste0('p',seq(0,80,20)),
                                labels=paste0('Survival from\n',
                                              seq(0,80,20),'-',
                                              20+seq(0,80,20))))



ggplot( data=keep.df, aes(x=Year, y=prob, group=PopName, color=PopName)) +
         geom_point() +
         geom_line() +
         geom_hline( yintercept = 0) +
         facet_grid(~var) +
         theme_bw() +
         labs(title='Probability of Survival over 20-Year age intervals\nJapanese and US Females 1947-2016',
              y="Probability of Survival (Females)",
              color='Country',
              caption='Source: Human Mortality Database      http://mortality.org')


ggsave(file='JPN vs USA age-specific survival time series.png',
       width=13, height=6)

