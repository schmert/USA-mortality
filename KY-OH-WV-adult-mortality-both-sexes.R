#---------------------------------------------
# Carl Schmertmann
# created 22 Aug 18
# altered 22 Aug 18
#
# Quick USMDB experiment: adult mortality trends 
# in Kentucky, Ohio, and West Va
#---------------------------------------------

rm(list=ls())
library(tidyverse)
graphics.off()
if (.Platform$OS.type == 'windows') windows(record=TRUE)

KY     = read.csv('KY_bltper_1x1.csv')
OH     = read.csv('OH_bltper_1x1.csv') 
WV     = read.csv('WV_bltper_1x1.csv') 

df = rbind(KY,OH,WV)

df$Age = c(0,1,seq(5,110,5))    # make ages numeric

q = df %>%
      filter(Age %in% c(25,65)) %>%
      group_by(PopName,Year) %>%
      summarize( q2565 = 1- lx[2]/lx[1])

ggplot(data=q, aes(x=Year, y=q2565, group=PopName, color=PopName)) +
     geom_point(size=3) +
     geom_smooth(lwd=2) +
     scale_x_continuous(breaks=seq(1960,2015,5)) +
     theme_bw() +
     theme( text = element_text(face='bold', size=14)) +
     labs(title='KY-OH-WV\nProbability of Death Between Ages 25-65\n(40q25, both sexes combined)',
          caption='Source: US Mortality Database http://usa.mortality.org')
     
ggsave('KY-OH-WV-adult-mortality-both-sexes.png') 



