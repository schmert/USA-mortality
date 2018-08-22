#---------------------------------------------
# Carl Schmertmann
# created 22 Aug 18
# altered 22 Aug 18
#
# Quick USMDB experiment: adult mortality trends 
# in Kentucky
#---------------------------------------------

rm(list=ls())
library(tidyverse)
graphics.off()
if (.Platform$OS.type == 'windows') windows(record=TRUE)

KY     = read.csv('KY_bltper_5x1.csv') 
KY$Age = c(0,1,seq(5,110,5))    # make ages numeric

q = KY %>%
      filter(Age %in% c(25,65)) %>%
      group_by(Year) %>%
      summarize( q2565 = 1- lx[2]/lx[1])

ggplot(data=q, aes(x=Year, y=q2565)) +
     geom_point(size=3) +
     geom_smooth(lwd=2,color='red',fill='orangered') +
     scale_x_continuous(breaks=seq(1960,2015,5)) +
     theme_bw() +
     theme( text = element_text(face='bold', size=14)) +
     labs(title='KENTUCKY\nProbability of Death Between Ages 25-65\n(40q25, both sexes combined)',
          caption='Source: US Mortality Database http://usa.mortality.org')
     
ggsave('KY-adult-mortality-both-sexes.png') 



