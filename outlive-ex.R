#---------------------------------------------
# Carl Schmertmann
# created 21 Oct 18
# altered 21 Oct 18
#
# probability of outliving e(x)
#---------------------------------------------

rm(list=ls())
library(tidyverse)

ages = 0:110

USA = read.table('USA_bltper_1x1.txt', skip=2, stringsAsFactors = FALSE,
                  header=TRUE)
USA$Age = ages # make Age numeric

USA$PopName = 'USA'

# function to take a data.frame composed of one country-year
# life table and return the probabilities of surviving longer
# than e(x) at each age x in 0...100

p_outlive = function(df) {
  tmp = df %>% 
           mutate(aad=Age+ex,
           lx_aad= approx(x=df$Age,y=df$lx,xout=df$aad)$y/lx[1],
           p_outlive = lx_aad)
  return(tmp$p_outlive)
}


df = USA %>%
           group_by(Year) %>%
           mutate(aad=Age+ex,
                  lx_aad= approx(x=Age,y=lx,xout=aad)$y,
                  p_outlive = lx_aad/lx,
                  u = rep(runif(1),111))

keep.df = df %>%
           ungroup() %>%
           filter(Year %in% 2016) 


G = ggplot(data=keep.df,
       aes(x=Age, y=p_outlive)) + 
       geom_line(lwd=2,alpha=.80, color='red') + 
       geom_hline(yintercept = c(.40,.50,.60), lty=c(2,1,2)) + 
       xlim(0,100) + 
       ylim(.39,.61) +
       labs(title='Probability of Outliving Remaining Life Expectancy\nUSA 2016, Both Sexes Combined',
            x='Age', y='Prob',
            caption='Source: Human Mortality Database;   http://mortality.org') +
       scale_color_manual(values=c('red','royalblue','darkgreen')) +
       theme_bw() +
       theme(text=element_text(face='bold', size=15))

G


ggsave(file='outlive-ex.png', height=11, width=8)
