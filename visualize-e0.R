#---------------------------------------------
# Carl Schmertmann
# created 29 Sep 18
# altered 29 Sep 18
#
# visualize remaining life expectancy
#---------------------------------------------

rm(list=ls())
library(tidyverse)

graphics.off()
if (.Platform$OS.type == 'windows') windows(record=TRUE, width=8, height=8)

## California females 2015

file_name = 'CA_fltper_1x1.csv'
file_path = 'States/CA/CA_fltper_1x1.csv'
unzip(zipfile='lifetables.zip', files=file_path, junkpaths=TRUE)  

df = read.csv(file_name, stringsAsFactors = FALSE) %>%
           filter(Year == 2015) %>%
           mutate(Age = 0:110,
                  lx = lx/lx[Age==0]) %>%
           select(PopName:Age, lx, ex)


file.remove(file_name)

## make plot

make_ex_plot = function(x) {
  
  tmp = df %>% 
    mutate(lx = ifelse(Age < x, NA, lx/lx[Age==x]))
  
  tmp = rbind(tmp,tmp[ tmp$Age==x,])

  G = ggplot( data=df, aes(x=Age, y=lx)) +
       geom_line(lwd=1, color='red') +
       geom_hline(yintercept = 0) +
       theme_bw()
  
  G + geom_polygon(data=tmp, fill='pink', alpha=.70)
  
}

