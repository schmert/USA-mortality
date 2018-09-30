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
                  lx = lx/lx[Age==0],
                  dx = dx/sum(dx)) %>%
           select(PopName:Age, lx, dx, ex)


file.remove(file_name)

## make plot

make_ex_plot = function(x) {
  
  conditional_dx = df %>% 
    mutate(dx = ifelse(Age < x, 0, dx/lx[Age==x]))
  
  this.ex = df$ex[df$Age == x]
  this.ex.char = formatC( df$ex[df$Age == x], format='f' , digits=1)
  
  G = ggplot( data=conditional_dx, aes(x=Age, y=dx)) +
        geom_segment(aes(x=Age+.5, xend=Age+.5, y=0, yend=dx), 
                     color='royalblue', inherit.aes = FALSE,
                     lwd=0.8) +
          geom_segment(aes(x=x,xend=x+this.ex,y=-.002, yend=-.002), lwd=1.2, color='black') +
          geom_text(x=x+this.ex/2, y=-.004, size=2.5,label=this.ex.char) +
            lims(y=range(-.005,.14)) +
        theme_bw()
    
  #      geom_line(lwd=0.5, color='royalblue') +
  #      geom_hline(yintercept = 0) +
  #      theme_bw()
  # 
  # G = G + 
  #      geom_line(data=conditional_surv, col='red', lwd=1.2) +
  #      geom_area(data=remaining_life, fill='pink', color=NA,alpha=.50) +
  #      geom_polygon(data=box, fill='grey', alpha=.30) +
  #      labs(title= paste0('e(',x,') = ', this.ex.char)) +
  #      geom_line(data=boxtop, lwd=1.5, color='black', 
  #                arrow=arrow(length=unit(.05,'inches'))) +
  #      geom_text(x=x+this.ex/2, y=1.03, size=3,label=this.ex.char)
  
  print(G)
}


for (x in 0:90) make_ex_plot(x)

