#---------------------------------------------
# Carl Schmertmann
# created 29 Sep 18
# altered 03 Oct 18
#
# visualize remaining life expectancy
#---------------------------------------------

rm(list=ls())
library(tidyverse)
library(gganimate)

graphics.off()

## California females 2015

file_name = 'CA_fltper_1x1.csv'
file_path = 'States/CA/CA_fltper_1x1.csv'
unzip(zipfile='lifetables.zip', files=file_path, junkpaths=TRUE)  

CA = read.csv(file_name, stringsAsFactors = FALSE) %>%
           filter(Year == 2015) %>%
           mutate(Age = 0:110,
                  lx = lx/lx[Age==0],
                  dx = dx/sum(dx)) %>%
           select(PopName:Age, lx, dx, ex)


file.remove(file_name)

#--------------------------
#  function to calculate
#  dx conditional on surv
#  to age z (returns a new data frame)
#--------------------------

conditional_survival = function(z) {
  CA %>% mutate( dx = ifelse(Age < z, 0, dx/lx[Age==z]),
                 ez = ex[Age==z],
                 ez.char = formatC( ez, format='f' , digits=1),
                 z = z)
}

# single long data frame with alternative dx columns conditional on
# surv to different reference ages
df = do.call( 'rbind', map( 0:100, conditional_survival))



  
  G = ggplot( data=df, aes(x=Age, y=dx)) +
        geom_segment(aes(x=Age+.5, xend=Age+.5, y=0, yend=dx), 
                     color='royalblue', inherit.aes = FALSE,
                     lwd=0.8) +
          geom_segment(aes(x=df$z,xend=df$z+df$ez,y=-.002, yend=-.002), 
                       lwd=1.2, color='black') +
          geom_text(x=df$z+df$ez/2, y=-.004, size=2.5,label=df$ez.char) +
            lims(y=range(-.005,df$dx)) +
          geom_text(x=10, y=.15, size=8, hjust=0,
                    label=paste0('Age ', df$z, '\ne(', df$z, ')= ',df$ez.char)) +
          labs(title = paste0('CA Females 2015'),
               x='Current Age', y='Fraction of Future Deaths',
               caption='Source: US Mortality Database http://usa.mortality.org') +
        transition_time(z) +
        theme_bw() 
    
  movie = animate(G, fps=4)
  
  anim_save('visualize-e0-using-dx-column.gif', 
            animation=movie )
  
  
