#---------------------------------------------
# Carl Schmertmann
# created 24 Oct 18
# altered 24 Oct 18
#
# male e65 by state, over time
#---------------------------------------------

rm(list=ls())
library(tidyverse)
library(viridis)
library(maps)

graphics.off()
if (.Platform$OS.type == 'windows') windows(record=TRUE)

#--------------------------
# Life tables are in a big (50MB) zipped file from the USMDB 
# Extract the division-level, both-sex tables if necessary

need.to.build.df = !exists('state.df')

included_states = sort( c(state.abb,'DC'))   

if (need.to.build.df) {
  file_list = paste0('States/',included_states,'/',
                     included_states,'_mltper_1x1.csv')
  unzip(zipfile='lifetables.zip', files=file_list, junkpaths=TRUE)  
  state.df = data.frame()
  for (this.state in included_states) {
    this.file = paste0(this.state,'_mltper_1x1.csv')
    this.df = read.csv(this.file, stringsAsFactors = FALSE)
    this.df$Age = 0:110 # make numeric
    state.df = rbind( state.df, this.df)
    
    file.remove(this.file)
  }
  

} # if need.to.build



#--------------------------
# calculate the prob of death
# between ages 45 and 65, conditional
# on survival to 45, for each 
# division and year
#--------------------------

cutoff_age = 65

e65 = 
    state.df %>%
      filter( !(PopName %in% c('AK','HI'))) %>%
      group_by(PopName, Year) %>% 
      summarize( Y = round(Tx[Age==65]/lx[Age==cutoff_age], 1))


usa = map_data('state')

abb_table = data.frame( region = tolower(state.name),
                        PopName= state.abb)

df = left_join( usa, abb_table, by='region') %>% 
     left_join( filter(e65, Year==2015), by='PopName')

centroids = read.table(text='
PopName lat long
 AK 	63.588753 	-154.493062
 AL 	32.318231 	-86.902298 
 AR 	35.20105 	   -91.831833
 AZ 	34.048928 	-111.093731
 CA 	36.778261 	-119.417932
 CO 	39.550051 	-105.782067
 CT 	41.603221 	-73.087749 
 DC 	38.905985 	-77.033418 
 DE 	38.910832 	-75.52767 
 FL 	27.664827 	-81.515754
 GA 	32.157435 	-82.907123
 HI 	19.898682 	-155.665857
 IA 	41.878003 	-93.097702 
 ID 	44.068202 	-114.742041
 IL 	40.633125 	-89.398528 
 IN 	40.551217 	-85.802364 
 KS 	39.011902 	-98.484246 
 KY 	37.839333 	-84.270018 
 LA 	31.244823 	-92.145024 
 MA 	42.407211 	-71.382437 
 MD 	39.245755 	-76.741271 
 ME 	45.253783 	-69.445469 
 MI 	43.614844 	-85.302364 
 MN 	46.729553 	-94.6859 	
 MO 	37.964253 	-91.831833 
 MS 	32.354668 	-89.398528 
 MT 	46.879682 	-110.362566
 NC 	35.759573 	-79.0193 	
 ND 	47.551493 	-101.002012 
 NE 	41.492537 	-99.901813 	
 NH 	43.193852 	-71.572395 	
 NJ 	40.058324 	-74.405661 	
 NM 	34.97273 	-105.032363 	
 NV 	38.80261 	-116.419389 	
 NY 	43.299428 	-74.217933 	
 OH 	40.417287 	-82.907123 	
 OK 	35.007752 	-97.092877 	
 OR 	43.804133 	-120.554201 
 PA 	41.203322 	-77.194525 	
 PR 	18.220833 	-66.590149 	
 RI 	41.3      	-71.477429 	
 SC 	33.836081 	-81.163725 	
 SD 	43.969515 	-99.901813 	
 TN 	35.517491 	-86.580447 	
 TX 	31.968599 	-99.901813 	
 UT 	39.32098 	-111.093731 	
 VA 	37.431573 	-78.656894 	
 VT 	44.558803 	-72.577841 	
 WA 	47.751074 	-120.740139 
 WI 	43.78444 	-88.787868 	
 WV 	38.597626 	-80.454903
 WY 	43.075968 	-107.290284', header=TRUE) %>%
  left_join( filter(e65, Year==2015), by='PopName') %>%
  filter(is.finite(Y)) %>%
  mutate(cat=factor(Y<18, labels=c('H','L')))

ggplot(data=df,
       aes(x=long,y=lat,group=group,fill=Y)) +
     geom_polygon(color='grey', alpha=.90) +
     coord_map() +
     theme_minimal() +
     theme(axis.title= element_blank(),
           axis.ticks= element_blank(),
           axis.text=  element_blank(),
           axis.line=  element_blank(),
           panel.grid.major = element_blank(), 
           panel.grid.minor = element_blank() ) +
     scale_fill_viridis(option='plasma', alpha=.80) +
     geom_text( data=filter(centroids, cat=='L'),
                aes(x=long,y=lat, label=Y), inherit.aes = FALSE,
                color='white',size=3) +
     geom_text( data=filter(centroids, cat=='H'),
                aes(x=long,y=lat, label=Y), inherit.aes = FALSE,
                color='black',size=3) +
     labs(title=paste0('Expected Years of Future Social Security Benefits\n',
                       cutoff_age,'-yr-old males'),
          caption='Source: US Mortality Database (2015 life tables) http://usa.mortality.org',
          fill = 'Years of Benefits')
     

ggsave('e65-map-male.png', height=5, width=8) 
