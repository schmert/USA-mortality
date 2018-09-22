#---------------------------------------------
# Carl Schmertmann
# created 21 Sep 18
# altered 21 Sep 18
#
# age at which e(x)=15, by state and period
#---------------------------------------------

rm(list=ls())
library(tidyverse)
library(viridis)

graphics.off()
if (.Platform$OS.type == 'windows') windows(record=TRUE, width=8, height=8)

included_states = sort( c(state.abb, 'DC'))


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

## find the age at which ex=15 from an ex column 
e_inverse = function( target_value=15, ex_column) {
  approx( x=ex_column, y=0:110, xout = target_value)$y
}
  
keep.df = state.df %>%
           group_by(PopName, Year) %>%
           summarize( x10 = e_inverse(10, ex),
                      x15 = e_inverse(15, ex)) %>%
           group_by(Year) %>% 
           mutate (rank10 = rank(x10),
                   rank15 = rank(x15))

ggplot(data = filter(keep.df, Year %in% c(1965,1990,2015)),
       aes(x=rank15, y=x15, label=PopName, color=as.factor(Year))) + 
  geom_text(size=2.8,  fontface='bold') +
  scale_color_manual(values=c('red','blue','black')) +
  guides(color=FALSE) +
  geom_text( aes(x=50, y=65.0), label='1965', size=5, col='red') +
  geom_text( aes(x=50, y=69.0), label='1990', size=5, col='blue') +
  geom_text( aes(x=50, y=72.0), label='2015', size=5, col='black') +
  labs(title='At which age you can expect to live 15 more years?\nby state of residence',
     x='State Rank', 
     y='Age at which e(x)=15',
     caption='Both sexes combined, from US Mortality Database http://usa.mortality.org') +
  theme(text=element_text(face='bold')) +
  theme_bw() +
  coord_flip()
  

ggsave(file='when-is-ex-equal-15.png', height=8.5, width=11)
