#......................................
# Carl Schmertmann
# 10 Apr 2022
# 
# decompose changes in state-level life 
# expectancy at birth into age-group-specific
# components
#......................................

library(tidyverse)

#........
# Life tables are in a big (50MB) zipped file from the USMDB 
# Extract the division-level, both-sex tables if necessary

# build dataframe of state life tables, if not already built ----
need.to.build.df = !exists('state.df')

included_states = state.abb   # (intentionally) excludes DC

if (need.to.build.df) {
  file_list = paste0('States/',included_states,'/',
                     included_states,'_bltper_1x1.csv')
  unzip(zipfile='lifetables.zip', files=file_list, junkpaths=TRUE)  
  state.df = data.frame()
  for (this.state in included_states) {
    this.file = paste0(this.state,'_bltper_1x1.csv')
    this.df = read.csv(this.file, stringsAsFactors = FALSE)
    this.df$Age = 0:110 # make numeric
    state.df = bind_rows( state.df, this.df)
    
    file.remove(this.file)
  }
  
  
} # if need.to.build

# keep only the 2000 and 2019 tables
 
data = state.df %>% 
        filter(Year %in% c(2000, 2019)) 

# decompose e0 gains by age ----
# calculate the life expectancy gains Gx caused by
# 2000-2015 mortality change at ages x AND ABOVE
#   Gx = lx_2000 * (ex_2019 - ex_2000)

data = data %>% 
        mutate( ex_fine = Tx/lx,
                lx      = lx/1e5) %>% 
        select(PopName, Age, Year, lx, ex) %>% 
        pivot_wider(id_cols=c('PopName','Age'), 
                    values_from = c('lx','ex'),
                    names_from=Year) %>% 
        mutate(Gx = lx_2000 * (ex_2019 - ex_2000))

# plot total e0 gains by state ----

tmp = data %>% 
       group_by(PopName) %>% 
       summarize( change = ex_2019[Age==0] - ex_2000[Age==0]) %>% 
       mutate(gain = (change>0))

ggplot(data=arrange(tmp,change)) +
        aes(x=reorder(PopName,change), y=change) +
        geom_bar(stat='identity',width=0.6, aes(fill=gain)) +
        scale_fill_manual(values=c('red','blue')) +
        scale_color_manual(values=c('red','blue')) +
        guides(fill='none', color='none') +
        labs(x = '',y='Additional Years of Expected Life, 2019',
             title='Change in Life Expectancy at Birth for US States, 2000-2019',
             subtitle='Both Sexes Combined',
             caption = 'Source: US Mortality Database, https://usa.mortality.org') +
        theme_bw() +
        geom_text( aes(label=PopName, color=gain), 
                   nudge_y = .12, size=2.8,
                   angle=90) +
        theme(panel.grid.major.x  = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(face='bold'),
              axis.title.y = element_text(face='bold')) +
        scale_y_continuous(expand=c(0,.08))
  
ggsave('overall-change-in-e0-2000-2019-by-state.png')

# pick a few cases and look at the gains/losses by age 
# group

L = seq(0,100,10)

groups = paste(L, L+9, sep='-')
groups[11] = '100+'

disagg = data %>% 
        filter(Age %in% L) %>% 
        group_by(PopName) %>% 
        mutate( gain = c( -diff(Gx), last(Gx)))

show = function(abb) {
  
tmp = filter(disagg, PopName == abb) %>% 
       mutate(sign= 1*(gain>0),
              hue = c('red', 'blue')[1+sign],
              agegroup = factor(Age,
                                levels=L,
                                labels=groups))

this_state = state.name[ state.abb==tmp$PopName[1]]

G = ggplot(data=tmp) +
  aes(x=agegroup, y=gain) +
  geom_bar(stat='identity', fill=tmp$hue) +
  labs(title='Life Expectancy Gains from Mortality Changes w/in Age Groups',
         subtitle=paste0(this_state, ' 2000-2019, both sexes combined'),
       y='Years of Life Gained',
       x='Age Group',
       caption = 'Source: US Mortality Database, https://usa.mortality.org') +
  guides(fill='none') +
  scale_y_continuous(limits=c(-0.65, 1.20),
                     breaks=seq(-0.5, 1, .25)) +
  theme_bw() 
  
print(G)
}

show('WV')
ggsave('WV-dissag.png')

show('CA')
ggsave('CA-dissag.png')

pdf('state-e0-decompositions-2000-2019.pdf')

for (this_abb in state.abb) {
  show(this_abb)
}

dev.off()

