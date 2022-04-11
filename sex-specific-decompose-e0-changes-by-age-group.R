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
# this version grabs BOTH female and male tables and concatenates
# everything

need.to.build.df = !exists('big.state.df')

included_states = c('DC',state.abb)   # (intentionally) excludes DC

if (need.to.build.df) {
  
  big.state.df = tibble()
  
  for (this_sex in c('f','m','b')) {
    
    file_list = paste0('States/',included_states,'/',
                       included_states,'_',this_sex,'ltper_1x1.csv')
    
    unzip(zipfile='lifetables.zip', files=file_list, junkpaths=TRUE)  
    
      for (this.state in included_states) {
        
        this.file = paste0(this.state,'_',this_sex,'ltper_1x1.csv')
        this.df = read.csv(this.file, stringsAsFactors = FALSE)
        this.df$Age = 0:110 # make numeric
        this.df$Sex = this_sex
        big.state.df = bind_rows( big.state.df, this.df)
        
        file.remove(this.file)
  } # for this_state
  } # for this_sex
  
  
} # if need.to.build


# keep only the 2000 and 2019 tables
 
data = big.state.df %>% 
        filter(Year %in% c(2000, 2019)) 

# decompose e0 gains by age ----
# calculate the life expectancy gains Gx caused by
# 2000-2015 mortality change at ages x AND ABOVE
#   Gx = lx_2000 * (ex_2019 - ex_2000)

data = data %>% 
        mutate( ex_fine = Tx/lx,
                lx      = lx/1e5) %>% 
        select(PopName, Age, Sex,Year, lx, ex = ex_fine) %>% 
        pivot_wider(id_cols=c('PopName','Age','Sex'), 
                    values_from = c('lx','ex'),
                    names_from=Year) %>% 
        mutate(Gx = lx_2000 * (ex_2019 - ex_2000))

# plot total e0 gains by state ----

tmp = data %>% 
       group_by(PopName, Sex) %>% 
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
             subtitle=c('f'='Females', m='Males')[this_sex],
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
        scale_y_continuous(expand=c(0,.08)) +
        facet_wrap(~Sex)
  
# pick a few cases and look at the gains/losses by age 
# group

L = seq(0,100,10)

groups = paste(L, L+9, sep='-')
groups[11] = '100+'

disagg = data %>% 
        filter(Age %in% L) %>% 
        group_by(PopName, Sex) %>% 
        mutate( gain = c( -diff(Gx), last(Gx)))

show = function(this_abb,this_sex) {
  
  tmp = filter(disagg, PopName == this_abb, Sex==this_sex) %>% 
         mutate(sign= 1*(gain>0),
                hue = c('red', 'blue')[1+sign],
                agegroup = factor(Age,
                                  levels=L,
                                  labels=groups))
  
  this_state = state.name[ state.abb==tmp$PopName[1]]
  
  this_max    = 1.25
  this_text_y = 1.1
  
  if (this_abb == 'DC') {
    this_state = 'District of Columbia'
    this_max   = 1.6
    this_text_y = 1.45
  }  
  
  change_text = (tmp$ex_2019[1] - tmp$ex_2000[1]) %>% 
                     sprintf("%3.2f", .)
  
  if (tmp$ex_2019[1] > tmp$ex_2000[1]) change_text = paste0('+', change_text)
  
  overall_change = paste0(this_state,
                          c('f'=' Females', 'm'=' Males')[this_sex],
                          ", 2000-2019\nTotal Change in Life Expectancy = ", 
                          change_text, " yrs")
  
  G = ggplot(data=tmp) +
    aes(x=agegroup, y=gain) +
    geom_bar(stat='identity', fill=tmp$hue, alpha=.80) +
    labs(title='Life Expectancy Gains from\nMortality Changes at different ages',
         subtitle=c('f'=' Females', 'm'=' Males')[this_sex],
         y='Years of Life Gained from Mortality Change',
         x='Age Group',
         caption = 'Source: US Mortality Database, https://usa.mortality.org') +
    guides(fill='none') +
    scale_y_continuous(limits=c(-0.65, this_max),
                       breaks=seq(-0.5, 1.50, .25)) +
    geom_text(x=0.9, y=this_text_y, 
              label=overall_change, size=5, hjust=0) +
    theme_bw() +
    theme(axis.title = element_text(face='bold'),
          axis.text  = element_text(face='bold')
          )
    
  print(G)
}


G1 = show('IN','f')
G2 = show('IN','m')

CP = cowplot::plot_grid( G1, G2, nrow=1)

ggsave(filename='IN.png', plot=CP, height=6, width=11, dpi=300)


G1 = show('NC','f')
G2 = show('NC','m')

CP = cowplot::plot_grid( G1, G2, nrow=1)

ggsave(filename='NC.png', plot=CP, height=6, width=11, dpi=300)

G1 = show('OH','f')
G2 = show('OH','m')

CP = cowplot::plot_grid( G1, G2, nrow=1)

ggsave(filename='OH.png', plot=CP, height=6, width=11, dpi=300)


G1 = show('DC','f')
G2 = show('DC','m')

CP = cowplot::plot_grid( G1, G2, nrow=1)

ggsave(filename='DC.png', plot=CP, height=6, width=11, dpi=300)


G1 = show('TX','f')
G2 = show('TX','m')

CP = cowplot::plot_grid( G1, G2, nrow=1)

ggsave(filename='TX.png', plot=CP, height=6, width=11, dpi=300)


