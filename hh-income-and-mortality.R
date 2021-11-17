#...................................................................
# Carl Schmertmann
# 17 Nov 2021
# 
# US median HH income and mortality 
# by state, 1985 1995 2005 2015
# ..................................................................

library(tidyverse)

rm(list=ls())

# read state-level income information ----
# Median HH income data, extracted from Table H-08 at 
# https://www2.census.gov/programs-surveys/cps/tables/time-series/historical-income-households/h08.xlsx
# Table H-8. Median Household Income by State: 1984 to 2020	

hh_income = read.csv(file='extract-from-census-table-H08.csv',
                     skip=10, header=TRUE)

# 'crosswalk' for state geographic coding
geo_info = tribble(
  ~state_name, ~fips, ~abb,
                     'Alabama',    1,  'AL',
                      'Alaska',    2,  'AK',
              'American Samoa',   60,  'AS',
                     'Arizona',    4,  'AZ',
                    'Arkansas',    5,  'AR',
                  'California',    6,  'CA',
                    'Colorado',    8,  'CO',
                 'Connecticut',    9,  'CT',
                    'Delaware',   10,  'DE',
        'District of Columbia',   11,  'DC',
                     'Florida',   12,  'FL',
                     'Georgia',   13,  'GA',
                        'Guam',   66,  'GU',
                      'Hawaii',   15,  'HI',
                       'Idaho',   16,  'ID',
                    'Illinois',   17,  'IL',
                     'Indiana',   18,  'IN',
                        'Iowa',   19,  'IA',
                      'Kansas',   20,  'KS',
                    'Kentucky',   21,  'KY',
                   'Louisiana',   22,  'LA',
                       'Maine',   23,  'ME',
                    'Maryland',   24,  'MD',
               'Massachusetts',   25,  'MA',
                    'Michigan',   26,  'MI',
                   'Minnesota',   27,  'MN',
                 'Mississippi',   28,  'MS',
                    'Missouri',   29,  'MO',
                     'Montana',   30,  'MT',
                    'Nebraska',   31,  'NE',
                     'Nevada',   32,  'NV',
               'New Hampshire',   33,  'NH',
                  'New Jersey',   34,  'NJ',
                  'New Mexico',   35,  'NM',
                    'New York',   36,  'NY',
              'North Carolina',   37,  'NC',
                'North Dakota',   38,  'ND',
    'Northern Mariana Islands',   69,  'MP',
                        'Ohio',   39,  'OH',
                    'Oklahoma',   40,  'OK',
                      'Oregon',   41,  'OR',
                'Pennsylvania',   42,  'PA',
                 'Puerto Rico',   72,  'PR',
                'Rhode Island',   44,  'RI',
              'South Carolina',   45,  'SC',
                'South Dakota',   46,  'SD',
                   'Tennessee',   47,  'TN',
                       'Texas',   48,  'TX',
 'U.S. Minor Outlying Islands',   74,  'UM',
         'U.S. Virgin Islands',   78,  'VI',
                        'Utah',   49,  'UT',
                     'Vermont',   50,  'VT',
                    'Virginia',   51,  'VA',
                  'Washington',   53,  'WA',
               'West Virginia',   54,  'WV',
                   'Wisconsin',   55,  'WI',
                     'Wyoming',   56,  'WY',
)

# merge in geo info
hh_income = left_join(hh_income, geo_info)

# convert to a long dataframe, with one obs per state-year
income_info = hh_income %>% 
               pivot_longer(cols=starts_with('inc'), 
                            names_to='year', 
                            values_to = 'income') %>% 
               mutate(year = as.numeric(str_extract(year,'[:digit:]{4}')))

# read US mortality database info ----
# Life tables are in a big (50MB) zipped file from the USMDB 
# Extract the division-level, both-sex tables if necessary

need.to.build.df = !exists('state.df')

included_states = hh_income$abb   

if (need.to.build.df) {
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
  
  
} # if need.to.build

e0_info = state.df %>% 
           filter(Year %in% seq(1985,2015,10), Age==0) %>% 
           select(abb=PopName, year=Year, e0=ex)


# combine income and e0 data, drop DC, then plot ----

df = left_join(income_info, e0_info) %>% 
      filter(!(abb == 'DC'))
 
# calculate correlations by year
rho = df %>% 
        group_by(year) %>% 
        summarize(rho=round(cor(income, e0),2))

# scatterplots and correlations ----
ggplot(df) +
  aes(x=income, y=e0, color=factor(year),label=abb) +
  geom_point(size=6,alpha=.20) +
  geom_text(size=2.5, fontface='bold') +
  geom_text(data=rho,x=40000,y=79, 
            aes(label=paste('rho:',rho)), size=6, parse=TRUE) +
  geom_smooth(method='lm',se=FALSE) +
  facet_wrap(~year) +
  guides(color='none') +
  labs(x='State Median Household Income (in 2020 dollars)',
       y='Life Expectancy at Birth (e0), Both Sexes',
       caption=paste0('Calculations by @CSchmert\n',
                      'HH income data: US Census Bureau\n',
                      'Life Expectancy data: US Mortality Database')) +
  theme_bw() +
  theme(axis.text = element_text(face='bold', size=12),
        axis.title = element_text(face='bold', size=14),
        strip.text = element_text(face='bold', size=16))

ggsave(filename='hh-income-and-mortality.png', 
       height=10, width=10, dpi=300)
