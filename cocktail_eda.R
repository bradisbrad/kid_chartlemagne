## Setup ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidyquant, tidytext, lubridate, odbc, rebus, gghighlight, DataExplorer, devtools, skimr, magrittr,rvest, tidygraph, ggraph, colorspace)
devtools::install_github('bradisbrad/olfatbones'); library(olfatbones)
replace_fractions <- function(col){
  col <- str_replace(col, ' ?1/32', '.03125')
  col <- str_replace(col, ' ?1/8', '.125')
  col <- str_replace(col, ' ?1/6', '.1667')
  col <- str_replace(col, ' ?1/5', '.2')
  col <- str_replace(col, ' ?1/4', '.25')
  col <- str_replace(col, ' ?1/3', '.333')
  col <- str_replace(col, ' ?3/8', '.375')
  col <- str_replace(col, ' ?1/2', '.5')
  col <- str_replace(col, ' ?5/8', '.625')
  col <- str_replace(col, ' ?2/3', '.666')
  col <- str_replace(col, ' ?3/4', '.75')
  col <- str_replace(col, ' ?7/8', '.875')
  col
}

## Load in data ----
cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')
conv_tbl <- read_html('https://en.wikibooks.org/wiki/Bartending/Glossary/Table_of_measures_and_conversions') %>% 
  html_table() %>% 
  `[[`(1) %>% 
  as_tibble() %>% 
  transmute(term = str_extract(Term, '^[0-9] [A-z]+'),
            term = str_extract(term, '[A-z]+'),
            oz = str_remove_all(`Measurement (US)`, '[A-z]'),
            oz = str_remove_all(oz, '\\(.*\\)'),
            oz = replace_fractions(oz),
            oz = str_squish(oz),
            oz = str_remove(oz, ' \\.$'),
            oz = as.numeric(oz)) %>% 
  add_row(term = c('ounce', 'oz', 'tsp', 'tblsp'), oz = c(1, 1, 0.167, 0.5)) %>% 
  drop_na()


## Clean cocktail data ----
# cocktails <- 
cocktails %>% 
  mutate_if(is.character,tolower) %>% 
  transmute(row_id, name = drink, alcoholic = as.factor(alcoholic), 
            category = as.factor(category), glass = as.factor(glass), 
            ingredient_number, ingredient, measure) %>% 
  separate(measure, c('amt', 'unit'), sep = '(?<=[0-9])\\s(?=[A-z])') %>% 
  mutate(amt = str_replace(amt, ' ?1/8', '.125'),
         amt = str_replace(amt, ' ?1/4', '.25'),
         amt = str_replace(amt, ' ?1/3', '.333'),
         amt = str_replace(amt, ' ?3/8', '.375'),
         amt = str_replace(amt, ' ?1/2', '.5'),
         amt = str_replace(amt, ' ?5/8', '.625'),
         amt = str_replace(amt, ' ?2/3', '.666'),
         amt = str_replace(amt, ' ?3/4', '.75'),
         amt = str_replace(amt, ' ?7/8', '.875'),
         unit = str_extract(unit, str_c(conv_tbl$term, collapse = '|'))) %>% 
  left_join(conv_tbl, by = c('unit' = 'term')) %>% 
  rename(oz_conv = oz) %>% 
  mutate(amt = ifelse(str_detect(amt, '-'), str_sub(amt, 1, str_locate(amt, '-')-1), amt)) %>% 
  select(name, ingredient, amt, unit, oz_conv) %>% 
  filter(ingredient == 'salt')




  mutate(amt = )
boston_cocktails <- boston_cocktails %>%
  mutate_if(is.character,tolower) %>% 
  mutate(category = as.factor(category))


## Clean Mr. Boston data ----



## Categorize main alcohol based on ingredients



## Categorize category based on ingredients

