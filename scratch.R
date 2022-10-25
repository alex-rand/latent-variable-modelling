library(tidyverse)

dat_ff <- foreign::read.spss('data/finch-and-french/performance.data.sav')

dat_ff <- psych::char2numeric(dat_ff) 

psych::mardia(what)

psych::fa.parallel(what, fa="fa", fm="ml")

what <- dat_ff %>% 
  
  as_tibble() %>% 
  
  select(1:12)

what %>% view()
  
dat %>% view()

?char2numeric
