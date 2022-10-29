library(tidyverse)
library(lavaan)

dat_raw <- foreign::read.spss('data/finch-and-french/edps744.sav') 
  
dat_ags <- dat_raw %>% 

  as.data.frame() %>% 
  
  select(matches("ags\\d"))  

h1.definition <- 
'map=~ags1+ags5+ags7
mav=~ags2+ags6+ags12
pap=~ags3+ags9+ags11
pav=~ags4+ags8+ags10'

h1.fit <- cfa(
  data  = dat_ags,
  model = h1.definition
)

?semp

semPlot::semPaths(h1.fit)
h1.summary <- summary(h1.fit)

factor_loadings <- h1.summary 

h1.summary$pe %>% 
  
  slice(1:12) %>% 
  
  
  
  
factor

what$pe
h1.cfa()

achievement.goal.cfa4.model
