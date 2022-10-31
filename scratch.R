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


h1.summary <- summary(h1.fit, fit.measures = TRUE, standardized = TRUE)


  
h1.summary$pe %>% 
  
  as_tibble() %>% 
 
  # Keep only the rows with info on factor loadings
  slice(25:34) %>% 
 
  # Clean up the important values, then combine them into a single column

  select(lhs, rhs, std.lv) %>% 
  
  mutate(
    std.lv = round(std.lv, 2),
    across(everything(), as.character)
  ) %>% 
  
  pivot_wider(
    names_from = "lhs", 
    values_from = "std.lv",
    values_fill = " " 
  ) %>% 
  
  column_to_rownames("rhs") %>% 

  knitr::kable(

  
  select()

tibble(
  Test             = "standard chi-squared",
  `DF`             = h1.summary$test$standard$df,
  `Test Statistic` = round(h1.summary$test$standard$stat, 2),
  `p-value`        = h1.summary$test$standard$pvalue
) %>% 
  
  mutate(across(everything(), as.character)) %>% 
  
  pivot_longer()
  
  

factor_loadings <- h1.summary 

h1.summary$pe %>% 
  
  slice(1:12) %>% 
  
  
  
  
factor

what$pe
h1.cfa()

achievement.goal.cfa4.model
