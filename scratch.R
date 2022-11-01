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

h1.summary

  
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
  
  column_to_rownames("rhs") 


# Define the relationships from my hypothesis
h1_orthogonal.definition <- 
  'map=~ags1+ags5+ags7
mav=~ags2+ags6+ags12
pap=~ags3+ags9+ags11
pav=~ags4+ags8+ags10

map ~~ 1*mav
pap ~~ 1*pav
'

# Fit the model
h1_orthogonal.fit <- cfa(
  data  = dat_ags,
  model = h1_orthogonal.definition
)

# Look at the results
h1_orthogonal.summary <- summary(h1_orthogonal.fit, fit.measures = TRUE, standardized = TRUE)

h1_orthogonal.summary

h1_parsimonious.definition <- 
'mastery=~ags1+ags5+ags7+ags2+ags6+ags12
 performance=~ags3+ags9+ags11+ags4+ags8+ags10
'

# Fit the model
h1_parsimonious.fit <- cfa(
  data  = dat_ags,
  model = h1_parsimonious.definition
)

# Look at the results
h1_parsimonious.summary <- summary(h1_parsimonious.fit, fit.measures = TRUE, standardized = TRUE)
h1_parsimonious.summary
h1.summary


anova(h1.fit, h1_orthogonal.fit) %>% 
  
  knitr::kable()
anova(h1_parsimonious.fit, h1.fit)

summary(h1_parsimonious.fit, fit.measures = TRUE)



anova(h1.fit)
