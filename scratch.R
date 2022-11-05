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
  slice(1:12) %>% 
  
  # Clean up the important values, then combine them into a single column
  mutate(
    std.all = round(std.all, 2),
    std.all = paste0(std.all, ", pvalue = ", pvalue, ")")
  ) %>% 
  
  # reformat the table
  select(lhs, rhs, std.all) %>% 
  
  pivot_wider(
    names_from = "lhs", 
    values_from = "std.all",
    values_fill = "0"
  ) %>% 
  
  column_to_rownames("rhs") %>% 
  
  knitr::kable(caption = "Standardized factor loadings, standard errors, and p-values")







# Define the relationships from my hypothesis
h1_test.definition <- 
'map =~ ags1 + a1*ags5 + a2*ags7
mav=~ags2+ags6+ags12
pap=~ags3+ags9+ags11
pav=~ags4+ags8+ags10

a1 == a2
'

# Fit the model
h1_test.fit <- cfa(
  data  = dat_ags,
  model = h1_test.definition
)

# Look at the results
h1_test.summary <- summary(h1_test.fit, fit.measures = TRUE, standardized = TRUE)

h1_test.summary

anova(h1_test.fit, h1.fit)

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
