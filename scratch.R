library(tidyverse)
library(lavaan)
library(ggdag)


  


dat <- read_csv('data/ucla/hsbdemo.csv')

onefac <- 'f1  =~ read + write + math + science'




configural.fit <- cfa(onefac, data = dat, group = "female", meanstructure = TRUE)

equal.loadings.fit <- cfa(onefac, data = dat, group = "female", 
                          group.equal = c("loadings"), meanstructure = TRUE) 

equal.intercepts.fit <- cfa(onefac, data = dat, group = "female", 
                            group.equal = c("loadings","intercepts"), meanstructure = TRUE)

equal.residuals.fit <- cfa(onefac, data = dat, group = "female", 
                            group.equal = c("loadings","intercepts", "residuals"), meanstructure = TRUE)


modindices(equal.intercepts.fit, sort = TRUE) %>% 
  
  # Arrange them in order of modification index
  arrange(desc(mi)) %>% 
  
  select(lhs, op, rhs, mi)


lavTestScore(equal.intercepts.fit)

anova(configural.fit, equal.loadings.fit, equal.intercepts.fit)
summary(equal.intercepts.fit, standardized = TRUE, fit.measures = TRUE)
