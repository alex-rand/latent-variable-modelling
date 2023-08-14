library(tidyverse)
library(lavaan)
library(ggdag)

### Load the data
dat <- read_csv('data/ucla/hsbdemo.csv')

### Load the data again but in split format, for what is to come.
dat_split <- list(
  boys  = dat %>% filter(female == "female"),
  girls = dat %>% filter(female == "male")
)

### Define the basic CFA model
onefac <- 'f1  =~ read + write + math + science'

### Fit the model separately for each group
onefac_models <- list(
  onefac_boys  = cfa(onefac, data = dat_split$boys, meanstructure = TRUE),
  onefac_girls = cfa(onefac, data = dat_split$girls, meanstructure = TRUE) 
)

### Gaze at the parameter estimates
onefac_models %>% map(summary, standardized = TRUE, fit.measures = TRUE)

### Configural fit
configural.fit <- cfa(onefac, data = dat, group = "female", meanstructure = TRUE)

### Equal loadings
equal.loadings.fit <- cfa(onefac, data = dat, group = "female", 
                          group.equal = c("loadings"), meanstructure = TRUE) 

summary(equal.loadings.fit, standardized = TRUE, fit.measures = TRUE)


### Equal intercepts
equal.intercepts.fit <- cfa(onefac, data = dat, group = "female", 
                            group.equal = c("loadings","intercepts"), meanstructure = TRUE)

### Partial invariance
onefac <- 'f1  =~ read + write + math + science'

partial.invariance.fit <- cfa(
  onefac_partial_invariance, 
  dat, 
  group = "female", 
  group.equal = c("loadings", "intercepts"), 
  group.partial=c("read~1"), # This frees up the desired intercepts
  meanstructure = TRUE)


summary(partial.invariance.fit, standardized=TRUE)


anova(configural.fit, equal.loadings.fit, partial.invariance.fit)
lavTestScore(equal.intercepts.fit)
  
