library(tidyverse)
library(lavaan)
library(ggdag)


  


dat <- read_csv('data/ucla/hsbdemo.csv')

dat_split <- list(
  boys  = read_csv('data/ucla/hsbdemo.csv') %>% filter(female == "female"),
  girls = read_csv('data/ucla/hsbdemo.csv') %>% filter(female == "male")
)

dat$girls

onefac <- 'f1  =~ read + write + math + science'

onefac_models <- list(
  onefac_boys  = cfa(onefac, data = dat$boys, meanstructure = TRUE),
  onefac_girls = cfa(onefac, data = dat$girls, meanstructure = TRUE) 
)

onefac_models %>% map(summary, standardized = TRUE, fit.measures = TRUE)

configural.fit <- cfa(onefac, data = dat, group = "female", meanstructure = TRUE)
summary()
cfa(onefac, data = femaledat, meanstructure = TRUE, fit.measures = TRUE) 

fit.configural <- cfa(onefac, data = hsbdemo, group = "female", meanstructure = TRUE)

summary(configural.fit, standardized=TRUE)

dat$boys

dat[[1]]
  
  

dat %>% 
  
  ggplot(aes(x = write, y = math)) +
  geom_point() +
  geom_smooth(formula = "lm")
  
  view()
