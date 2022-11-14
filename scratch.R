library(tidyverse)
library(lavaan)
library(ggdag)


  

h1.definition <- 
'diversity =~ mammals + birds + amphibians + reptiles + beetles + butterflies'

h1.fit <- cfa(
  data  = dat_clean %>% select(-country) %>% scale(),
  model = h1.definition
)

h1.summary <- summary(h1.fit, fit.measures = TRUE, standardized = TRUE)

h1.summary





### Simulate Data from the DAG

BROKEN BECAUSE OF WHAT THIS GUY SAYS: https://stackoverflow.com/questions/44114501/model-identification-in-lavaan-for-r
Try simulating the method effects explicitly as factors instead

# Set seed for replicable results
set.seed(233)

# Set sample size
N <- 30000

# Create the dataset
dat_fake <- tibble(
  
  # The factors are uncorrelated in reality, but
  # will be confounded by the measurement effects!
  F1 = rnorm(N, 0, 1),
  F2 = rnorm(N, 0, 1),
  
  # The measurement effects
  M1 = rnorm(N, 0, 1),
  M2 = rnorm(N, 0, 1),
  
  # The DAG says the measurements are fully determined by the latent factors and measurement effects
  H1 = 0.7*M1 + rnorm(N, 0, .3),
  H2 = 0.8*M1 + rnorm(N, 0, .3),
  H3 = 0.9*F1 + 0.8*M1 + rnorm(N, 0, .3),
  S1 = 0.7*M2 + rnorm(N, 0, .3),
  S2 = 0.7*F2 + 0.8*M2 + rnorm(N, 0, .3),
  S3 = 0.7*M2 + rnorm(N, 0, .3) 
) 

cor(dat_fake)


basic.definition <- 
  'happy =~ H1 + H2 + H3
   sad =~ S1 + S2 + S3
   '

correlated_uniqueness.definition <- 
  'happy =~ H1 + H2 + H3
   sad   =~ S1 + S2 + S3
   
   H1 ~~ H2
   H1 ~~ H3
   H2 ~~ H3
   S1 ~~ S2
   S1 ~~ S3
   S2 ~~ S3
   '

correlated_methods.definition <- 
  'happy =~ H1 + H2 + H3
   sad   =~ S1 + S2 + S3
   Method1    =~ H1 + S1 
   '


basic.fit <- cfa(
  data = dat_fake,
  model = basic.definition
)

correlated_uniqueness.fit <- cfa(
  data = dat_fake,
  model = correlated_uniqueness.definition
)

correlated_methods.fit <- cfa(
  data = dat_fake,
  model = correlated_methods.definition
)


summary(dumb.fit, standardized = TRUE)

summary(correlated_uniqueness.fit, standardized = TRUE)

summary(dumb.fit, standardized = TRUE)
summary(correlated_uniqueness.fit, standardized = TRUE)


summary(uncorrelated_methods.fit, standardized = TRUE)


correlated_uniqueness.definition
  
  view()
dat_fake

dat_fake %>% 
  
  select(H1, S1) %>% 
  
  view()


cov(dat_fake %>% select(matches("^(H|S)")))

cor(dat_fake %>% select(matches("^F")))


lm(H1 ~ S1 )
lm(H1 ~ S1)


cov




  
  view()
?read_xlsx
brown %>% view()
