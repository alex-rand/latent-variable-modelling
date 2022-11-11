library(tidyverse)
library(lavaan)
library(readxl)

dat_raw <- read.csv('data/grace/SEM_09_2-Ex1_CFA_exercise_data.csv')

dat_clean <- dat_raw %>%  
  
  janitor::clean_names()
  

h1.definition <- 
'diversity =~ mammals + birds + amphibians + reptiles + beetles + butterflies'

h1.fit <- cfa(
  data  = dat_clean %>% select(-country) %>% scale(),
  model = h1.definition
)

h1.summary <- summary(h1.fit, fit.measures = TRUE, standardized = TRUE)

h1.summary


library(readxl)

brown_cov <- readxl::read_excel("data/brown/figure-6.3.xlsx") %>% 
  
  column_to_rownames("...1") 

h1.definition <- 
  'happy =~ HACL + HAD + HDESC + HLKRT 
   sad   =~ SACL + SAD + SDESC + SLKRT'

h1.fit <- cfa(
  sample.cov = brown_cov,
  sample.nobs = 304,
  model = h1.definition
)


### Simulate based on a DAG

# Sample size
N <- 400

dat_fake <- tibble(
  
  # The factors are uncorrelated in reality, but
  # will be confounded by the measurement effects!
  F1 = rnorm(N, 0, 1),
  F2 = rnorm(N, 0, 1),
  
  # The measurement effects
  M1 = rnorm(N, 0, 1),
  M2 = rnorm(N, 0, 1),
  M3 = rnorm(N, 0, 1),
  M4 = rnorm(N, 0, 1),
  
  
  # The measurements
  H1 = .8*F1 + M1,
  H2 = .7*F1 + M2,
  H3 = .9*F1 + M3,
  H4 = .7*F1 + M4,
  S1 = .8*F2 + M1,
  S2 = .7*F2 + M2,
  S3 = .9*F2 + M3,
  S4 = .7*F2 + M4
)

voc()

h1.definition <- 
  'happy =~ H1 + H2 + H3 + H4
   sad =~ S1 + S2 + S3 + S4'

h1.fit <- cfa(
  data = dat_fake,
  model = h1.definition
)

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
