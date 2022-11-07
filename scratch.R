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



### Example of CFA from Stan forum
library(tidyverse)
library(brms)

options(brms.backend = "cmdstanr")
options(mc.cores = parallel::detectCores())

N <- 200

dta <- 
  tibble(
    x = rnorm(N, 0, 1),
    y1 = rnorm(N, 2*x, 1),
    y2 = rnorm(N, 1*x, 1),
    y3 = rnorm(N, 0.5*x, 1),
    xo = as.numeric(NA)
  )

dta

m1 <- 
  brm(
    formula =
      bf(y1 ~ 0 + mi(xo)) +
      bf(y2 ~ 0 + mi(xo)) +
      bf(y3 ~ 0 + mi(xo)) +
      bf(xo | mi() ~ 1) + 
      set_rescor(rescor = FALSE),
    family = gaussian(),
    prior =
      prior(constant(1), class = "b", resp = "y1") +
      prior(constant(1), class = "sigma", resp = "y1") +
      prior(normal(0, 10), class = "b", resp = "y2") +
      prior(constant(1), class = "sigma", resp = "y2") +
      prior(normal(0, 10), class = "b", resp = "y3") +
      prior(constant(1), class = "sigma", resp = "y3") +
      prior(normal(0, 10), class = "Intercept", resp = "xo") +
      prior(cauchy(0, 1), class = "sigma", resp = "xo"),
    data = dta,
    backend = "cmdstanr",
    cores = 4,
    chains = 4,
    threads = threading(2),
    refresh = 5
  )

h1.definition <- 
  'what=~y1+y2+y3'

h1.fit <- cfa(
  data  = dta,
  model = h1.definition
)

h1.summary <- summary(h1.fit, fit.measures = TRUE, standardized = TRUE)

h1.summary



summary(m1)






