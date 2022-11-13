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

dag_coords <- list(
  x = c(
    F1 = 1, 
    F2 = 1,
    H1 = 2,
    H2 = 2,
    H3 = 2,
    S1 = 2,
    S2 = 2,
    S3 = 2,
    M1 = 3,
    M2 = 3),
  y = c(
    F1 = 2.5,
    F2 = 1.5,
    H1 = 2.8,
    H2 = 2.5,
    H3 = 2.2,
    S1 = 1.8,
    S2 = 1.5,
    S3 = 1.2,
    M1 = 2.5,
    M2 = 1.5
  )
)

# Set DAG relationships and aesthetics
measurement_confounding_dag <- ggdag::dagify(
  H3 ~ F1,
  S2 ~ F2,
  H1 ~ M1,
  H2 ~ M1,
  H3 ~ M1,
  S1 ~ M2,
  S2 ~ M2,
  S3 ~ M2,
  coords = dag_coords
) %>% 
  
  tidy_dagitty() %>% 
  
  mutate(
    
    node_colour = case_when(
      grepl("^F|M", name) ~ "latent",
      grepl("^H|S", name) ~ "observed"
    ),
    
    edge_colour = case_when(
      grepl("M1", name)  ~ "cornflower blue",
      grepl("M2", name) ~ "#ed7864",
      grepl("^XX", name) & grepl("3$", to) ~ "#ed7864",
      grepl("^F", name)                   ~ "black"
    )
  )

# Plot the DAG
measurement_confounding_dag %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(aes(colour = node_colour)) +
  scale_colour_manual(values = c("dark blue", "#edbc64")) + 
  geom_dag_edges(aes(edge_colour = edge_colour)) +
  geom_dag_text() +
  theme_void()



### Simulate Data from the DAG

# Set seed for replicable results
set.seed(233)

# Set sample size
N <- 3000

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

lm(H1 ~ H2 + M1, dat_fake)


dumb.definition <- 
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


dumb.fit <- cfa(
  data = dat_fake,
  model = dumb.definition
)

summary(dumb.fit, standardized = TRUE)

correlated_uniqueness.fit <- cfa(
  data = dat_fake,
  model = correlated_uniqueness.definition
)

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
