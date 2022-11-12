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


# Set DAG coordinates
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
    M2 = 3,
    M3 = 3),
  y = c(
    F1 = 2.5,
    F2 = 1.5,
    H1 = 2.8,
    H2 = 2.5,
    H3 = 2.2,
    S1 = 1.8,
    S2 = 1.5,
    S3 = 1.2,
    M1 = 2.6,
    M2 = 2,
    M3 = 1.4
  )
)

# Set DAG relationships and aesthetics
measurement_confounding_dag <- ggdag::dagify(
  H1 ~ F1,
  H2 ~ F1,
  H3 ~ F1,
  S1 ~ F2,
  S2 ~ F2,
  S3 ~ F2,
  H1 ~ M1,
  S1 ~ M1,
  H2 ~ M2,
  S2 ~ M2,
  H3 ~ M3,
  S3 ~ M3,
  coords = dag_coords
) %>% 
  
  tidy_dagitty() %>% 
  
  mutate(
    
    node_colour = case_when(
      grepl("^F|M", name) ~ "latent",
      grepl("^H|S", name) ~ "observed"
    ),
    
    edge_colour = case_when(
      grepl("^M", name) & grepl("1$", to) ~ "cornflower blue",
      grepl("^M", name) & grepl("2$", to) ~ "#daed64",
      grepl("^M", name) & grepl("3$", to) ~ "#ed7864",
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
  theme()
  theme_void()

summary(h1.fit, standardized = TRUE)


population.model <- ' happy =~ H1 + 0.8*H2 + 1.2*H3
                      sad =~ S1 + 0.8*S2 + 1.2*S3
                      M1 =~ H1 + 0.3*S1
                      M2 =~ H2 + 0.3*S2
                      M3 =~ H3 + 0.3*S3
                    '

# generate data
set.seed(1234)
dat_sim <- simulateData(population.model, sample.nobs=100L)


dumb.definition <- 
  'happy =~ H1 + H2 + H3
   sad =~ S1 + S2 + S3
   '

correlated_uniqueness.definition <- 
  'happy =~ H1 + H2 + H3
   sad =~ S1 + S2 + S3
   
   H1 ~~ S1
   H2 ~~ S2
   H3 ~~ S3
   '


dumb.fit <- cfa(
  data = dat_sim,
  model = dumb.definition
)

correlated_uniqueness.fit <- cfa(
  data = dat_sim,
  model = correlated_uniqueness.definition
)

uncorrelated_methods.fit <- cfa(
  data = dat_sim,
  model = uncorrelated_methods.definition
)


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
