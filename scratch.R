library(tidyverse)
library(lavaan)

dat_raw <- read.csv('data/grace/SEM_09_2-Ex1_CFA_exercise_data.csv')

dat_clean <- dat_raw %>%  
  
  janitor::clean_names()
  

# Raw Counts
dat_clean %>% 
  
  pivot_longer(
    cols      = !matches("^c"),
    names_to  = "animal",
    values_to = "count"
  ) %>% 
  
  ggplot() + 
  geom_bar(aes(x = animal, y = count), stat = "identity") + 
  theme_bw() + 
  facet_wrap(~country)

# Proportions
dat_clean %>% 
  
  janitor::clean_names() %>% 
  
  pivot_longer(
    cols      = !matches("^c"),
    names_to  = "animal",
    values_to = "count"
  ) %>% 
  
  group_by(country) %>% 
  mutate(
    total = sum(count), 
    prop  = round(count / total, 2)
  ) %>% 

  ggplot() + 
  geom_bar(aes(x = animal, y = prop), stat = "identity") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~country)
  

# Corr
cor(dat_clean %>% select(-country))

dat_clean %>% 
  
  select(-country) %>% 
  
  scale()

h1.definition <- 
'diversity =~ mammals + birds + amphibians + reptiles + beetles + butterflies'

h1.fit <- cfa(
  data  = dat_clean %>% select(-country) %>% scale(),
  model = h1.definition
)


h1.summary <- summary(h1.fit, fit.measures = TRUE, standardized = TRUE)

h1.summary

modindices(h1.fit) %>% 
  
  # Arrange them in order of modification index
  arrange(desc(mi)) 
  
  select(lhs, ops, rhs, mi) %>% 
  
  knitr::kable(digits = 2)

# Get the estimated change in chi-squared for each fixed parameter
modindices(h1.fit, op = "~~") %>% 
  
  # Arrange them in order of modification index
  arrange(desc(mi)) %>% 
  
  knitr::kable()


h2.definition <- 
'diversity =~ mammals + birds + amphibians + 
              reptiles + beetles + butterflies
 
 birds ~~ beetles'


h2.fit <- cfa(
  data  = dat_clean %>% select(-country) %>% scale(),
  model = h2.definition
)

h2.summary <- summary(h2.fit, fit.measures = TRUE, standardized = TRUE)


h3.definition <- 
  'diversity =~ mammals + birds + amphibians + 
              reptiles + beetles + butterflies
 
 birds ~~ amphibians'


h3.fit <- cfa(
 data  = dat_clean %>% select(-country) %>% scale(),
  model = h3.definition
)

h3.summary <- summary(h3.fit, fit.measures = TRUE, standardized = TRUE)

h2.summary


