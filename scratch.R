library(tidyverse)

dat_raw <- foreign::read.spss('data/finch-and-french/edps744.sav') 
  
dat_ags <- dat_raw %>% 

  as.data.frame() %>% 
  
  select(matches("ags\\d"))  



mardia.object <- psych::mardia(dat_ags)

# Plot the multivariate version of the normal probability plot:
plot(mardia.object)

# Present the outputs we're interested in.
tibble(
  "Skew" = mardia.object$skew,
  "Skew p-value" = mardia.object$p.skew,
  "Kurtosis" = mardia.object$kurtosis,
  "Kurtosis p-value" = mardia.object$p.kurt
) %>% 
  
  knitr::kable()

mardia.object$p.skew
mardia.object$p.kurt
psych::describe(dat_ags)

qqnorm(dat_ags$ags1, 
       ylab = "Birth Weight (in grams)")


qqline(birthwt$bwt)

dat_ags %>% 
  
  # Calculate the proportion of missing values 
  summarise_all(~ sum(is.na(.)) / nrow(.)) 
  
  mutate(across(everything(), round, 6)) %>% 
  
  knitr::kable(title = "Proportion of Missing Responses in Each Column") 
