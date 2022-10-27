library(tidyverse)

dat_raw <- foreign::read.spss('data/finch-and-french/edps744.sav') 
  
dat_ags <- dat_raw %>% 

  as.data.frame() %>% 
  
  select(matches("ags\\d"))  

get_prop_na <- function(x){
  
  res <- sum(is.na(x)) / (sum(is.na(x) + sum(!is.na(x))))
  
  res
  
}
  
  



dat_ags %>% 
  
  summarise_all(~ sum(is.na(.)) / (sum(is.na(.) + sum(!is.na(.))))) %>% 
  
  mutate(across(everything(), round, 6)) %>% 
  
  knitr::kable(title = "Proportion of Missing Responses in Each Column") 
  
  knitr::kable() 
  
  
  
 
  
  summarise_all(sum(is.na(.)))

mice::md.pattern(dat_ags)
  
  view(
  
  select(starts_with("ags")) %>% 
  
  view()

select(matches("ags\d")) 