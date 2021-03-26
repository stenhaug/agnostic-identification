library(tidyverse)
library(difR)
data("verbal")

verbal %>%
	as_tibble() %>%
	janitor::clean_names() %>%
	select(-anger) %>%
	select(gender, everything()) %>%
	mutate(gender = ifelse(gender == 1, "male", "female")) %>%
	write_rds("data/verbal.rds")
