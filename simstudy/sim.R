library(tidyverse)
library(ggtext)
library(here)
library(mirt)
library(cluster)
library(DescTools)
library(knitr)
library(kableExtra)
R.utils::sourceDirectory("R")
set.seed(999)
library(furrr)
options(future.fork.enable = TRUE)
plan(multiprocess)

# functions

sim_n_dif_items_to_foc_mean <- function(sim, n_dif_items){
    mod <-
        mod_flexible(
            sim$data,
            sim$groups,
            flex_items = (ncol(sim$data) - n_dif_items + 1):ncol(sim$data)
        )

    coef_1f(mod)$ability$b_foc[1]
}

add_foc_mean <- function(one, n_dif_items){
    one %>%
        mutate(foc_mean = sim %>% map_dbl(sim_n_dif_items_to_foc_mean, n_dif_items))
}

# run

out <-
    tibble(n_dif_items = 2:6) %>%
    mutate(
        bigsim = n_dif_items %>% future_map(~ bigsim(runs = 100, n_dif_items = .)) # future_map
    )

out <-
    out %>%
    mutate(bigsim = map2(bigsim, n_dif_items, add_foc_mean))

out %>% write_rds("simstudy/finalsim.rds")#
