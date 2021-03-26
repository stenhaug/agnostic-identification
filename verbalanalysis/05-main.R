library(tidyverse)
library(mirt)
R.utils::sourceDirectory("../anchor/R")

item_map <- read_rds("02-itemmap.rds")
anchor_items <- read_rds("03-anchor-items.rds")

# just the plain EM MILG
anchor_items$intuitive_mod[[1]] %>%
    mod_intuitive_to_draws_df() %>%
    select(-run) %>%
    gather(var, val) %>%
    mutate(item = parse_number(var)) %>%
    left_join(item_map %>% select(item, full)) %>%
    mutate(val = -val) %>%
    ggplot(aes(x = val, y = full)) +
    ggridges::geom_density_ridges() +
    labs(
        x = latex2exp::TeX("$\\tilde{b_j}^{male} - \\tilde{b_j}^{female}$")
    )

# EM MILG with shading from AOAA
anchor_items$intuitive_mod[[1]] %>%
    mod_intuitive_to_draws_df() %>%
    select(-run) %>%
    gather(var, val) %>%
    mutate(item = parse_number(var)) %>%
    left_join(item_map %>% select(item, full)) %>%
    left_join(anchor_items$AOAA_status[[1]]) %>%
    mutate(val = -val) %>%
    ggplot(aes(x = val, y = full, fill = status)) +
    ggridges::geom_density_ridges() +
    labs(
        x = latex2exp::TeX("$\\tilde{b_j}^{male} - \\tilde{b_j}^{female}$")
    )


anchor_items$AOAA_mod[[1]] %>% mod_to_draws_df(n = 10000)
anchor_items$AOAA_final_dif

anchor_items$AOAA_final_dif[[1]] %>%
    mutate(difference_in_easy = difference_in_easy - difference_in_easy[anchor][1]) %>%

    mutate(item = parse_number(item)) %>%
    left_join(item_map %>% select(item, full)) %>%

    ggplot(aes(x = difference_in_easy, y = full)) +
    ggridges::geom_density_ridges(
        data =
            anchor_items$AOAA_mod[[1]] %>%
            mod_to_draws_df(n = 1000) %>%
            select(-run) %>%
            gather(var, val) %>%
            mutate(item = parse_number(var)) %>%
            left_join(item_map %>% select(item, full)) %>%
            filter(val != 0) %>%
            mutate(val = -val),
        aes(x = val, y = full), scale = 0.3
    ) +
    geom_point(
        data =
            anchor_items$AOAA_final_dif[[1]] %>%
            mutate(difference_in_easy = difference_in_easy - difference_in_easy[anchor][1]) %>%
            mutate(item = parse_number(item)) %>%
            left_join(item_map %>% select(item, full)) %>%
            filter(a_ref_easy == b_foc_easy)
    ) +
    labs(
        x = latex2exp::TeX("$\\hat{d_j} = \\hat{b_j}^{ref} - \\hat{b_j}^{foc}$"),
        y = ""
    )

