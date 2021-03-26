library(tidyverse)
library(here)

verbal <- read_rds("data/verbal.rds")
verbal_summarized_p_values <- read_rds("01-verbal_summarized_p_values.rds")

verbal_summarized_p_values %>%
    select(label) %>%
    separate(label, c("Situation", "Type", ))

add_zero <- function(x){
    ifelse(nchar(x) == 1, paste0("0", x), x)
}

item_map <-
    tibble(
        item = names(verbal)[-1]
    ) %>%
    left_join(
        verbal_summarized_p_values %>%
            select(item, label, l_rank) %>%
            slice(c(1:2, 4, 3, 5:24)) %>%
            mutate(l_rank = add_zero(row_number())) %>%
            mutate(LABEL = paste0(l_rank, ". ", label))
    ) %>%
    rename(original = item, text = label, log_rank = l_rank, full = LABEL) %>%
    mutate(item = row_number()) %>%
    select(item, full, everything())

item_map %>% write_rds("02-itemmap.rds")










anchor_items <- read_rds(here("03-anchor-items.rds"))
item_map <- read_rds(here("02-itemmap.rds"))

# just the plain EM MILG
anchor_items$intuitive_mod[[1]] %>%
    mod_intuitive_to_draws_df() %>%
    select(-run) %>%
    gather(var, val) %>%
    mutate(item_num = parse_number(var)) %>%
    group_by(item_num) %>%
    summarize(med = median(-val)) %>%
    arrange(med) %>%
    left_join(verbal_summarized_p_values %>% select(item_num, label, original = item, var)) %>%
    mutate(r = add_zero(row_number())) %>%
    mutate(full = paste0(r, ". ", label)) %>%
    write_rds("02labels.rds")
