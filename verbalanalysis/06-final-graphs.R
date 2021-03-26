# AOAA --------------------------------------------------------------------

bind_rows(
    anchor_items$intuitive_mod[[1]] %>%
        mod_intuitive_to_draws_df() %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        left_join(anchor_items$AOAA_status[[1]]) %>%
        mutate(val = -val) %>%
        mutate(what = "EM-MILG") %>%
        mutate(
            status = case_when(status == "anchor" ~ "Final Anchor Item", status == "flex" ~ "Final DIF Item")
        ),
    anchor_items$AOAA_mod[[1]] %>%
        mod_to_draws_df(n = 1000) %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        filter(val != 0) %>%
        mutate(val = -val) %>%
        mutate(status = "Final DIF Item") %>%
        mutate(what = "Final Model")
) %>%
    left_join(item_map %>% select(item, full)) %>%
    ggplot(aes(x = val, y = full)) +
    ggridges::geom_density_ridges(aes(fill = status)) +
    labs(
        y = "",
        x = latex2exp::TeX("$\\tilde{d_j} = \\tilde{b_j}^{male} - \\tilde{b_j}^{female} \\;\\;\\;\\;\\;\\;\\;\\; \\hat{d_j} = \\hat{b_j}^{male} - \\hat{b_j}^{female}$")
    ) +
    facet_wrap(~ what) +
    geom_point(
        data =
            anchor_items$AOAA_final_dif[[1]] %>%
            mutate(difference_in_easy = difference_in_easy - difference_in_easy[anchor][1]) %>%
            mutate(item = parse_number(item)) %>%
            left_join(item_map %>% select(item, full)) %>%
            filter(a_ref_easy == b_foc_easy) %>%
            mutate(status = "Final Anchor Item") %>%
            mutate(what = "Final Model"),
        aes(x = difference_in_easy, y = full, color = status)
    ) +
    guides(color = FALSE) +
    scale_fill_manual(values =
                          c("Final Anchor Item" = "#34bb99ff",
                            "Final DIF Item" = "red")) +
    scale_color_manual(values =
                          c("Final Anchor Item" = "#34bb99ff",
                            "Final DIF Item" = "red")) +
    labs(fill = "")

# AOAA-OAT TRY 1 THIS FAILED --------------------------------------------------------------------
bind_rows(
    anchor_items$intuitive_mod[[1]] %>%
        mod_intuitive_to_draws_df() %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        left_join(anchor_items$AOAA_OAT_status[[1]]) %>%
        mutate(val = -val) %>%
        mutate(what = "Iteration 1") %>%
        mutate(
            status = case_when(order > 1 | is.na(order) ~ "Anchor Item", order <= 1 ~ "Contains DIF")
        ),

    anchor_items$intuitive_mod[[1]] %>%
        mod_intuitive_to_draws_df() %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        left_join(anchor_items$AOAA_OAT_status[[1]]) %>%
        mutate(val = -val) %>%
        mutate(what = "Iteration 2") %>%
        mutate(
            status = case_when(order > 2 | is.na(order) ~ "Anchor Item", order <= 2 ~ "Contains DIF")
        ),

    anchor_items$intuitive_mod[[1]] %>%
        mod_intuitive_to_draws_df() %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        left_join(anchor_items$AOAA_OAT_status[[1]]) %>%
        mutate(val = -val) %>%
        mutate(what = "Iteration 3") %>%
        mutate(
            status = case_when(order > 3 | is.na(order) ~ "Anchor Item", order <= 3 ~ "Contains DIF")
        ),

    anchor_items$intuitive_mod[[1]] %>%
        mod_intuitive_to_draws_df() %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        left_join(anchor_items$AOAA_OAT_status[[1]]) %>%
        mutate(val = -val) %>%
        mutate(what = "Iteration 4") %>%
        mutate(
            status = case_when(order > 4 | is.na(order) ~ "Anchor Item", order <= 4 ~ "Contains DIF")
        ),

    anchor_items$intuitive_mod[[1]] %>%
        mod_intuitive_to_draws_df() %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        left_join(anchor_items$AOAA_OAT_status[[1]]) %>%
        mutate(val = -val) %>%
        mutate(what = "Iteration 5") %>%
        mutate(
            status = case_when(order > 5 | is.na(order) ~ "Anchor Item", order <= 5 ~ "Contains DIF")
        ),

    anchor_items$intuitive_mod[[1]] %>%
        mod_intuitive_to_draws_df() %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        left_join(anchor_items$AOAA_OAT_status[[1]]) %>%
        mutate(val = -val) %>%
        mutate(what = "Iteration 6") %>%
        mutate(
            status = case_when(order > 6 | is.na(order) ~ "Anchor Item", order <= 6 ~ "Contains DIF")
        ),

    anchor_items$intuitive_mod[[1]] %>%
        mod_intuitive_to_draws_df() %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        left_join(anchor_items$AOAA_OAT_status[[1]]) %>%
        mutate(val = -val) %>%
        mutate(what = "Iteration 7") %>%
        mutate(
            status = case_when(order > 7 | is.na(order) ~ "Anchor Item", order <= 7 ~ "Contains DIF")
        ),

    anchor_items$intuitive_mod[[1]] %>%
        mod_intuitive_to_draws_df() %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        left_join(anchor_items$AOAA_OAT_status[[1]]) %>%
        mutate(val = -val) %>%
        mutate(what = "Iteration 8") %>%
        mutate(
            status = case_when(order > 8 | is.na(order) ~ "Anchor Item", order <= 8 ~ "Contains DIF")
        ),

    anchor_items$intuitive_mod[[1]] %>%
        mod_intuitive_to_draws_df() %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        left_join(anchor_items$AOAA_OAT_status[[1]]) %>%
        mutate(val = -val) %>%
        mutate(what = "Iteration 9") %>%
        mutate(
            status = case_when(order > 9 | is.na(order) ~ "Anchor Item", order <= 9 ~ "Contains DIF")
        ),

    anchor_items$AOAA_OAT_mod[[1]] %>%
        mod_to_draws_df(n = 1000) %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        filter(val != 0) %>%
        mutate(val = -val) %>%
        mutate(status = "Contains DIF") %>%
        mutate(what = "Final Model")
) %>%
    left_join(item_map %>% select(item, full)) %>%
    ggplot(aes(x = val, y = full)) +
    ggridges::geom_density_ridges(aes(fill = status)) +
    labs(
        y = "",
        x = latex2exp::TeX("$\\tilde{d_j} = \\tilde{b_j}^{male} - \\tilde{b_j}^{female} \\;\\;\\;\\;\\;\\;\\;\\;\\; \\hat{d_j} = \\hat{b_j}^{male} - \\hat{b_j}^{female}$")
    ) +
    facet_wrap(~ what)

# TRY 2 THIS WILL WORK ----------------------------------------------------

bind_rows(
    anchor_items$intuitive_mod[[1]] %>%
        mod_intuitive_to_draws_df() %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        left_join(anchor_items$AOAA_OAT_status[[1]]) %>%
        mutate(val = -val) %>%
        mutate(
            order = as.character(order),
            order = ifelse(is.na(order), "Final Anchor Item", order),
            order = case_when(
                order == "1" ~ "1st",
                order == "2" ~ "2nd",
                order == "3" ~ "3rd",
                order == "4" ~ "4th",
                order == "5" ~ "5th",
                order == "6" ~ "6th",
                order == "7" ~ "7th",
                order == "8" ~ "8th",
                order == "9" ~ "9th",
                TRUE ~ order
            ),
            what = "EM-MILG"
        ),

    anchor_items$AOAA_OAT_mod[[1]] %>%
        mod_to_draws_df(n = 1000) %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        filter(val != 0) %>%
        mutate(val = -val) %>%
        mutate(order = "Final DIF Item") %>%
        mutate(what = "Final Model")
) %>%
    left_join(item_map %>% select(item, full)) %>%
    ggplot(aes(x = val, y = full)) +
    ggridges::geom_density_ridges(aes(fill = order)) +
    labs(
        y = "",
        x = latex2exp::TeX("$\\tilde{d_j} = \\tilde{b_j}^{male} - \\tilde{b_j}^{female} \\;\\;\\;\\;\\;\\; \\hat{d_j} = \\hat{b_j}^{male} - \\hat{b_j}^{female}$")
    ) +
    facet_wrap(~ what) +
    scale_fill_manual(values =
                          c("1st" = "#a32e2eff",
                            "2nd" = "#cf2a2aff",
                            "3rd" = "#d31c1cff",
                            "4th" = "#bb347cff",
                            "5th" = "#bb34a1ff",
                            "6th" = "#a834bbff",
                            "7th" = "#8434bbff",
                            "8th" = "#7634bbff",
                            "9th" = "#6934bbff",
                            "Final Anchor Item" = "#34bb99ff",
                            "Final DIF Item" = "red")) +
    geom_point(
        data =
            anchor_items$AOAA_OAT_final_dif[[1]] %>%
            mutate(difference_in_easy = difference_in_easy - difference_in_easy[anchor][1]) %>%
            mutate(item = parse_number(item)) %>%
            left_join(item_map %>% select(item, full)) %>%
            filter(a_ref_easy == b_foc_easy) %>%
            mutate(order = "Final Anchor Item") %>%
            mutate(what = "Final Model"),
        aes(x = difference_in_easy, y = full, color = order)
    ) +
    scale_color_manual(values = c("Final Anchor Item" = "#34bb99ff")) +
    guides(color = FALSE) +
    labs(fill = "Removed From Anchor Set")

# EMC ---------------------------------------------------------------------

anchor_items$cluster_stats

anchor_items$cluster_final_dif[[1]] %>% print(n = 30)

