
# Take 2 ------------------------------------------------------------------

lines2 <-
    tibble(
        Method = c("AOAA & AOAA-AS", "AOAA-OAT", "MAXGI", "MINBC"),
        Mean =
            c(
                anchor_items$AOAA_AS_mod[[1]] %>% coef_1f() %>% .$ability %>% pull(b_foc) %>% .[1],
                anchor_items$AOAA_OAT_mod[[1]] %>% coef_1f() %>% .$ability %>% pull(b_foc) %>% .[1],
                -gini_results$gini_anchor_points,
                min_bc$between_curves_anchor_points
            )
    ) %>%
    mutate(Method = factor(Method, levels = c("AOAA-OAT", "MAXGI", "AOAA & AOAA-AS", "MINBC")))

lines2 %>% write_rds("07-lines2.rds")

read_rds("07-lines2.rds")

anchor_items$intuitive_mod[[1]] %>%
    coef_1f() %>%
    .$items %>%
    mutate(diff =  b_foc_easy - a_ref_easy) %>%
    left_join(item_map) %>%
    ggplot(aes(x = diff, y = full)) +
    geom_point() +
    geom_vline(
        data = lines2,
        aes(xintercept = Mean, linetype = Method, color = Method),
        size = 0.7
    ) +
    theme_minimal(base_size = 10) +
    theme(legend.position="bottom", legend.justification = c(0, 2)) +
    labs(
        x = latex2exp::TeX("$\\tilde{d_j} = \\tilde{b_j}^{male} - \\tilde{b_j}^{female}$"),
        y = "",
        color = "",
        linetype = ""
    )

# Take 1 ------------------------------------------------------------------
bind_rows(
    anchor_items$intuitive_mod[[1]] %>%
        mod_intuitive_to_draws_df() %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        left_join(anchor_items$AOAA_final_dif[[1]] %>% mutate(item = parse_number(item))) %>%
        mutate(val = -val) %>%
        mutate(method = "AOAA & AOAA-AS"),

    anchor_items$intuitive_mod[[1]] %>%
        mod_intuitive_to_draws_df() %>%
        select(-run) %>%
        gather(var, val) %>%
        mutate(item = parse_number(var)) %>%
        left_join(anchor_items$AOAA_OAT_final_dif[[1]] %>% mutate(item = parse_number(item))) %>%
        mutate(val = -val) %>%
        mutate(method = "AOAA-OAT")
) %>%
    left_join(item_map) %>%
    ggplot(aes(x = val, y = full)) +
    ggridges::geom_density_ridges(aes(fill = anchor)) +
    geom_vline(
        data = lines,
        aes(xintercept = val)
    ) +
    theme(legend.position = "bottom") +
    labs(
        x = latex2exp::TeX("$\\tilde{b_j}^{male} - \\tilde{b_j}^{female}$"),
        y = ""
    ) +
    facet_wrap(~ method, ncol = 1) +
    coord_cartesian(xlim = c(-2, 2))

lines <-
    tibble(
        method = c("AOAA & AOAA-AS", "AOAA-OAT", "MAXGI", "MINBC"),
        val = c(0.197, 0.01, 0.155, 0.27)
    )
