# pars <- create_pars_noncompensatory(n_items = 10, n_dif_items = 5, angle_start = 30, angle_end = 60, b_nuisance_with_dif_sd = 0)
#
# create_pars_noncompensatory(n_items = 10, n_dif_items = 5, angle_start = 30, angle_end = 60, b_nuisance_with_dif_sd = 0) %>%
#     graph_pars_noncompensatory(0, -1)

angle_to_a_nuisance <- function(angle){
    # uses that cos formula. assumes a_target is 1. see p 949 of walker and sahin
    # angle_to_a_nuisance(30)
    sqrt((1 - cos(angle * pi / 180)^2) / cos(angle * pi / 180)^2)
}

create_pars_noncompensatory <- function(n_items, n_dif_items, angle_start, angle_end){
    # create item parameters. all target disc is 1 and nuisance disc is 0 for non-dif items and has angle that
    # is lattice from 30 to 60 for dif items

    tibble(
        item = 1:n_items,
        dif = c(rep("no", n_items - n_dif_items), rep("yes", n_dif_items))
    ) %>%
        mutate(
            a_target = 1,
            angle = c(rep(0, n_items - n_dif_items), seq(angle_start, angle_end, length.out = n_dif_items)),
            a_nuisance = angle_to_a_nuisance(angle),
            b_target = c(rep(0, n_items - n_dif_items), rep(2, n_dif_items)),
            b_nuisance = c(rep(10000, n_items - n_dif_items), rep(2, n_dif_items))
        )
}

add_zero <- function(x){ifelse(nchar(x) == 1, paste0("0", x), x)}

graph_pars_noncompensatory <- function(pars, ref_mean_ability_nuisance, foc_mean_ability_nuisance){
    # graphs
    crossing(
        item = 1:nrow(pars),
        ability_target = seq(-3, 3, by = 0.1)
    ) %>%
        left_join(pars) %>%
        mutate(
            ref =
                sigmoid(a_target * ability_target + b_target) *
                sigmoid(a_nuisance * ref_mean_ability_nuisance + b_nuisance),
            foc =
                sigmoid(a_target * ability_target + b_target) *
                sigmoid(a_nuisance * foc_mean_ability_nuisance + b_nuisance)
        ) %>%
        dplyr::select(item, ability_target, ref, foc) %>%
        mutate(same = ref == foc) %>%
        gather(Group, prob, -item, -ability_target, -same) %>%
        filter((same & Group == "ref") | !same) %>%
        mutate(Group = ifelse(same, "both", Group)) %>%
        mutate(Group = factor(Group, levels = c("both", "ref", "foc"))) %>%
        mutate(item = paste0("Item ", add_zero(item))) %>%
        ggplot(aes(x = ability_target, y = prob, color = Group)) +
        geom_path(size = 2) +
        facet_wrap(~ item) +
        scale_color_manual(values = c("black", "blue", "red"))
}
