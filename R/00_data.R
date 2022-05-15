da <- readr::read_rds("data/coorte-t3.rds") |>
    janitor::clean_names()

recs_ac <- list.files("data/gt3x/") |>
    stringr::str_remove_all("REC|.gt3x") |>
    as.numeric() |>
    sort()

dados <- da |>
    dplyr::filter(a02rec %in% recs_ac) |>
    dplyr::mutate(
        ep_dep = dplyr::if_else(
                mini_a08at == 1 |
                mini_a08ps == 1 |
                mini_a08atpa == 1 |
                mini_a08atpp == 1 |
                mini_a15b == 1,
                "sim",
                "não",
                missing = "não"
        ),
        ep_hipo = dplyr::if_else(mini_d06at == 1 |
            mini_d06ps == 1 | mini_d06at == 1 |
            mini_d08ps == 1 | mini_d09ps == 1,
            "sim",
            "não",
            missing = "não"
            ),
        ep_mania = dplyr::if_else(
            mini_d07at == 1 | mini_d07ps == 1,
            "sim",
            "não",
            missing = "não"
        ),
        diagnostico = dplyr::case_when(
            ep_dep == "sim" & (ep_hipo == "sim" & ep_mania == "não") ~ "tb",
            ep_mania == "sim" ~ "tb",
            ep_dep == "sim" & (ep_hipo == "não" & ep_mania == "não") ~ "tdm",
            TRUE ~ "ctl",
        ),
        a02rec = as.character(a02rec)) |>
    dplyr::select(
        rec = a02rec,
        diagnostico
        )

leituras <- list.files("data/agd", full.names = TRUE) |>
    purrr::set_names(
        \(x) basename(x) |> stringr::str_remove_all("REC|.agd")
        ) |>
    purrr::map(actigraph.sleepr::read_agd) |>
    purrr::map(actigraph.sleepr::collapse_epochs, 60) |>
    dplyr::bind_rows(.id = "rec") |>
    dplyr::group_by(rec) |>
    tidyr::nest()

leituras

da_nested <- dados |>
    dplyr::left_join(leituras, by = "rec")

td <- da_nested |>
    dplyr::mutate(
        tudor_locke = purrr::map(
            data,
            \(x) x |>
                actigraph.sleepr::apply_sadeh() |>
                actigraph.sleepr::apply_tudor_locke()
        )
    )

td$tudor_locke

td_sum <- td |>
    dplyr::mutate(
        td_filtered = purrr::map(tudor_locke, purrr::keep, is.numeric),
        td_summ = purrr::map(td_filtered, \(x) purrr::map_dfr(x, mean))
    ) |>
    tidyr::unnest(td_summ)

dplyr::glimpse(td_sum)

td_sum |>
    dplyr::select(
        diagnostico, efficiency, duration, activity_counts, nonzero_epochs,
        total_sleep_time, wake_after_onset, nb_awakenings, ave_awakening,
        movement_index, fragmentation_index, sleep_fragmentation_index
    ) |>
    dplyr::group_by(diagnostico) |>
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   \(x) mean(x, na.rm = TRUE))) |>
    View()

