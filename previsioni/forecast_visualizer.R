library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(paletteer)
library(ggtext)
library(patchwork)
library(ggpubr)
library(RColorBrewer)

dirs <- list.dirs("/Users/tommasobertola/Git/Influcast/previsioni",
    recursive = FALSE
)
all_data <- data.frame()
for (dir in dirs) {
    files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
    for (file in files) {
        data <- read.csv(file)
        data$folder <- basename(dir)
        data$file <- basename(file)
        all_data <- bind_rows(all_data, data)
    }
}

labels_order <- c(
    paste0("2024-", c(40:52)),
    paste0("2025-", sprintf("%02d", c(1:20)))
)

limit <- "2025_03"
limit_dash <- gsub("_", "-", limit)

all_data <- all_data %>%
    filter(
        file == paste0(limit, ".csv"),
        luogo == "IT",
        target == "ILI"
    )

incidence <- read.csv("/Users/tommasobertola/Git/Influcast/sorveglianza/ILI/2024-2025/latest/italia-latest-ILI.csv") %>%
    mutate(
        year_week = paste0(anno, "-", sprintf("%02d", settimana)),
        orizzonte = factor(year_week,
            levels = labels_order,
            labels = labels_order
        )
    ) %>%
    filter(year_week %in% labels_order[1:which(labels_order == limit_dash)])

last_incidence <- incidence %>%
    filter(orizzonte == gsub("_", "-", limit)) %>%
    arrange(desc(orizzonte)) %>%
    slice(1) %>%
    select(anno, settimana, year_week, incidenza)

# past seasons
years <- c(
    "2003-2004", "2004-2005",
    "2005-2006", "2006-2007",
    "2007-2008", "2008-2009",
    "2009-2010", "2010-2011",
    "2011-2012", "2012-2013",
    "2013-2014", "2014-2015",
    "2015-2016", "2016-2017",
    "2017-2018", "2018-2019",
    "2019-2020", "2020-2021",
    "2021-2022", "2022-2023",
    "2023-2024"
)
special_years <- c("2009-2010", "2020-2021", "2021-2022", "2022-2023", "2023-2024")


incidence_past_seasons <- data.frame()
for (y in years) {
    base <- strsplit(y, "-")[[1]][2]
    file_read <- paste0("/Users/tommasobertola/Git/Influcast/sorveglianza/ILI/", y, "/italia-", base, "_15-ILI.csv")
    incidence_past_seasons <- rbind(incidence_past_seasons, read.csv(file_read) %>%
        mutate(
            week = sprintf("%02d", settimana),
            year_week = paste0(anno, "-", sprintf("%02d", settimana)),
            orizzonte = factor(year_week,
                levels = labels_order,
                labels = labels_order
            ),
            epi_year = y
        ))
}
incidence_past_seasons$epi_year <- factor(incidence_past_seasons$epi_year, levels = years)

palette_past_seasons <- colorRampPalette(brewer.pal(9, "Blues")[5:9])(length(years))

clean_db <- incidence_past_seasons %>%
    filter(week != "53") %>%
    filter(!epi_year %in% special_years) %>%
    mutate(week = ifelse(week < 40, paste0("2025-", week), paste0("2024-", week))) %>%
    mutate(year_week_2 = factor(week, levels = c("2024-39", "2024-40", "2024-41", unique(week)), labels = c("2024-39", "2024-40", "2024-41", unique(week))))

clean_db_special <- incidence_past_seasons %>%
    filter(week != "53") %>%
    filter(epi_year %in% special_years) %>%
    mutate(week = ifelse(week < 40, paste0("2025-", week), paste0("2024-", week))) %>%
    mutate(year_week_2 = factor(week, levels = c("2024-39", "2024-40", "2024-41", unique(week)), labels = c("2024-39", "2024-40", "2024-41", unique(week))))


models_order <- c(
    "Influcast-Ensemble",
    "Influcast-quantileBaseline",
    "comunipd-mobnetSI2R",
    "CSL_PoliTo-metaFlu",
    "EpiQMUL-ARIMA_QMUL",
    "EpiQMUL-SEIR_QMUL",
    "EpiQMUL-SEIRaugment_QMUL",
    "ev_and_modelers-DeepRE",
    "FBK_HE-REST_HE",
    "ISI-FluABCaster",
    "ISI-FluBcast",
    "ISI-GLEAM",
    "ISI-IPSICast",
    "TestTeam-TestModel",
    "UNIPD_NEIDE-SEEIIRS_MCMC"
)


all_data_reshaped <- all_data %>%
    filter(
        anno != "2023",
        !is.na(target),
        id_valore %in% c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975, 0.5) # ,
    ) %>%
    mutate(
        week = settimana + orizzonte,
        settimana2 = sprintf("%02d", ifelse(week > 52, week - 52,
            ifelse(week == 0, 52, week)
        )),
        anno2 = ifelse(week > 52, anno + 1, anno),
        year_week = paste0(anno2, "-", settimana2),
        id_valore = factor(id_valore)
    ) %>%
    filter(anno2 != "2026") %>%
    pivot_wider(names_from = id_valore, values_from = valore)

ensemble_comunipd <- all_data_reshaped %>% filter(
    folder %in% c(
        "Influcast-Ensemble",
        "comunipd-mobnetSI2R"
    )
)

ensemble <- all_data_reshaped %>%
    filter(
        folder == "Influcast-Ensemble"
    )

comunipd <- all_data_reshaped %>% filter(
    folder %in% c(
        "comunipd-mobnetSI2R"
    )
)

baseline <- all_data_reshaped %>%
    filter(
        folder == "Influcast-quantileBaseline"
    ) %>%
    select(year_week, `0.025`, `0.05`, `0.25`, `0.75`, `0.95`, `0.975`, `0.5`, folder) %>%
    rbind(
        data.frame(
            year_week = last_incidence$year_week,
            `0.025` = last_incidence$incidenza,
            `0.05` = last_incidence$incidenza,
            `0.25` = last_incidence$incidenza,
            `0.75` = last_incidence$incidenza,
            `0.95` = last_incidence$incidenza,
            `0.975` = last_incidence$incidenza,
            `0.5` = last_incidence$incidenza,
            folder = "Influcast-quantileBaseline",
            check.names = FALSE
        )
    )

remaining <- all_data_reshaped %>%
    filter(
        !folder %in% c(
            "Influcast-Ensemble",
            "Influcast-quantileBaseline",
            "comunipd-mobnetSI2R",
            "TestTeam-TestModel"
        )
    )

ensemble_comuni <- ggplot() +
    geom_ribbon(data = comunipd, mapping = aes(x = year_week, ymin = `0.025`, ymax = `0.975`, group = folder, fill = folder), alpha = 0.2) +
    geom_ribbon(data = comunipd, mapping = aes(x = year_week, ymin = `0.05`, ymax = `0.95`, group = folder, fill = folder), alpha = 0.4) +
    geom_ribbon(data = comunipd, mapping = aes(x = year_week, ymin = `0.25`, ymax = `0.75`, group = folder, fill = folder), alpha = 1) +
    geom_line(data = ensemble, mapping = aes(x = year_week, y = `0.25`, group = folder, color = folder), size = 1, linetype = "solid") +
    geom_line(data = ensemble, mapping = aes(x = year_week, y = `0.75`, group = folder, color = folder), size = 1, linetype = "solid") +
    geom_line(data = ensemble, mapping = aes(x = year_week, y = `0.95`, group = folder, color = folder), size = 1, linetype = "dashed") +
    geom_line(data = ensemble, mapping = aes(x = year_week, y = `0.05`, group = folder, color = folder), size = 1, linetype = "dashed") +
    geom_line(data = ensemble, mapping = aes(x = year_week, y = `0.025`, group = folder, color = folder), size = 1, linetype = "dotted") +
    geom_line(data = ensemble, mapping = aes(x = year_week, y = `0.975`, group = folder, color = folder), size = 1, linetype = "dotted") +
    geom_line(data = baseline, mapping = aes(x = year_week, y = `0.25`, group = folder, color = folder), size = 1, linetype = "solid") +
    geom_line(data = baseline, mapping = aes(x = year_week, y = `0.75`, group = folder, color = folder), size = 1, linetype = "solid") +
    geom_line(data = baseline, mapping = aes(x = year_week, y = `0.95`, group = folder, color = folder), size = 1, linetype = "dashed") +
    geom_line(data = baseline, mapping = aes(x = year_week, y = `0.05`, group = folder, color = folder), size = 1, linetype = "dashed") +
    geom_line(data = baseline, mapping = aes(x = year_week, y = `0.025`, group = folder, color = folder), size = 1, linetype = "dotted") +
    geom_line(data = baseline, mapping = aes(x = year_week, y = `0.975`, group = folder, color = folder), size = 1, linetype = "dotted") +
    geom_point(data = incidence, mapping = aes(x = year_week, y = incidenza, color = "Data"), size = 3) +
    geom_line(data = incidence, mapping = aes(x = year_week, y = incidenza, group = target, color = "Data"), size = 1) +
    geom_vline(xintercept = last_incidence$year_week, linetype = "dashed", color = "black") +
    geom_vline(xintercept = labels_order[which(labels_order == last_incidence$year_week) + 2], linetype = "dotted", color = "black") +
    geom_hline(yintercept = 5, linetype = "dotted", color = "black", alpha = 0.5) +
    geom_hline(yintercept = 15, linetype = "dotted", color = "black", alpha = 0.5) +
    scale_fill_paletteer_d("ggsci::alternating_igv", direction = -1) +
    coord_cartesian(ylim = c(0, 20), expand = TRUE) +
    labs(
        x = "Epiweek",
        y = NULL,
        fill = NULL,
        color = NULL,
        caption = "Blue curves represent past seasonal epidemic trends.\nRed curves represent epidemic trends outliers of 2009-2010 and 2020-2024 seasons"
    ) +
    theme_bw() +
    theme(strip.text = element_markdown(size = 10), legend.position = c(0.99, 0.7), legend.justification = c(1, 0), plot.caption = element_text(hjust = 0), axis.text.x = element_text(size = 11, angle = 30, hjust = 1), axis.text.y = element_text(size = 11), legend.spacing.y = unit(-0.2, "cm"))

for (y in seq_along(years)) {
    yea <- years[y]
    df <- rbind(clean_db_special, clean_db) %>% filter(epi_year == yea)
    if (yea %in% special_years) {
        ensemble_comuni <- ensemble_comuni +
            geom_point(
                data = df,
                mapping = aes(x = year_week_2, y = incidenza, col = "Special years"),
                alpha = 0.2
            ) +
            geom_line(
                data = df,
                mapping = aes(x = year_week_2, y = incidenza, group = 1, col = "Special years"),
                alpha = 0.2, size = 1
            )
    } else {
        color <- palette_past_seasons[y]
        ensemble_comuni <- ensemble_comuni +
            geom_point(
                data = df,
                mapping = aes(x = year_week_2, y = incidenza), col = color,
                alpha = 0.2, size = 1, show.legend = FALSE
            ) +
            geom_line(
                data = df,
                mapping = aes(x = year_week_2, y = incidenza, group = 1), col = color,
                alpha = 0.2, size = 1, show.legend = FALSE
            )
    }
}

ensemble_comuni <- ensemble_comuni +
    geom_line(aes(x = c("2024-42", "2024-43"), y = c(-10, -20), group = 1, color = "Normal years"), alpha = 0.6, size = 1) +
    geom_point(aes(x = factor("2024-42"), y = -10, color = "Normal years")) +
    scale_color_manual(
        values = c(paletteer_d("nbapalettes::knicks_retro")[c(3, 1, 2)], palette_past_seasons[1], "red")
    )
# ) + annotate("label",
#     x = last_incidence$year_week, y = 20, color = "black",
#     size = 5, label = "Last available data point", angle = 90,
#     vjust = 0, hjust = 1, fontface = "italic", alpha = 0.6
# ) +
# annotate("label",
#     x = labels_order[which(labels_order == last_incidence$year_week) + 2], y = 20, color = "black",
#     size = 5, label = paste0("Today ", Sys.Date()), angle = 90,
#     vjust = 0, hjust = 1, fontface = "italic", alpha = 0.6
# )
# ) + annotate("label",
#     x = last_incidence$year_week, y = 20, color = "black",
#     size = 5, label = "", angle = 90,
#     vjust = 0, hjust = 1, fontface = "italic", alpha = 0.6
# ) +
# annotate("label",
#     x = labels_order[which(labels_order == last_incidence$year_week) + 2], y = 20, color = "black",
#     size = 5, label = "", angle = 90,
#     vjust = 0, hjust = 1, fontface = "italic", alpha = 0.6
# )

all_models <- ggplot(remaining) +
    geom_ribbon(aes(x = year_week, ymin = `0.025`, ymax = `0.975`, group = folder, fill = "95%"), alpha = 1) +
    geom_ribbon(aes(x = year_week, ymin = `0.05`, ymax = `0.95`, group = folder, fill = "90%"), alpha = 1) +
    geom_ribbon(aes(x = year_week, ymin = `0.25`, ymax = `0.75`, group = folder, fill = "50%"), alpha = 1) +
    geom_point(data = incidence, aes(x = year_week, y = incidenza), color = "black", size = 2) +
    geom_line(data = incidence, aes(x = year_week, y = incidenza, group = target), color = "black", size = 1) +
    geom_vline(xintercept = "2024-48", linetype = "dashed", color = "black") +
    geom_hline(yintercept = 5, linetype = "dotted", color = "black", alpha = 0.5) +
    geom_hline(yintercept = 15, linetype = "dotted", color = "black", alpha = 0.5) +
    scale_fill_manual(values = paletteer_d("MoMAColors::Flash")[c(2, 4, 5)]) +
    scale_x_discrete(labels = function(labels) {
        labels <- substr(labels, 6, nchar(labels))
        labels[seq_along(labels) %% 3 != 1] <- ""
        labels
    }) +
    coord_cartesian(ylim = c(0, 20), expand = TRUE) +
    facet_wrap(~folder) +
    labs(
        x = "Week",
        y = NULL,
    ) +
    theme_bw() +
    theme(
        strip.text = element_markdown(size = 8),
        legend.position = "none",
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)
    )


ggarrange(ensemble_comuni, all_models, ncol = 2, align = "hv") +
    plot_annotation(
        title = paste0(
            "Weekly ILI incidence prediction with data up to week ",
            last_incidence$year_week
        ),
        subtitle = "Cases per 1000 individuals",
        caption = "Different color shades respectively show 50%, 90%, and 95% confidence intervals\nData source: influcast.org, elaborated on CloudVeneto infrastructure",
        theme = theme(
            plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 13),
            plot.caption = element_text(size = 11)
        )
    )

ggsave(paste0("previsioni/plot_", last_incidence$year_week, ".png"),
    width = 13, height = 8, dpi = 300
)


# ensemble_comuni + labs(x = "Epiweek", y = "ILI inc. per 1000 indiv. in Italy") +
#     theme(
#         axis.text.x = element_text(size = 13),
#         axis.text.y = element_text(size = 13),
#         axis.title.x = element_text(size = 15),
#         axis.title.y = element_text(size = 15),
#         legend.text = element_text(size = 13)
#     ) -> ensemble_comuni_to_print


# df <- readRDS("pot_critical.rds")
# ggarrange(df, ensemble_comuni_to_print, labels = c("A", "B"), widths = c(1.5, 1), align = "hv")
# ggsave("output.png", width = 18, height = 5, dpi = 300, scale = 0.9)
