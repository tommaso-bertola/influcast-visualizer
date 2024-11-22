library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(paletteer)
library(ggtext)
library(patchwork)
library(ggpubr)

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


incidence <- read.csv("/Users/tommasobertola/Git/Influcast/sorveglianza/ILI/2024-2025/latest/italia-latest-ILI.csv") %>%
    mutate(
        year_week = paste0(anno, "-", settimana),
        orizzonte = factor(year_week,
            levels = labels_order,
            labels = labels_order
        )
    )

last_incidence <- incidence %>%
    arrange(desc(orizzonte)) %>%
    slice(1) %>%
    select(anno, settimana, year_week, incidenza)


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


ensemble_comunipd <- all_data %>%
    filter(
        anno != "2023",
        !is.na(target),
        id_valore %in% c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975, 0.5),
        folder %in% c(
            "Influcast-Ensemble",
            "comunipd-mobnetSI2R"
        )
    ) %>%
    mutate(
        week = settimana + orizzonte,
        settimana2 = sprintf("%02d", ifelse(week > 52, week - 52,
            ifelse(week == 0, 52, week)
        )),
        anno2 = ifelse(week == 0, anno - 1, anno),
        year_week = paste0(anno2, "-", settimana2),
        id_valore = factor(id_valore)
    ) %>%
    pivot_wider(names_from = id_valore, values_from = valore)

ensemble <- all_data %>%
    filter(
        anno != "2023",
        !is.na(target),
        id_valore %in% c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975, 0.5),
        folder == "Influcast-Ensemble"
    ) %>%
    mutate(
        week = settimana + orizzonte,
        settimana2 = sprintf("%02d", ifelse(week > 52, week - 52,
            ifelse(week == 0, 52, week)
        )),
        anno2 = ifelse(week == 0, anno - 1, anno),
        year_week = paste0(anno2, "-", settimana2),
        id_valore = factor(id_valore)
    ) %>%
    pivot_wider(names_from = id_valore, values_from = valore)

comunipd <- all_data %>%
    filter(
        anno != "2023",
        !is.na(target),
        id_valore %in% c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975, 0.5),
        folder == "comunipd-mobnetSI2R"
    ) %>%
    mutate(
        week = settimana + orizzonte,
        settimana2 = sprintf("%02d", ifelse(week > 52, week - 52,
            ifelse(week == 0, 52, week)
        )),
        anno2 = ifelse(week == 0, anno - 1, anno),
        year_week = paste0(anno2, "-", settimana2),
        id_valore = factor(id_valore)
    ) %>%
    pivot_wider(names_from = id_valore, values_from = valore)

baseline <- all_data %>%
    filter(
        anno != "2023",
        !is.na(target),
        id_valore %in% c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975, 0.5),
        folder == "Influcast-quantileBaseline"
    ) %>%
    mutate(
        week = settimana + orizzonte,
        settimana2 = sprintf("%02d", ifelse(week > 52, week - 52,
            ifelse(week == 0, 52, week)
        )),
        anno2 = ifelse(week == 0, anno - 1, anno),
        year_week = paste0(anno2, "-", settimana2),
        id_valore = factor(id_valore),
        folder = "Influcast-Baseline"
    ) %>%
    select(-settimana2, -anno2) %>%
    pivot_wider(names_from = id_valore, values_from = valore) %>%
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
            folder = "Influcast-Baseline",
            check.names = FALSE
        )
    )

remaining <- all_data %>%
    filter(
        anno != "2023",
        !is.na(target),
        id_valore %in% c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975, 0.5),
        !folder %in% c(
            "Influcast-Ensemble",
            "Influcast-quantileBaseline",
            "comunipd-mobnetSI2R",
            "TestTeam-TestModel"
        )
    ) %>%
    mutate(
        week = settimana + orizzonte,
        settimana2 = sprintf("%02d", ifelse(week > 52, week - 52,
            ifelse(week == 0, 52, week)
        )),
        anno2 = ifelse(week == 0, anno - 1, anno),
        year_week = paste0(anno2, "-", settimana2),
        id_valore = factor(id_valore)
    ) %>%
    pivot_wider(names_from = id_valore, values_from = valore)


ensemble_comuni <- ggplot() +
    geom_ribbon(
        data = comunipd,
        mapping = aes(x = year_week, ymin = `0.025`, ymax = `0.975`, group = folder, fill = folder),
        alpha = 0.2
    ) +
    geom_ribbon(
        data = comunipd,
        mapping = aes(x = year_week, ymin = `0.05`, ymax = `0.95`, group = folder, fill = folder),
        alpha = 0.4
    ) +
    geom_ribbon(
        data = comunipd,
        mapping = aes(x = year_week, ymin = `0.25`, ymax = `0.75`, group = folder, fill = folder),
        alpha = 1
    ) +
    geom_line(
        data = ensemble, mapping = aes(
            x = year_week, y = `0.25`, group = folder,
            color = folder
        ),
        size = 1,
        linetype = "solid"
    ) +
    geom_line(
        data = ensemble,
        mapping = aes(
            x = year_week, y = `0.75`, group = folder, color = folder
        ),
        size = 1,
        linetype = "solid"
    ) +
    geom_line(
        data = ensemble,
        mapping = aes(
            x = year_week, y = `0.95`, group = folder, color = folder
        ),
        size = 1,
        linetype = "dashed"
    ) +
    geom_line(
        data = ensemble,
        mapping = aes(
            x = year_week, y = `0.05`, group = folder, color = folder
        ),
        size = 1,
        linetype = "dashed"
    ) +
    geom_line(
        data = ensemble,
        mapping = aes(
            x = year_week, y = `0.025`, group = folder, color = folder
        ),
        size = 1,
        linetype = "dotted"
    ) +
    geom_line(
        data = ensemble,
        mapping = aes(
            x = year_week, y = `0.975`, group = folder, color = folder
        ),
        size = 1,
        linetype = "dotted"
    ) +
    geom_line(
        data = baseline,
        mapping = aes(
            x = year_week, y = `0.25`, group = folder, color = folder
        ),
        size = 1,
        linetype = "solid"
    ) +
    geom_line(
        data = baseline,
        mapping = aes(
            x = year_week, y = `0.75`, group = folder, color = folder
        ),
        size = 1,
        linetype = "solid"
    ) +
    geom_line(
        data = baseline,
        mapping = aes(
            x = year_week, y = `0.95`, group = folder, color = folder
        ),
        size = 1,
        linetype = "dashed"
    ) +
    geom_line(
        data = baseline,
        mapping = aes(
            x = year_week, y = `0.05`, group = folder, color = folder
        ),
        size = 1,
        linetype = "dashed"
    ) +
    geom_line(
        data = baseline,
        mapping = aes(
            x = year_week, y = `0.025`, group = folder, color = folder
        ),
        size = 1,
        linetype = "dotted"
    ) +
    geom_line(
        data = baseline,
        mapping = aes(
            x = year_week, y = `0.975`, group = folder, color = folder
        ),
        size = 1,
        linetype = "dotted"
    ) +
    geom_point(
        data = incidence,
        mapping = aes(x = year_week, y = incidenza, color = " Data"),
        size = 3
    ) +
    geom_line(
        data = incidence,
        mapping = aes(x = year_week, y = incidenza, group = target, color = " Data"),
        size = 1
    ) +
    annotate("text",
        x = "2024-45", y = 0, color = "black",
        size = 5, label = "Past\nFuture", angle = 90,
        vjust = 0.5, hjust = 0, fontface = "italic"
    ) +
    geom_vline(xintercept = "2024-45", linetype = "dashed", color = "black") +
    geom_hline(
        yintercept = 5, linetype = "dotted",
        color = "black", alpha = 0.5
    ) +
    geom_hline(
        yintercept = 15, linetype = "dotted",
        color = "black", alpha = 0.5
    ) +
    scale_fill_paletteer_d("ggsci::alternating_igv", direction = -1) +
    scale_color_manual(
        values = paletteer_d("nbapalettes::knicks_retro")[c(3, 1, 2)],
        aesthetics = "color"
    ) +
    coord_cartesian(ylim = c(0, 15), expand = TRUE) +
    labs(
        x = "Week",
        y = NULL,
        fill = NULL,
        color = NULL
    ) +
    theme_bw() +
    theme(
        strip.text = element_markdown(size = 10),
        legend.position = c(0.99, 0.01),
        legend.justification = c(1, 0),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.spacing.y = unit(-0.2, "cm")
    )


all_models <- ggplot(remaining) +
    geom_ribbon(aes(x = year_week, ymin = `0.025`, ymax = `0.975`, group = folder, fill = "95%"), alpha = 1) +
    geom_ribbon(aes(x = year_week, ymin = `0.05`, ymax = `0.95`, group = folder, fill = "90%"), alpha = 1) +
    geom_ribbon(aes(x = year_week, ymin = `0.25`, ymax = `0.75`, group = folder, fill = "50%"), alpha = 1) +
    geom_point(data = incidence, aes(x = year_week, y = incidenza), color = "black", size = 2) +
    geom_line(data = incidence, aes(x = year_week, y = incidenza, group = target), color = "black", size = 1) +
    geom_vline(xintercept = "2024-45", linetype = "dashed", color = "black") +
    geom_hline(yintercept = 5, linetype = "dotted", color = "black", alpha = 0.5) +
    geom_hline(yintercept = 15, linetype = "dotted", color = "black", alpha = 0.5) +
    scale_fill_manual(values = paletteer_d("MoMAColors::Flash")[c(2, 4, 5)]) +
    scale_x_discrete(labels = function(labels) substr(labels, 6, nchar(labels))) +
    coord_cartesian(ylim = c(0, 15), expand = TRUE) +
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


ggarrange(ensemble_comuni, all_models, ncol = 2, align = "h") +
    plot_annotation(
        title = paste0(
            "Predicted weekly ILI incidence for week ",
            last_incidence$year_week
        ),
        subtitle = "Cases per 1000 individuals",
        caption = "Different color shades respectively show 50%, 90%, and 95% confidence intervals\nData source: influcast.org",
        theme = theme(
            plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 13),
            plot.caption = element_text(size = 11)
        )
    )

ggsave(paste0("previsioni/plot_", last_incidence$year_week, ".png"),
    width = 13, height = 8, dpi = 300
)
