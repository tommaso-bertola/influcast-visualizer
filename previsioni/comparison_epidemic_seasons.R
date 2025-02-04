library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(paletteer)
library(ggtext)
library(patchwork)
library(ggpubr)

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


incidence <- data.frame()
for (y in years) {
    base <- strsplit(y, "-")[[1]][2]
    file_read <- paste0("/Users/tommasobertola/Git/Influcast/sorveglianza/ILI/", y, "/italia-", base, "_15-ILI.csv")
    incidence <- rbind(incidence, read.csv(file_read) %>%
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

incidence_current <- read.csv("/Users/tommasobertola/Git/Influcast/sorveglianza/ILI/2024-2025/latest/italia-latest-ILI.csv") %>%
    mutate(
        week = sprintf("%02d", settimana),
        year_week = paste0(anno, "-", sprintf("%02d", settimana)),
        orizzonte = factor(year_week,
            levels = labels_order,
            labels = labels_order
        ),
        epi_year = "2024-2025"
    )

blues_palette <- c(colorRampPalette(brewer.pal(9, "Blues")[5:9])(length(years)))
special_years <- c("2009-2010", "2020-2021", "2021-2022", "2022-2023", "2023-2024")

clean_db <- incidence %>%
    filter(week != "53") %>%
    filter(!epi_year %in% special_years) %>%
    mutate(week = ifelse(week < 40, paste0("2025-", week), paste0("2024-", week))) %>%
    mutate(year_week_2 = factor(week, levels = c("2024-39", "2024-40", "2024-41", unique(week)), labels = c("2024-39", "2024-40", "2024-41", unique(week))))

clean_db_special <- incidence %>%
    filter(week != "53") %>%
    filter(epi_year %in% special_years) %>%
    mutate(week = ifelse(week < 40, paste0("2025-", week), paste0("2024-", week))) %>%
    mutate(year_week_2 = factor(week, levels = c("2024-39", "2024-40", "2024-41", unique(week)), labels = c("2024-39", "2024-40", "2024-41", unique(week))))


graph_obj <- ggplot() +
    geom_point(data = clean_db, mapping = aes(x = year_week_2, y = incidenza, col = epi_year), alpha = 0.5, size = 1, show.legend = F) +
    geom_line(data = clean_db, mapping = aes(x = year_week_2, y = incidenza, group = epi_year, col = epi_year), size = 0.7, alpha = 0.5, show.legend = F) +
    geom_point(data = incidence_current, mapping = aes(x = year_week, y = incidenza), col = "black", alpha = 0.5, size = 2) +
    geom_line(data = incidence_current, mapping = aes(x = year_week, y = incidenza, group = 1), col = "black", size = 2, alpha = 0.5) +
    geom_point(data = clean_db_special, mapping = aes(x = year_week_2, y = incidenza, shape = epi_year), col = "red", alpha = 0.5, size = 2, show.legend = F) +
    geom_line(data = clean_db_special, mapping = aes(x = year_week_2, y = incidenza, group = epi_year), col = "red", size = 0.7, alpha = 0.5, show.legend = F) +
    scale_color_manual(values = blues_palette) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    scale_x_discrete(limits = levels(clean_db$year_week_2)) +
    scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5), expand = c(0, 0)) +
    labs(
        x = "Epiweek reported to current season",
        y = "ILI incidence per 1000 individuals",
        title = "Comparison of past ILI seasons incidence at the national level",
        subtitle = "Black line represents current 2024-2025 season\nHorizontal lines indicate the peak of the season",
    )
for (yea in years) {
    max_inc <- rbind(clean_db, clean_db_special) %>%
        filter(epi_year == yea) %>%
        select(settimana, incidenza) %>%
        arrange(desc(incidenza)) %>%
        mutate(settimana = sprintf("%02d", settimana)) %>%
        head(1)
    x_position <- ifelse(max_inc$settimana < 40,
        paste0("2025-", max_inc$settimana),
        paste0("2024-", max_inc$settimana)
    )
    if (yea %in% c("2017-2018")) {
        max_inc_line <- max_inc$incidenza + 0.2
    } else if (yea %in% c("2016-2017")) {
        max_inc_line <- max_inc$incidenza - 0.2
    } else if (yea %in% c("2011-2012")) {
        max_inc_line <- max_inc$incidenza + 0.1
    } else if (yea %in% c("2010-2011")) {
        max_inc_line <- max_inc$incidenza + 0.2
    } else {
        max_inc_line <- max_inc$incidenza
    }
    graph_obj <- graph_obj + annotate("text",
        x = "2024-39",
        y = max_inc_line,
        label = yea,
        col = ifelse(yea %in% special_years, "red", "black"),
        hjust = 0,
        vjust = 0,
        size = 3
    ) + annotate("segment",
        x = x_position,
        y = max_inc$incidenza,
        xend = "2024-39",
        yend = max_inc_line,
        col = ifelse(yea %in% special_years, "red", "black"),
        alpha = 0.4,
        linewidth = 0.4
    )
}
graph_obj


max_inc_2 <- rbind(clean_db, clean_db_special) %>%
    group_by(epi_year) %>%
    filter(incidenza == max(incidenza)) %>%
    ungroup() %>%
    mutate(week = ifelse(settimana < 40, 52 + settimana, settimana)) %>%
    arrange(desc(incidenza)) %>%
    select(week, settimana, incidenza, epi_year) %>%
    mutate(
        avg_peak = median(week),
        sd_peak = sd(week),
        outlier = ifelse(week > avg_peak + 0.5 * sd_peak | week < avg_peak - 0.5 * sd_peak, FALSE, TRUE)
    ) %>%
    as.data.frame()

peak_week <- max_inc_2 %>% ggplot(aes(x = week, y = epi_year)) +
    geom_point(aes(size = incidenza, col = outlier)) +
    theme_light() +
    geom_text(aes(label = incidenza), col = "black", nudge_x = 1) +
    geom_vline(xintercept = max_inc_2$avg_peak, linetype = "dashed") +
    geom_vline(xintercept = max_inc_2$avg_peak - 0.5 * max_inc_2$sd_peak, linetype = "dotted") +
    geom_vline(xintercept = max_inc_2$avg_peak + 0.5 * max_inc_2$sd_peak, linetype = "dotted") +
    scale_x_continuous(
        breaks = seq(40, 80, 1),
        labels = ifelse(seq(40, 80, 1) < 53, seq(40, 80, 1), seq(40, 80, 1) - 52)
    ) +
    scale_colour_paletteer_d("ggsci::alternating_igv") +
    labs(
        x = "Epiweek", y = "Epidemiological season",
        color = expression(paste("Typical peak\noccurrence\nwithin 0.5σ\nfrom median")),
        title = "Typical week of peak for ILI incidence at the national level",
        size = "Max incidence\n1000 individuals"
    )



ggarrange(graph_obj, peak_week, ncol = 1, align = "v", heights = c(1, 0.7))
ggsave("previsioni/plot_maxima.png", width = 10, height = 10, dpi = 300)
