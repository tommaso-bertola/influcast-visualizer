library(ggplot2)
library(dplyr)
library(tidyr)
library(paletteer)
library(stringr)
library(patchwork)
library(janitor)
library(reshape2)
df_a <- read.csv("/Users/tommasobertola/Git/Influcast/sorveglianza/ILI+_FLU/2023-2024/italia-2024_17-ILI+_FLU_A.csv")
df_b <- read.csv("/Users/tommasobertola/Git/Influcast/sorveglianza/ILI+_FLU/2023-2024/italia-2024_17-ILI+_FLU_B.csv")
df_it <- read.csv("/Users/tommasobertola/Git/Influcast/sorveglianza/ILI/2023-2024/italia-2024_17-ILI.csv") %>%
    mutate(
        year_week = paste0(anno, "-", sprintf("%02d", settimana)),
        kind = "25th week"
    )

df_it_latest <- read.csv("/Users/tommasobertola/Git/Influcast/sorveglianza/ILI/2023-2024/latest/italia-latest-ILI.csv") %>%
    mutate(
        year_week = paste0(anno, "-", sprintf("%02d", settimana)),
        kind = "latest"
    )

df_a <- df_a %>% mutate(year_week = paste0(anno, "-", sprintf("%02d", settimana)))
df_b <- df_b %>% mutate(year_week = paste0(anno, "-", sprintf("%02d", settimana)))

df_ab <- full_join(df_a, df_b, by = "year_week", suffix = c("_a", "_b")) %>%
    select(-c(anno_a, settimana_a, anno_b, settimana_b)) %>%
    mutate(incidenza_a = as.numeric(incidenza_a), incidenza_b = as.numeric(incidenza_b)) %>%
    mutate(incidenza_tot = incidenza_a + incidenza_b)


df_ab_long <- rbind(
    df_ab %>% select(year_week, incidenza_a) %>%
        rename(incidenza = incidenza_a) %>%
        mutate(kind = "influenza A"),
    df_ab %>% select(year_week, incidenza_b) %>%
        rename(incidenza = incidenza_b) %>%
        mutate(kind = "influenza B"),
    df_ab %>% select(year_week, incidenza_tot) %>%
        rename(incidenza = incidenza_tot) %>%
        mutate(kind = "influenza A+B")
)
df_ab_long$kind <- factor(df_ab_long$kind,
    levels = c("influenza A", "influenza B", "influenza A+B")
)

df_total <- rbind(
    df_ab_long,
    df_it %>% select(year_week, incidenza, kind),
    df_it_latest %>% select(year_week, incidenza, kind)
)

graph_different_datasets <- df_total %>%
    ggplot(aes(x = year_week, y = incidenza)) +
    geom_point(aes(color = kind)) +
    geom_line(aes(group = kind, color = kind)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("sorveglianza/check_strains_from_data_sources.pdf",
    graph_different_datasets,
    width = 12, height = 6
)

wider_total <- clean_names(pivot_wider(df_total,
    names_from = kind,
    values_from = incidenza
)) %>%
    mutate(
        non_ab = latest - influenza_a_b,
        ratio = influenza_a_b / non_ab,
        non_ab_ratio = non_ab / latest,
        ab_ratio = influenza_a_b / latest
    )

year_week_labels <- wider_total %>%
    arrange(year_week) %>%
    select(year_week) %>%
    as.vector() %>%
    unlist() %>%
    unname()

percent_from_iss <- readRDS("sorveglianza/virus_incidence_definitive_ab.rds")
percent_from_iss$year_week <- year_week_labels[-c(1:3)]
percent_from_iss <- percent_from_iss %>% mutate(influenza_a = `A+B` / 100)

wider_total <- wider_total %>%
    select(year_week, influenza_a_b, non_ab) %>%
    melt(id.var = "year_week") %>%
    arrange(year_week) %>%
    mutate(variable = factor(variable,
        levels = rev(c("influenza_a_b", "non_ab"))
    ))


strain_prevalence_comparison_dataset <- ggplot() +
    geom_bar(
        data = wider_total,
        mapping = aes(x = year_week, y = value, fill = variable),
        stat = "identity",
        position = "fill"
    ) +
    geom_point(
        data = percent_from_iss,
        mapping = aes(x = year_week, y = influenza_a),
        color = "red"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
        subtitle = "red points are from ISS epicentro, bars are from influcast",
        title = "Influenza A+B vs non A+B",
        y = "percentage",
        x = "year_week"
    )

ggsave("sorveglianza/strains_comparison.pdf",
    strain_prevalence_comparison_dataset,
    width = 12, height = 6
)
