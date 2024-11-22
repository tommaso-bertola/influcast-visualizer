library(ggplot2)
library(dplyr)
library(tidyr)
library(paletteer)
library(patchwork)

season <- "2023-2024"
region_names <- readLines("sorveglianza/regions.txt")
weeks <- readLines("sorveglianza/weeks.txt")


df <- data.frame()
for (index_week in seq_along(weeks)) {
    for (index_region in seq_along(region_names)) {
        path <- paste0("/Users/tommasobertola/Git/Influcast/sorveglianza/ILI/", season, "/", region_names[index_region], "-", weeks[index_week], "-ILI.csv")
        path_latest <- paste0("/Users/tommasobertola/Git/Influcast/sorveglianza/ILI/", season, "/latest/", region_names[index_region], "-latest-ILI.csv")
        weekly_df <- read.csv(path) %>% mutate(
            incidenza = as.numeric(incidenza),
            region = region_names[index_region],
            week_estimate = weeks[index_week],
            id_week_estimate = index_week
        )
        latest_df <- read.csv(path_latest) %>% mutate(
            incidenza = as.numeric(incidenza),
            region = region_names[index_region],
            week_estimate = weeks[index_week],
            id_week_estimate = 100
        )
        df <- rbind(df, weekly_df, latest_df)
    }
}
df <- df %>% mutate(id_week_estimate = as.factor(id_week_estimate))

random_colors <- sample(colors(), length(unique(df$id_week_estimate)))
random_colors[length(random_colors)] <- "red"

underreporting_regions <- df %>%
    filter(region != "italia") %>%
    mutate(year_week = paste0(anno, "-", sprintf("%02d", settimana))) %>%
    ggplot(aes(x = year_week, y = incidenza)) +
    geom_point(aes(color = id_week_estimate)) +
    facet_wrap(~region) +
    geom_line(aes(group = id_week_estimate, color = id_week_estimate)) +
    scale_color_manual(values = random_colors, name = "Week Estimate") +
    labs(
        x = "Week",
        y = "ILI incidence per 1000 individuals",
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


underreporting_italy <- df %>%
    filter(region == "italia") %>%
    mutate(year_week = paste0(anno, "-", sprintf("%02d", settimana))) %>%
    ggplot(aes(x = year_week, y = incidenza)) +
    geom_point(aes(color = id_week_estimate)) +
    geom_line(aes(group = id_week_estimate, color = id_week_estimate)) +
    scale_color_manual(values = random_colors, name = "Week Estimate") +
    labs(
        x = "Week",
        y = "ILI incidence per 1000 individuals",
    ) +
    scale_y_continuous(limits = c(4, 20)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


combined_plot <- underreporting_italy / underreporting_regions
ggsave("sorveglianza/check_underreporting_incidence.pdf", combined_plot, width = 29, height = 21, units = "cm")
