library(ggplot2)
library(dplyr)
library(tidyr)
library(paletteer)
library(stringr)
library(patchwork)


file_list <- list.files("/Users/tommasobertola/Git/Influcast/sorveglianza/ILI+_FLU/2023-2024",
    full.names = TRUE
)
df <- data.frame()
for (file in file_list) {
    year_week <- str_extract(file, "\\d{4}_\\d{2}")
    df <- rbind(df, read.csv(file) %>% mutate(progress = year_week))
}
df <- df %>% mutate(
    year_week = paste0(anno, "-", sprintf("%02d", settimana)),
    target = as.factor(target)
)
percent_strain <- df %>%
    ggplot(aes(x = year_week, y = incidenza, fill = target)) +
    geom_bar(position = "fill", stat = "identity") +
    facet_wrap(~progress) +
    theme_minimal() +
    labs(x = "Year-Week", y = "Incidenza") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(fill = "Strain")


underreporting <- df %>% ggplot(aes(
    x = year_week,
    y = incidenza,
    shape = target
)) +
    geom_point(aes(color = progress)) +
    geom_line(aes(group = interaction(progress, target), color = progress)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(shape = "Strain", color = "Week of data publication")


combined_plot <- percent_strain / underreporting
ggsave("sorveglianza/check_underreporting_strains.pdf",
    combined_plot,
    width = 29, height = 21, units = "cm"
)
