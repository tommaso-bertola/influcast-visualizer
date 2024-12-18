# Load necessary library
library(readr)
library(ggplot2)
library(reshape2)
library(magrittr)
library(dplyr)
# Load the CSV file
andamento_settimanale_de <- read_csv("/Users/tommasobertola/Git/influcast-local-plots/sorveglianza/andamento-settimanale-de.csv")

# Display the first few rows of the dataframe
head(andamento_settimanale_de)

melted_andamento <- andamento_settimanale_de %>%
    select(-`Andamento RespiVirNet-Epi`) %>%
    melt(, id.vars = c("Settimana"))


influ_a <- "Influ A"
influ_b <- "Influ B"
influ_others <- "Influ Others"
list_pathogens <- c(
    "A/non sottotipizzati" = influ_a,
    "A(H1N1)" = influ_a,
    "A(H3N2)" = influ_a,
    "B" = influ_b,
    "SARS-CoV2" = influ_others,
    "Adenovirus" = influ_others,
    "Bocavirus" = influ_others,
    "Coronavirus (no SARS CoV2)" = influ_others,
    "Metapneumovirus" = influ_others,
    "Rhinovirus" = influ_others,
    "RSV" = influ_others,
    "Virus Parainfluenzali" = influ_others
)

df <- data.frame(
    Name = names(list_pathogens), # Extract names
    Value = unlist(list_pathogens, use.names = FALSE) # Extract values
)


pathogens <- melted_andamento %>%
    left_join(df, by = c("variable" = "Name"))

pathogens %>%
    group_by(Settimana) %>%
    mutate(perc = value / sum(value) * 100) %>%
    ggplot(aes(x = Settimana, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(aes(label = paste0(round(perc, 0),'%')), position = position_dodge(width = 0.9), vjust = -0.5)


pathogens %>%
    group_by(Settimana, Value) %>%
    summarise(sum = sum(value)) %>%
    ungroup() %>%
    group_by(Settimana) %>%
    mutate(perc = sum / sum(sum) * 100) %>%
    ggplot(aes(x = Settimana, y = perc, fill = Value)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(aes(label = round(perc, 1)), position = position_dodge(width = 0.9), vjust = -0.5)
