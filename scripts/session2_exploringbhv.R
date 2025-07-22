# AIM: Explore the distribution of behaviour indicators

# 0. Call the packages ----
library(tidyverse)               # To manipulate data
library(vtable)                  # To calculate summary statistics
library(patchwork)               # For combining plots
library(ggpubr)                  # For combining plots
library(likert)                  # For analyzing Liker variables

# 1. Load the file ----
# This file contains the anonymized answers from the interviews and the answers were standarized in the session1_cleaning script.
my_rs <- read_csv(file="outputs/data_rs_clean.csv")

# 2. Sample size ----
# Number of surveys obtained to measure the behavioral pre-campaign indicators

my_rs |>
  group_by(pais2) |>
  filter(pais2 != "Francia") |>
  dplyr::summarise(answers = n())|>
  mutate(freq = answers / sum(answers)) |>
  mutate(goal = c(160,160, 160, 160),
         glassful = answers*100/goal)

# 3 Demography of the sample ----
## 3.1 Age distribution ----
age <- my_rs |>
  group_by(pais2, edad) |>
  filter(pais2 != "Francia") |>
  dplyr::summarise(count = n()) |>
  mutate(fraction = round(count / sum(count), digits = 2)) |> # Compute percentages
  ggplot( aes(x = edad, y = count, fill = pais2)) +
  geom_bar(stat ="identity", alpha =.6, width = .4) +
  ylim(c(0, 50)) +
  facet_grid(~ pais2) +
  coord_flip() +
  labs(y = "Number of answers", x = "", title = "a)") +
  geom_text(aes(label= fraction), vjust= 0.9, hjust = 0.1, color= "black", size= 4) +
  theme_linedraw() +
  theme(legend.position = "none",
        title = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14) )

# Summary statistics
my_rs |>
  group_by(pais2, t_cria2) |>
  filter(pais2 != "Francia") |>
  st(vars = c('edad'), out = "return", group = 'pais2', group.test = TRUE)

## 3.2 Time breeding -----
time_bree <- my_rs |>
  filter(!is.na(t_cria2)) |>
  filter(pais2 != "Francia") |>
  group_by(pais2, t_cria2) |>
  dplyr::summarise(count = n()) |>
  mutate(fraction = round(count / sum(count), digits = 2)) |>
  ggplot( aes(x = t_cria2, y = count, fill = pais2)) +
  geom_bar(stat ="identity", alpha =.6, width = .4) +
  ylim(c(0, 50)) +
  facet_grid(~ pais2) +
  coord_flip() +
  labs(y = "Number of answers", x = "", title = "b)") +
  geom_text(aes(label= fraction), vjust= 0.9, hjust = 0.1, color= "black", size= 4) +
  theme_linedraw() +
  theme(legend.position = "none",
        title = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14) )

time_bree

# summary statistics
my_rs |>
  group_by(pais2, t_cria2) |>
  filter(pais2 != "Francia") |>
  st(vars = c('t_cria2'), out = "return", group = 'pais2', group.test = TRUE)

## 3.3 Membership ----
membership <- my_rs |>
  filter(!is.na(t_afiliacion2)) |>
  filter(pais2 != "Francia") |>
  group_by(pais2, t_afiliacion2) |>
  dplyr::summarise(count = n()) |>
  mutate(fraction = round(count / sum(count), digits = 2)) |>
  ggplot( aes(x = t_afiliacion2, y = count, fill = pais2)) +
  geom_bar(stat ="identity", alpha =.6, width = .4) +
  ylim(c(0, 50)) +
  facet_grid(~ pais2) +
  coord_flip() +
  labs(y = "Number of answers", x = "", title = "c)" ) +
  geom_text(aes(label= fraction), vjust= 0.9, hjust = 0.8, color= "black", size= 4) +
  theme_linedraw() +
  theme(legend.position = "none",
        title = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14) )

membership

# Summary statistics
my_rs |>
  group_by(pais2, t_cria2) |>
  filter(pais2 != "Francia") |>
  st(vars = c('t_afiliacion2'), out = "return", group = 'pais2', group.test = TRUE)

## 3.4 Figure membership ----
age + time_bree + membership +
  plot_layout(ncol = 1, guides = "collect")

ggsave("outputs/plots/figure_membership.png", width = 10, height = 7)

# 4. Demand behaviour -----
## 4.1 Demand intentions ----
# Intentions to acquire wild-caught birds
# demand_intentions1 = "I would like to have a wild-caught Red Siskin in my aviary this year."
# demand_intentions2 = "I would like to obtain a wild-caught Red Siskin for my aviary this year."
demand_int <- my_rs |>
  dplyr::select(c(pais2, edad, intencion_tenencia1, intencion_tenencia2)) |>
  filter(pais2 != "Francia") |>
  mutate_if(is.numeric, as.factor) |> #convert to factors
  na.omit(intencion_tenencia1,intencion_tenencia2, act_colaboracion2) |> #Delete empty rows
  rename(demand_intentions1 = "intencion_tenencia1",
         demand_intentions2 = "intencion_tenencia2") |>
  droplevels() 

head(demand_int)

demand_int <- as.data.frame(demand_int) # Convert to dataframe because likert function only works with data frames

# Summary statistics by country
demand_int |>
  group_by(pais2) |>
  filter(pais2 != "Francia")  |>
  st(vars = c('demand_intentions1'), out = "return", group = 'pais2', group.test = TRUE)

# Summary statistics by age
demand_int |>
  group_by(edad) |>
  st(vars = c('demand_intentions2'), out = "return", group = 'edad', group.test = TRUE)

## 4.2 Figure demand intention ----
demand_int_age <- plot(likert(demand_int[,c(3:4)], grouping = demand_int[,2])) # By age
demand_int_country <- plot(likert(demand_int[,c(3:4)], grouping = demand_int[,1])) # By country

ggarrange(demand_int_age, demand_int_country,
          common.legend = TRUE,
          labels = c("a)", "b)"),
          font.label = list(size = 12, face = "plain"),
          legend = "bottom")

ggsave("outputs/plots/figure_demand_intention.png", width = 10, height = 4)

## 4.3 Self reported keeping ----
my_rs |>
  select(pais2, edad, cria_cardenal2)  |>
  filter(!is.na(cria_cardenal2)) |>
  filter(pais2 != "Francia")  |>
  group_by(pais2, cria_cardenal2) |>
  dplyr::summarise(count = n()) |>
  mutate(fraction = round(count / sum(count), digits = 2)) |>
  ggplot( aes(x = cria_cardenal2, y = count, fill = pais2)) +
  geom_bar(stat ="identity", alpha =.6, width = .4) +
  ylim(c(0, 50)) +
  facet_grid(~ pais2) +
  coord_flip() +
  labs(y = "Count", x = "" ) +
  geom_text(aes(label= fraction), vjust= 0.9, hjust = 0.8, color= "black", size= 4) +
  theme_linedraw() +
  theme(legend.position = "none",
        title = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14) )

## 4.4 Figure keeping ----
ggsave("outputs/plots/figure_keeping.png", width = 10, height = 7)
  
# Summary statistics by country
my_rs |>
  select(pais2, cria_cardenal2)  |>
  filter(pais2 != "Francia")  |>
  group_by(pais2, cria_cardenal2) |>
  st(vars = c('cria_cardenal2'), out = "return", group = 'pais2', group.test = TRUE)

# Summary statistics by age
my_rs |>
  select(edad, cria_cardenal2)  |>
  group_by(edad, cria_cardenal2) |>
  st(vars = c('cria_cardenal2'), out = "return", group = 'edad', group.test = TRUE)

# 5. Behaviour indicators -----
## 5.1 Adoption intention ----
adopt_intent <- my_rs |>
  dplyr::select(c(pais2, edad, intencion_participacion1,intencion_participacion2)) |>
  filter(pais2 != "Francia") |>
  mutate_at(c('pais2', 'edad', 'intencion_participacion1', 'intencion_participacion2'), as.factor) |> #convert to factors
  na.omit(intencion_participacion1,intencion_participacion2)  |> #Delete empty rows
  droplevels() |>
  rename(adopt_intentions1 = "intencion_participacion1",
         adopt_intentions2 = "intencion_participacion2")

adopt_intent <- as.data.frame(adopt_intent)
str(adopt_intent)

## 5.2 Figure adoption ----
adopt_intent_age <- plot(likert(adopt_intent[,c(3:4)], grouping = adopt_intent[,2])) # By age
adopt_intent_country <- plot(likert(adopt_intent[,c(3:4)], grouping = adopt_intent[,1])) #By country

ggarrange(adopt_intent_age , adopt_intent_country,
          common.legend = TRUE,
          labels = c("a)", "b)"),
          font.label = list(size = 12, face = "plain"),
          legend = "bottom")

ggsave("outputs/plots/figure_adotpion_intention.png", width = 10, height = 4)

## 5.3 Behaviour index ----
bhv_index <- my_rs |>
  select(c(pais2, edad, act_reconocimiento:control_impostor)) |>
  filter(pais2 != "Francia") |>
  mutate_if(is.numeric, as.factor) |> # Convert to factor
  mutate_if(is.character, as.factor) |>
  na.omit(act_reconocimiento:control_impostor) |> #Delete empty rows
  rename(att_recognition = "act_reconocimiento",
         att_collaboration1 = "act_colaboracion1",
         att_collaboration2 = "act_colaboracion2",
         att_self_doubt1 = "act_impostor1",
         att_self_doubt2 = "act_impostor2",
         norm_recognition1 = "norm_reconocimiento1",
         norm_recognition2 = "norm_reconocimiento2",
         norm_collaboration1 = "norm_colaboracion1",
         norm_collaboration2 = "norm_colaboracion2",
         norm_self_doubt1 = "norm_impostor1",
         norm_self_doubt2 = "norm_impostor2",
         control_recognition = "control_reconocimiento",
         control_collaboration = "control_colaboracion",
         control_self_doubt = "control_impostor"
         ) 

bhv_index <- as.data.frame(bhv_index)
str(bhv_index)

# Some variables have one level missing
# Define the columns and their corresponding missing levels
missing_levels <- list(
  norm_collaboration2 = "1",
  norm_self_doubt2 = "1",
  moral1 = "1",
  moral2 = "1",
  control_collaboration = "1"
)

# Loop through the columns and add the missing level if it's not already present
for (col in names(missing_levels)) {
  # Add the missing level to the factor if it doesn't exist
  bhv_index[[col]] <- factor(bhv_index[[col]], levels = c(missing_levels[[col]], levels(bhv_index[[col]])))
}

# Check the structure to ensure the level was added
str(bhv_index)

## 5.4 Figure TPB vars ----
tpb_by_age <- plot(likert(bhv_index[,c(3:18)], grouping = bhv_index[,2])) # By age
tpb_by_country <- plot(likert(bhv_index[,c(3:18)], grouping = bhv_index[,1])) # By country

ggarrange(tpb_by_age, tpb_by_country,
          common.legend = TRUE,
          labels = c("a)", "b)"),
          font.label = list(size = 12, face = "plain"),
          legend = "bottom")

ggsave("outputs/plots/figure_tpb_variables.png", width = 8, height = 15)

# I asked my behaviour data what motivates people...It said, “Mostly missing values and social desirability bias.”

# THE END