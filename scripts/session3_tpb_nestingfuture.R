# AIM: Calculated the behavioural indicators based on the Theory of Planned Behavior and built the predictive model

# 0. Call the packages ----
library(tidyverse)  # To manipulate data
library(vtable)     # To calculate summary statistics
library(patchwork)  # For combining plots
library(likert)     # For analyzing Liker variables
library(psych)      # For doing the PCA
library(corrplot)   # For calculate correlation   
library(Hmisc)      # Functions useful for data analysis  
library(lme4)       # For effect mixed models
library(ggdist)     # For visualizations of distributions and uncertainty
library(marginaleffects)
library(performance)

# 1. Load files ----
# This file contains the anonymized answers from the interviews and the answers were standardized in the session1_cleaning script.
my_rs <- read_csv(file="outputs/data_rs_clean.csv")

# 2. Summary ----
all_rs <- my_rs |>
  dplyr::select(c(pais2,intencion_participacion1:control_impostor)) |>
  filter(pais2 != "Francia") |>
  mutate_if(is.numeric, as.factor) |> #convert to factors
  mutate_if(is.character, as.factor) |>
  na.omit(pais2, intencion_participacion1:control_impostor) |> #Delete empty rows
  rename(adoption_intention1 = "intencion_participacion1",
         adoption_intention2 = "intencion_participacion2",
         att_recognition = "act_reconocimiento",
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

str(all_rs)

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
  all_rs[[col]] <- factor(all_rs[[col]], levels = c(missing_levels[[col]], levels(all_rs[[col]])))
}

str(all_rs)

#Summary results
# st(all_rs, group.long = TRUE)

all_rs <- as.data.frame(all_rs) # Convert to a data frame becuse the liker function only works with dataframes
# 5 levels items
plot(likert(all_rs[,c(2:19)], nlevels = 5),
     type = 'heat', low.color = "white",
     high.color = "grey30",
     text.color = "black",
     text.size = 4,
     wrap = 50)

ggsave("outputs/plots/figure_summarytable_likert.png", width = 10, height = 9)

# 3. Constructs ----
siskin_pca <- my_rs |>
  select(act_reconocimiento:control_impostor) |>
  psych::principal(rotate="varimax", 
                   nfactors = 4, scores = TRUE, missing = T) # Calculate PCA for all items

siskin_pca$values # Eigenvalues 
siskin_pca

siskin_pca$scores <- siskin_pca$scores[1:50,]  #just take the first 50

# 4. Subscales ----
# Moral
moral_rs <- my_rs |>
  select(act_impostor2, norm_impostor2, moral1, control_reconocimiento) |>
  psych::principal(rotate="varimax",  scores = TRUE, missing = T)

# Attitudes
att_rs <- my_rs |>
  select(act_colaboracion1, act_colaboracion2, norm_reconocimiento1, norm_colaboracion1) |>
  psych::principal(rotate="varimax", scores=TRUE, missing = T)

# Social - prestige 
social_rs <- my_rs |>
  select(act_reconocimiento, norm_colaboracion1, norm_impostor2) |>
  psych::principal(rotate = "varimax",  scores = TRUE, missing = T)

# Social - conformity
conform_rs <- my_rs |>
  select(control_impostor, control_colaboracion) |>
  psych::principal(rotate = "varimax",  scores = TRUE, missing = T)

# 5. Theory of Planned Behaviour ----
## 5.1 Create the df ----
my_rs <- my_rs |>
  mutate_at(c('edad', 'pais2', 'experiencia'), as.factor) # Convert to factor

# Create a data frame to use with Likert
tpb_rs <- data.frame(
  respondent_id = my_rs$respondent_id,
  age = my_rs$edad,
  country = my_rs$pais2,
  expert = my_rs$experiencia,
  intention = my_rs$intencion_participacion2,
  moral = moral_rs$scores,
  att = att_rs$scores,
  norms = social_rs$scores,
  conform = conform_rs$scores)

colnames(tpb_rs) <- c("respondent_id", "age", "country", "expert", "intention", "moral","att", "norms", "conform")

## 5.2 Check distribution ----
multi.hist(tpb_rs[,5:9])

# Create a binomial version for intentions
tpb_rs <- tpb_rs |>
  mutate(intention2 = case_when(
    intention == 1 ~ 0,
    intention == 2 ~ 0,
    intention == 3 ~ 0,
    intention == 4 ~ 1,
    intention == 5 ~ 1
  ))

# Change expert to a binomial variable too
tpb_rs <- tpb_rs |>
  mutate(expert2 = case_when(
    expert == "Expert" ~ 1,
    expert == "Formation" ~ 0,
    expert == "No_breed" ~ 0,
    expert == "Amateur" ~ 0
  ))

## 5.3 Check correlation ----
cortest_rs <- rcorr(as.matrix(tpb_rs[,5:9]), type = "pearson")
cortest_rs$r
# To visualize
corrplot(cortest_rs$r, method="circle", diag = FALSE, title = "RS")
# High correlation between moral + norm (0.57) and att + norm (0.68). So we excluded the scale norms from subsequent analysis.

## 5.4 Cronbach’s alpha
# Alfa  0.9 Excellent, 
# 0.9  alfa  0.8 Good
# 0.8  alfa  0.7 Acceptable
# 0.7  alfa  0.6 Reasonable 
# 0.6  alfa  0.5 Poor 
# 0.5 Unacceptable
alpha(tpb_rs[5:9], check.keys = TRUE)
# Reasonable values of standardized alpha (>0.6) for all scales except conform (0.56).

# 6 Explore the variability ----
### Figure S1 ----
# Country
plot_att <- tpb_rs |>
  filter(country != "Francia") |>
  ggplot(aes(x= att, y = intention2, color = country)) +
  geom_point() +
  stat_smooth(method = "lm", formula = 'y ~ x', se=F,fullrange = T) +
  facet_wrap(~country) +
  labs(x = "Attitudes", y = "Intentions", 
       title = "a)") +
  theme_linedraw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

plot_conform <- tpb_rs |>
  filter(country != "Francia") |>
  ggplot(aes(x= conform, y = intention2, color = country)) +
  geom_point() +
  stat_smooth(method = "lm", formula = 'y ~ x', se=F,fullrange = T) +
  facet_wrap(~country) +
  labs(x = "Conform", y = "", title = "b)") +
  theme_linedraw() +
  theme(legend.position = "none")
  
plot_moral <- tpb_rs |>
  filter(country != "Francia") |>
  ggplot(aes(x= moral, y = intention2, color = country)) +
  geom_point() +
  stat_smooth(method = "lm", formula = 'y ~ x', se=F,fullrange = T) +
  facet_wrap(~country) +
  labs(x = "Moral", y = "", title = "c)" ) +
  theme_linedraw() +
  theme(legend.position = "none")

plot_att + plot_conform + plot_moral
ggsave("outputs/plots/figure_scales_versus_intention_country.png", width = 10, height = 6)

### Figure S2 ----
# Age
plot_age_att <- tpb_rs |>
  filter(country != "Francia") |>
  filter (age != "NA") |>
  ggplot(aes(x = att, y = intention2, color = age)) +
  geom_point() +
  stat_smooth(method = "lm", formula = 'y ~ x', se=F,fullrange = T) +
  facet_wrap(~ age) +
  labs(x = "Attitudes", y = "Intentions", title = "a)" ) +
  theme_linedraw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
  
plot_age_conform <- tpb_rs |>
  filter(country != "Francia") |>
  filter (age != "NA") |>
  ggplot(aes(x = conform, y = intention2, color = age)) +
  geom_point() +
  stat_smooth(method = "lm", formula = 'y ~ x', se = F,fullrange = T) +
  facet_wrap(~ age) +
  labs(x = "Conform", y = "Intentions", title = "b)" ) +
  theme_linedraw() +
  theme(legend.position = "none")

plot_age_moral <- tpb_rs |>
  filter(country != "Francia") |>
  filter (age != "NA") |>
  ggplot(aes(x = moral, y = intention2, color = age)) +
  geom_point() +
  stat_smooth(method = "lm", formula = 'y ~ x', se = F,fullrange = T) +
  facet_wrap(~ age) +
  labs(x = "Moral", y = "Intentions", title = "c)" ) +
  theme_linedraw() +
  theme(legend.position = "none")

plot_age_att + plot_age_conform +  plot_age_moral

ggsave("outputs/plots/figure_scales_versus_intention_age.png", width = 10, height = 6)

# Experience
plot_ext_att <- tpb_rs |>
  filter(country != "Francia") |>
  filter (expert != "NA") |>
  ggplot(aes(x = att, y = intention2, color = expert)) +
  geom_point() +
  stat_smooth(method = "lm", formula = 'y ~ x', se=F,fullrange = T) +
  facet_wrap(~ expert) +
  labs(x = "Attitudes", y = "Intentions", title = "a)" ) +
  theme_linedraw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

plot_ext_conform <- tpb_rs |>
  filter(country != "Francia") |>
  filter (expert != "NA") |>
  ggplot(aes(x = conform, y = intention2, color = expert)) +
  geom_point() +
  stat_smooth(method = "lm", formula = 'y ~ x', se=F,fullrange = T) +
  facet_wrap(~ expert) +
  labs(x = "Conform", y = "", title = "b)" ) +
  theme_linedraw() +
  theme(legend.position = "none")

plot_ext_moral <- tpb_rs |>
  filter(country != "Francia") |>
  filter (expert != "NA") |>
  ggplot(aes(x = moral, y = intention2, color = expert)) +
  geom_point() +
  stat_smooth(method = "lm", formula = 'y ~ x', se=F,fullrange = T) +
  facet_wrap(~ expert) +
  labs(x = "Moral", y = "", title = "c)" ) +
  theme_linedraw() +
  theme(legend.position = "none")

plot_ext_att + plot_ext_conform + plot_ext_moral
ggsave("outputs/plots/figure_scales_versus_intention_expert.png", width = 10, height = 6)

# 7. Save combined dataset ----
big_rs_data <- cbind(my_rs, tpb_rs, by = "respondent_id")

#Export
# write_csv(big_rs_data, file = "outputs/big_rs_data.csv")

# Why did the Theory of Planned Behavior go to therapy? Because it had great intentions but just couldn’t seem to turn them into actions!
# THE END