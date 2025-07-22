# AIM: We describe the code required to analyse the communication network among Red Siskin breeders community in Venezuela, Brasil, and Spain. 
# We specifically describe:
# 1. The actors typology.
# 2. The interaction between actors (network analysis).

# 0. Call the packages ----
library(tidyverse)   # To manipulate data
library(ggdist)      # For plotting distribution
library(patchwork)   # For combining ggplots
library(ggraph)      # For plotting networks
library(igraph)      # For analysis of network, includinf network metrics  
library(tidygraph)   # For network analysis
library(waffle)      # For visualizing a distribution with Waffle Charts
library(scales)      # For color palette
library(lme4)        # For effect mixed models
library(performance) # For evaluating regression models 
library(broom.mixed) # Convert Statistical Analysis Objects Into Tidy Data Frames
# 1. Load files ----
# This file contains the original survey answers, plus the beavioural indicators calculated in the session3_tpb_nestingfuture
big_data <- read_csv(file = "outputs/big_rs_data.csv")

my_rs <- read_csv(file="outputs/data_rs_clean.csv")

# Fixing annoying col names
big_data <- big_data |>
  rename_with(~ str_replace_all(., " ", "_"), starts_with("Persona"))

# 2. Actor typology ----
## 2.1 Subset the data ----
cardenal_com <- big_data |>
  select(experiencia, edad, cria_cardenal2, 
         pais2, intention, moral, att, norms, conform,
         Persona_4_relacion, Persona_4_edad, Persona_4_experiencia, Persona_4_frecuencia, Persona_4_pais,
         Persona_3_relacion, Persona_3_edad, Persona_3_experiencia, Persona_3_frecuencia, Persona_3_pais,
         Persona_2_relacion, Persona_2_edad, Persona_2_experiencia, Persona_2_frecuencia, Persona_2_pais,
         Persona_1_relacion, Persona_1_edad, Persona_1_experiencia, Persona_1_frecuencia, Persona_1_pais ) |>
  mutate_if(is.integer, as.character) |>
  rename(my_expert = experiencia, 
         my_age = edad, 
         my_keep = cria_cardenal2, 
         my_country = pais2) 

## 2.2 Typology ----
# Create a new variable with the actors' typology based on age categories and experience 
# Use only combinations that already appear in the data

# General typology
my_actors <-  cardenal_com |>
  select(my_age, my_expert, intention) |>
  filter(my_age != "Young") |> # filter this isolated age category
  group_by(my_age, my_expert, intention) |> 
  summarise(count = n() ) |> 
  ungroup() |>
  filter(my_expert != "") |>
  tidyr::unite(my_type, my_age, my_expert, sep = '-', na.rm = TRUE, remove = FALSE) |>
  group_by(my_type, my_age, my_expert) |>
  summarise(intention_freq = sum(intention >= 4, na.rm = T),
            count_sum = sum(count)) |>
  mutate(prop_intention = round(intention_freq/count_sum, 3)) |>
  ungroup()

str(my_actors) #12 actors types based on experience and age category

# By country
my_actors_ve <-  cardenal_com |>
  filter(my_country == "Venezuela") |>
  select(my_age, my_expert, intention) |>
  filter(my_age != "Young") |> # filter this isolated age category
  group_by(my_age, my_expert, intention) |> 
  summarise(count = n() ) |> 
  ungroup() |>
  filter(my_expert != "") |>
  tidyr::unite(my_type, my_age, my_expert, sep = '-', na.rm = TRUE, remove = FALSE) |>
  group_by(my_type, my_age, my_expert) |>
  summarise(intention_freq = sum(intention >= 4, na.rm = T),
            count_sum = sum(count)) |>
  mutate(prop_intention = round(intention_freq/count_sum, 3)) |>
  ungroup()

str(my_actors_ve) #10 actors types based on experience and age category


my_actors_br <-  cardenal_com |>
  filter(my_country == "Brazil") |>
  select(my_age, my_expert, intention) |>
  filter(my_age != "Young") |> # filter this isolated age category
  group_by(my_age, my_expert, intention) |> 
  summarise(count = n() ) |> 
  ungroup() |>
  filter(my_expert != "") |>
  tidyr::unite(my_type, my_age, my_expert, sep = '-', na.rm = TRUE, remove = FALSE) |>
  group_by(my_type, my_age, my_expert) |>
  summarise(intention_freq = sum(intention >= 4, na.rm = T),
            count_sum = sum(count)) |>
  mutate(prop_intention = round(intention_freq/count_sum, 3)) |>
  ungroup()

str(my_actors_br) #11 actors types

my_actors_sp <-  cardenal_com |>
  filter(my_country == "Spain") |>
  select(my_age, my_expert, intention) |>
  filter(my_age != "Young") |> # filter this isolated age category
  group_by(my_age, my_expert, intention) |> 
  summarise(count = n() ) |> 
  ungroup() |>
  filter(my_expert != "") |>
  tidyr::unite(my_type, my_age, my_expert, sep = '-', na.rm = TRUE, remove = FALSE) |>
  group_by(my_type, my_age, my_expert) |>
  summarise(intention_freq = sum(intention >= 4, na.rm = T),
            count_sum = sum(count)) |>
  mutate(prop_intention = round(intention_freq/count_sum, 3)) |>
  ungroup()

str(my_actors_sp) #9 actors types based on experience and age category


my_actors_por <-  cardenal_com |>
  filter(my_country == "Portugal") |>
  select(my_age, my_expert, intention) |>
  filter(my_age != "Young") |> # filter this isolated age category
  group_by(my_age, my_expert, intention) |> 
  summarise(count = n() ) |> 
  ungroup() |>
  filter(my_expert != "") |>
  tidyr::unite(my_type, my_age, my_expert, sep = '-', na.rm = TRUE, remove = FALSE) |>
  group_by(my_type, my_age, my_expert) |>
  summarise(intention_freq = sum(intention >= 4, na.rm = T),
            count_sum = sum(count)) |>
  mutate(prop_intention = round(intention_freq/count_sum, 3)) |>
  ungroup()

str(my_actors_por) # 8 actors types based on experience and age category


# 3. Relationship matrix ----
## General ----
rel_matrix <- cardenal_com |> 
  filter(my_age != "Young") |>  # filter this isolated age category
  pivot_longer(
    cols = contains("Persona"),
    names_to = c("Persona", ".value"),
    names_pattern = "(Persona_\\d+)_(.*)",
    values_transform = list(
      frecuencia = as.numeric,
      relacion = as.character,
      experiencia = as.character,
      edad = as.character,
      pais = as.character
    )
  ) |>
  tidyr::unite(from, my_age, my_expert, sep = '-', na.rm = TRUE, remove = FALSE) |> # Create the variable "from"
  filter(edad != "") |>
  filter(edad != "Young") |>
  filter(experiencia != "") |>
  tidyr::unite(to, edad, experiencia, sep = '-', na.rm = TRUE, remove = FALSE) # Create the variable "to"

head(rel_matrix)


## Venezuela ----
rel_matrix_ve <- cardenal_com |> 
  filter(my_age != "Young") |>  # filter this isolated age category
  filter(my_country == "Venezuela") |>
  pivot_longer(
    cols = contains("Persona"),
    names_to = c("Persona", ".value"),
    names_pattern = "(Persona_\\d+)_(.*)",
    values_transform = list(
      frecuencia = as.numeric,
      relacion = as.character,
      experiencia = as.character,
      edad = as.character,
      pais = as.character
    )
  ) |>
  tidyr::unite(from, my_age, my_expert, sep = '-', na.rm = TRUE, remove = FALSE) |> # Create the variable "from"
  filter(edad != "") |>
  filter(edad != "Young") |>
  filter(experiencia != "") |>
  tidyr::unite(to, edad, experiencia, sep = '-', na.rm = TRUE, remove = FALSE) # Create the variable "to"

head(rel_matrix_ve)

## Spain ----
rel_matrix_sp <- cardenal_com |> 
  filter(my_age != "Young") |>  # filter this isolated age category
  filter(my_country == "Spain") |>
  pivot_longer(
    cols = contains("Persona"),
    names_to = c("Persona", ".value"),
    names_pattern = "(Persona_\\d+)_(.*)",
    values_transform = list(
      frecuencia = as.numeric,
      relacion = as.character,
      experiencia = as.character,
      edad = as.character,
      pais = as.character
    )
  ) |>
  tidyr::unite(from, my_age, my_expert, sep = '-', na.rm = TRUE, remove = FALSE) |> # Create the variable "from"
  filter(edad != "") |>
  filter(edad != "Young") |>
  filter(experiencia != "") |>
  tidyr::unite(to, edad, experiencia, sep = '-', na.rm = TRUE, remove = FALSE) # Create the variable "to"

head(rel_matrix_sp)

## Portugal ----
rel_matrix_por <- cardenal_com |> 
  filter(my_age != "Young") |>  # filter this isolated age category
  filter(my_country == "Portugal") |>
  pivot_longer(
    cols = contains("Persona"),
    names_to = c("Persona", ".value"),
    names_pattern = "(Persona_\\d+)_(.*)",
    values_transform = list(
      frecuencia = as.numeric,
      relacion = as.character,
      experiencia = as.character,
      edad = as.character,
      pais = as.character
    )
  ) |>
  tidyr::unite(from, my_age, my_expert, sep = '-', na.rm = TRUE, remove = FALSE) |> # Create the variable "from"
  filter(edad != "") |>
  filter(edad != "Young") |>
  filter(experiencia != "") |>
  tidyr::unite(to, edad, experiencia, sep = '-', na.rm = TRUE, remove = FALSE) # Create the variable "to"


## Brazil ----
rel_matrix_br <- cardenal_com |> 
  filter(my_age != "Young") |>  # filter this isolated age category
  filter(my_country == "Brazil") |>
  pivot_longer(
    cols = contains("Persona"),
    names_to = c("Persona", ".value"),
    names_pattern = "(Persona_\\d+)_(.*)",
    values_transform = list(
      frecuencia = as.numeric,
      relacion = as.character,
      experiencia = as.character,
      edad = as.character,
      pais = as.character
    )
  ) |>
  tidyr::unite(from, my_age, my_expert, sep = '-', na.rm = TRUE, remove = FALSE) |> # Create the variable "from"
  filter(edad != "") |>
  filter(edad != "Young") |>
  filter(experiencia != "") |>
  tidyr::unite(to, edad, experiencia, sep = '-', na.rm = TRUE, remove = FALSE) # Create the variable "to"

head(rel_matrix_br)

#Extract only the relevant columns to create a relationship matrix
my_relations <- rel_matrix  |> 
  select(from, to, relacion, frecuencia, intention, moral, att, norms, conform, my_country, my_age, my_expert) |>
  filter(from != "YoungAdults") # exclude the age category without expertise level

# Summarise the number of self-loops
self_loops_summary <- my_relations |>
  # Identify self-loops
  filter(from == to) |>
  # Group by the 'from' category
  group_by(my_country, from) |> 
  # Count how many per group
  summarise(n_self_loops = n()) 

self_loops_summary
write_csv(self_loops_summary, "outputs/self_loops_summary.csv")

my_relations_ve <- rel_matrix_ve  |> 
  select(from, to, relacion, frecuencia, intention, moral, att, norms, conform, my_country, my_age, my_expert) |>
  filter(from != "YoungAdults") # exclude the age category without expertise level

my_relations_sp <- rel_matrix_sp  |> 
  select(from, to, relacion, frecuencia, intention, moral, att, norms, conform, my_country, my_age, my_expert) |>
  filter(from != "YoungAdults") # exclude the age category without expertise level

my_relations_por <- rel_matrix_por  |> 
  select(from, to, relacion, frecuencia, intention, moral, att, norms, conform, my_country, my_age, my_expert) |>
  filter(from != "YoungAdults") # exclude the age category without expertise level

my_relations_br <- rel_matrix_br  |> 
  select(from, to, relacion, frecuencia, intention, moral, att, norms, conform, my_country, my_age, my_expert) |>
  filter(from != "YoungAdults") # exclude the age category without expertise level

# 4. Network analysis ----
## 4.1 Graph objects ----
graph_Venezuela <- as_tbl_graph(my_relations_ve, directed = TRUE) |>
  mutate(Information_flow = centrality_degree(mode = 'out'))

graph_Spain <- as_tbl_graph(my_relations_sp, directed = TRUE) |>
  mutate(Information_flow = centrality_degree(mode = 'out'))

graph_Portugal <- as_tbl_graph(my_relations_por, directed = TRUE) |>
  mutate(Information_flow = centrality_degree(mode = 'out'))

graph_Brazil <- as_tbl_graph(my_relations_br, directed = TRUE) |>
  mutate(Information_flow = centrality_degree(mode = 'out'))

# Extract node data as a tibble
node_data_ve <- as_tibble(graph_Venezuela)
node_data_sp <- as_tibble(graph_Spain)
node_data_por <- as_tibble(graph_Portugal)
node_data_br <- as_tibble(graph_Brazil)

# Join `my_actors` information with the node data
node_data_ve <- node_data_ve |>
  left_join(my_actors_ve, by = c("name" = "my_type"))

node_data_sp <- node_data_sp |>
  left_join(my_actors_sp, by = c("name" = "my_type"))

node_data_por <- node_data_por |>
  left_join(my_actors_por, by = c("name" = "my_type"))

node_data_br <- node_data_br |>
  left_join(my_actors_br, by = c("name" = "my_type"))

# Reintegrate the updated node data back into the graph
graph_Venezuela <- graph_Venezuela |>
  activate(nodes) |>
  left_join(node_data_ve, by = c("name" = "name"), multiple = "first")

graph_Spain <- graph_Spain |>
  activate(nodes) |>
  left_join(node_data_sp, by = c("name" = "name"), multiple = "first")

graph_Portugal <- graph_Portugal |>
  activate(nodes) |>
  left_join(node_data_por, by = c("name" = "name"), multiple = "first")

graph_Brazil <- graph_Brazil |>
  activate(nodes) |>
  left_join(node_data_br, by = c("name" = "name"), multiple = "first")


# 5. Network metrics ----
## 5.1 Clustering coefficient and Density ----
net_ve <-  igraph :: graph_from_data_frame(d = my_relations_ve, vertices = my_actors_ve, directed = T)
net_br <-  igraph :: graph_from_data_frame(d = my_relations_br, vertices = my_actors_br, directed = T)
net_por <-  igraph :: graph_from_data_frame(d = my_relations_por, vertices = my_actors_por, directed = T)
net_sp <-  igraph :: graph_from_data_frame(d = my_relations_sp, vertices = my_actors_sp, directed = T)

# Create a named list of network objects and their corresponding countries
networks <- list(
  Venezuela = net_ve,
  Brazil = net_br,
  Portugal = net_por,
  Spain = net_sp
)

# Calculate metrics for each network and store the results in a data frame
coordination_metrics <- map_df(names(networks), function(country) {
  net <- networks[[country]]
  
  # Calculate metrics
  data.frame(
    country = country,
    density = edge_density(net), # Density
    clustering_coefficient = transitivity(net) # # Clustering coefficient
  )
})

head(coordination_metrics)

# Now let's summarize the membership prevalence across country so we can related with the density and clutering metrics
prop_member <- big_data |>
  group_by(country, afiliacion2) |>
  filter_at(vars(country, afiliacion2), all_vars(!is.na(.))) |>
  ungroup() |>
  group_by(country) |>
  summarise(prop_member = sum(afiliacion2 == "Active member") / n()) |>
  filter(country != "Francia")

coordination_metrics_all <- merge(coordination_metrics, prop_member, by = "country")

## 5.2 Social influence and information flow ---- 

# Calculate betweenness, in and outdegree by node and country
# List of countries you want to calculate metrics for
countries <- c("Venezuela", "Brazil", "Spain", "Portugal")

# Initialize an empty list to store results
results_list <- list()

# Loop through each country to calculate metrics
for (country in countries) {
  metrics <- my_relations |>
    filter(my_country == country) |> 
    as_tbl_graph() |>
    activate(nodes) |>
    mutate(
      indegree = centrality_degree(mode = "in"),
      outdegree = centrality_degree(mode = "out"),
      betweenness = centrality_betweenness()
    ) |>
    as_tibble() |> 
    mutate(my_country = country)  # Add country name to the result
  
  # Store the result in the list
  results_list[[country]] <- metrics
}

social_flow <- bind_rows(results_list)

# Calculate reciprocity per node
# Create the net object for igraph format
net <-  igraph :: graph_from_data_frame(d = my_relations, vertices = my_actors, directed = T)

# Function to calculate reciprocity per node
node_reciprocity <- function(graph) {
  sapply(V(graph), function(v) {
    # Get outgoing and incoming neighbors for each node
    out_neighbors <- neighbors(graph, v, mode = "out")
    in_neighbors <- neighbors(graph, v, mode = "in")
    
    # Count reciprocal connections
    reciprocal_count <- sum(out_neighbors %in% in_neighbors)
    
    # Calculate reciprocity for the node
    if (length(out_neighbors) == 0) {
      return(NA)  # Return NA if no outgoing edges
    } else {
      return(reciprocal_count / length(out_neighbors))
    }
  })
}

# Calculate reciprocity for each node
# Define your networks and countries
networks <- list(net_ve = "Venezuela", net_br = "Brazil", net_por = "Portugal", net_sp = "Spain")

# Create a list to store the reciprocity data frames for each country
reciprocity_list <- map2(
  networks, names(networks),
  ~ enframe(node_reciprocity(get(.y)), name = "name", value = "reciprocity") %>%
    mutate(my_country = .x)
)

# Combine all data frames in reciprocity_list by row
reciprocity_df <- bind_rows(reciprocity_list)


# Combine all results into one data frame
metric_country <- left_join(social_flow, reciprocity_df, by = c("name" = "name", "my_country" = "my_country"))
  
# write_csv(metric_country, "outputs/metric_country.csv")

# Create dataframe for each metric
my_indegree <- metric_country |>
  select(name, indegree, my_country) 

my_outdegree <- metric_country |>
  select(name, outdegree, my_country) 

my_betweenness <- metric_country |>
  select(name, betweenness, my_country) 

my_reciprocity <- metric_country |>
  select(name, reciprocity, my_country) 

## 5.3 Color palette by actor type ----

# Define gradients for each age class
seniors_colors <- colorRampPalette(c("#1b7837", "#a6d96a"))(4)  # Dark to light green (Seniors)
adults_colors <- colorRampPalette(c("#FD8D3C", "#FFFFB2"))(4)   # Purple to beige (Adults)
youngadults_colors <- colorRampPalette(c("#0571b0", "#BDD7E7"))(4) # Blue gradient (YoungAdults)
young_colors <- "grey"  # Grey (Young)

# Combine gradients into a single palette
custom_colors <- c(
  seniors_colors,
  adults_colors,
  youngadults_colors,
  young_colors
)

# Assign colors to categories
color_mapping <- c(
  "Seniors-Amateur" = seniors_colors[1],
  "Seniors-Formation" = seniors_colors[2],
  "Seniors-Expert" = seniors_colors[3],
  "Seniors-No_breed" = seniors_colors[4],
  "Adults-Expert" = adults_colors[1],
  "Adults-Amateur" = adults_colors[2],
  "Adults-No_breed" = adults_colors[3],
  "Adults-Formation" = adults_colors[4],
  "YoungAdults-No_breed" = youngadults_colors[1],
  "YoungAdults-Expert" = youngadults_colors[2],
  "YoungAdults-Formation" = youngadults_colors[3],
  "YoungAdults-Amateur" = youngadults_colors[4],
  "Young-Formation" = young_colors
)

# Check the mapping
print(color_mapping)

## 5.4 Plot the waffles ----
plot_indegree <- my_indegree |>
ggplot(aes(values = indegree, fill = name)) +
  waffle::geom_waffle(
    n_rows = 4,        # Number of squares in each row
    color = "white",   # Border color
    flip = TRUE, na.rm = TRUE, 
    make_proportional = T,
    show.legend = F) +
  facet_grid(~ my_country) +
  labs(title = "a)") +
  scale_fill_manual(values = color_mapping) + 
  coord_equal() +
  theme_linedraw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) 

plot_outdegree <- my_outdegree |>
  ggplot(aes(values = outdegree, fill = name)) +
  waffle::geom_waffle(
    n_rows = 4,        # Number of squares in each row
    color = "white",   # Border color
    flip = TRUE, na.rm = TRUE,
    make_proportional = T) +
  facet_grid(~ my_country) +
  scale_fill_manual(values = color_mapping) + 
  labs(title = "b)") +
  coord_equal() +
  theme_linedraw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) 


plot_betweenness <- my_betweenness |>
  ggplot(aes(values = betweenness, fill = name)) +
  waffle::geom_waffle(
    n_rows = 4,        # Number of squares in each row
    color = "white",   # Border color
    flip = TRUE, na.rm = TRUE,
    make_proportional = T,
    show.legend = F) +
  facet_grid(~ my_country) +
  scale_fill_manual(values = color_mapping) + 
  labs(title = "c)") +
  coord_equal() +
  theme_linedraw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) 

plot_reciprocity <- my_reciprocity |>
  filter(reciprocity != "NA") |>
  ggplot(aes(values = reciprocity, fill = name)) +
  waffle::geom_waffle(
    n_rows = 4,        
    color = "white",
    flip = TRUE, 
    na.rm = TRUE,
    make_proportional = T,
    show.legend = F) +
  facet_grid(~ my_country) +
  scale_fill_manual(values = color_mapping) + 
  labs(title = "d)") +
  coord_equal() +
  theme_linedraw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) 

plot_indegree + plot_outdegree + plot_betweenness + plot_reciprocity
ggsave("outputs/plots/figure_network_metric_waffles.png", width = 12, height = 10)

# 6. Regression ----
# How adoption intentions is influenced by behaviour variableas and social network metrics?
## 6.1 Create the DF ----
tmp <-  metric_country |>
  rename("from" = "name")

# Combine the datasets with the intentions with the network metrics and convert intentions in binary variable
network_intentions <- left_join(my_relations, tmp, by = c("from" = "from", "my_country" = "my_country") ) |>
  mutate(intention2 = case_when(
    intention == 1 ~ 0,
    intention == 2 ~ 0,
    intention == 3 ~ 0,
    intention == 4 ~ 1,
    intention == 5 ~ 1
  ))

head(network_intentions)

## 6.2 Set models ----
mdlA <- network_intentions %>%
  glmer(intention2 ~ moral + att + conform + indegree + outdegree + betweenness + (1|from), 
        na.action = "na.omit",
        family = binomial(link = "logit"),
        data =  .)
summary(mdlA)

mdlB <- network_intentions %>%
  glmer(intention2 ~ moral + att + conform + indegree + outdegree + betweenness + (1|my_country), 
        na.action = "na.omit",
        family = binomial(link = "logit"),
        data =  .)
summary(mdlB)

mdlC <- network_intentions %>%
  glmer(intention2 ~ moral + att + conform + indegree + outdegree + betweenness + (1|my_country) + (1|from), 
        na.action = "na.omit",
        family = binomial(link = "logit"),
        data =  .)
summary(mdlC)

mdlD <- network_intentions %>%
  glmer(intention2 ~ moral + att + conform + (1|from), 
        na.action = "na.omit",
        family = binomial(link = "logit"),
        data =  .)
summary(mdlD)

mdlE <- network_intentions %>%
  glmer(intention2 ~ indegree + outdegree + betweenness + (1|from), 
        na.action = "na.omit",
        family = binomial(link = "logit"),
        data =  .)
summary(mdlE)

## 6.2 Perform ----
check_singularity(mdlA) # FALSE
check_singularity(mdlB) # TRUE
check_singularity(mdlC) # FALSE
check_singularity(mdlD) # FALSE  
check_singularity(mdlE) # FALSE

check_convergence(mdlA) # TRUE
check_convergence(mdlB) # TRUE
check_convergence(mdlC) # TRUE
check_convergence(mdlD) # TRUE
check_convergence(mdlE) # TRUE

compare_performance(mdlA, mdlB, mdlC, mdlD, mdlE) # Model A is the best!

## 6.4 Predictions -----
# Get predictions with the model
pred_data <- network_intentions %>%
  mutate(pred_intention2 = predict(mdlA, newdata = ., type = "response", re.form = NA))

## 6.5 Coefficients ----
coeff_mdlA <- broom.mixed::tidy(mdlA, effects = "fixed", conf.int = TRUE)

plot_coeff_mdlA <- coeff_mdlA |>
  ggplot(aes(x = estimate, y = term)) +
  geom_point(size = 2) +  # Points for estimates
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  labs(x = "Coefficient estimate", y = "", title = "a)") +
  # Reference line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

plot_coeff_mdlA 


# 7. Final figures ----
## 7.1 Figure 1 ----
# Create the network plots
graph_Brazil <- graph_Brazil |>
  # Adapt the size of the node
  mutate(size_adjusted = case_when(
    count_sum >= 13 ~ count_sum/2,
    count_sum == 1 ~ count_sum*2.1,
    TRUE ~ as.numeric(count_sum)
  ))

network_br <- ggraph(graph_Brazil, layout = 'kk') + 
  # Regular edges. Map width to 'frequencia'
  geom_edge_link(aes(width = frecuencia), 
                 arrow = arrow(length = unit(3, 'mm')),
                 show.legend = FALSE ) +  
  # Self-loops
  #geom_edge_loop(aes(width = frecuencia),
   #              arrow = arrow(length = unit(4, 'mm')),
    #             show.legend = FALSE) +
  geom_node_point(aes(size = size_adjusted, colour = my_age, shape = my_expert)) + 
  labs(title = "a) Brazil") +
  theme_graph(foreground = 'grey', 
              fg_text_colour = 'white', 
              base_size = 12,
              strip_text_size = 12) +
  scale_size(name = "Node size") +
  scale_color_discrete(name = "Age category") +  
  scale_shape(name = "Expertise level") +
  scale_edge_width(range = c(0.1, 1), name = "Edge width (frequency)") +  # Adjust width scale
  theme(plot.title = element_text(face = "plain", size = 16),
        legend.position = "none" )

##
graph_Venezuela <- graph_Venezuela |>
  # Adapt the size of the node
  mutate(size_adjusted = case_when(
    count_sum >= 11 ~ count_sum/2,
    count_sum == 1 ~ count_sum*2.1,
    TRUE ~ as.numeric(count_sum)
  ))

network_ve <- ggraph(graph_Venezuela, layout = 'kk') + 
  geom_edge_link(aes(width = frecuencia), 
                 arrow = arrow(length = unit(3, 'mm')),
                 show.legend = FALSE ) +
  # Self-loops
  #geom_edge_loop(aes(width = frecuencia),
                 #arrow = arrow(length = unit(4, 'mm')),
                 #show.legend = FALSE) +
  geom_node_point(aes(size = size_adjusted, colour = my_age, shape = my_expert)) + 
  labs(title = "b) Venezuela") +
  theme_graph(foreground = 'grey', 
              fg_text_colour = 'white', 
              base_size = 12,
              strip_text_size = 12) +
  scale_size_identity(name = "Node size") +   
  scale_color_discrete(name = "Age category") +  
  scale_shape(name = "Expertise level") +
  scale_edge_width(range = c(0.1, 1), name = "Edge width (frequency)") +  # Adjust width scale
  theme(plot.title = element_text(face = "plain", size = 16),
        legend.position = "none" )

network_ve

###
graph_Spain <- graph_Spain |>
  # Adapt the size of the node
  mutate(size_adjusted = case_when(
    count_sum >= 8 ~ count_sum/1.7,
    TRUE ~ as.numeric(count_sum)
  ))

network_sp <- ggraph(graph_Spain, layout = 'kk') + 
  geom_edge_link(aes(width = frecuencia), 
                 arrow = arrow(length = unit(3, 'mm')),
                 show.legend = FALSE ) + 
  # Self-loops
  #geom_edge_loop(aes(width = frecuencia),
   #              arrow = arrow(length = unit(4, 'mm')),
    #             show.legend = FALSE) +
  geom_node_point(aes(size = size_adjusted, colour = my_age, shape = my_expert)) + 
  labs(title = "c) Spain") +
  theme_graph(foreground = 'grey', 
              fg_text_colour = 'white', 
              base_size = 12,
              strip_text_size = 12) +
  scale_size_identity(name = "Node size") +   
  scale_color_discrete(name = "Age category") +  
  scale_shape(name = "Expertise level") +
  scale_edge_width(range = c(0.1, 1), name = "Edge width (frequency)") +  # Adjust width scale
  theme(plot.title = element_text(face = "plain", size = 16),
        legend.position = "none")

network_sp

###
graph_Portugal <- graph_Portugal |>
  # Adapt the size of the node
  mutate(size_adjusted = case_when(
    count_sum >= 6 ~ count_sum/1.3,
    count_sum == 1 ~ count_sum*2.09,
    TRUE ~ as.numeric(count_sum)
  ))

network_por <- ggraph(graph_Portugal, layout = 'kk') + 
  geom_edge_link(aes(width = frecuencia), 
                 arrow = arrow(length = unit(3, 'mm')),
                 show.legend = FALSE ) +  
  # Self-loops
  #geom_edge_loop(aes(width = frecuencia),
   #              arrow = arrow(length = unit(4, 'mm')),
    #             show.legend = FALSE) +
  geom_node_point(aes(size = size_adjusted, colour = my_age, shape = my_expert)) + 
  labs(title = "d) Portugal") +
  theme_graph(foreground = 'grey', 
              fg_text_colour = 'white', 
              base_size = 12,
              strip_text_size = 12) +
  scale_size(guide = "none") +
  scale_color_discrete(name = "Age category" ) +  
  scale_shape(name = "Expertise level") +
  scale_edge_width(range = c(0.1, 1), name = "Edge width (frequency)") +
  theme(plot.title = element_text(face = "plain", size = 16),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  # Increase the size of the shapes in the legend
  guides(
    colour = guide_legend(override.aes = list(size = 4)),
    shape = guide_legend(override.aes = list(size = 4))
  )

network_por 

(network_br/ network_ve/ network_sp/ network_por) +
  plot_layout(ncol = 2, nrow = 2)


ggsave("outputs/plots/figure_networks_by_country.png", width = 10, height = 8)


## 7.2 Figure 2 -----
# The fixed effect plot
pred_mdlA_outdegree <- ggplot(pred_data, aes(x = outdegree, y = pred_intention2, color = from)) +
  stat_smooth(method = "lm", formula = y ~ x, linewidth = 0.5, se = T) +
  labs(x = "Outdegree", 
       y = "Predicted adoption intention", 
       title = "a)",
       color = "Actor tipology") +
  scale_color_manual(values = color_mapping) + 
  scale_y_continuous(breaks = seq(0, 1.25, by = 0.1)) +
  theme_classic() +
  theme(legend.position = "right",
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14)
  )

pred_mdlA_att <- ggplot(pred_data, aes(x = att, y = pred_intention2, color = from)) +
  stat_smooth(method = "lm", formula = y ~ x, linewidth = 0.5, se = T) +
  labs(x = "Attitude", 
       y = "Predicted adoption intention", 
       title = "b)") +
  scale_color_manual(values = color_mapping) + 
  scale_y_continuous(breaks = seq(0, 1.25, by = 0.1)) +
  theme_classic() +
  theme(legend.position = "none",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14)
  )

# To see the effect of the grouping factor (actor type)
# Extract random effects
re_mdlA <- as.data.frame(ranef(mdlA))

# Plot random effects
plot_re_mdlA <- re_mdlA  |>
  ggplot(aes(x = condval, y = grp)) +
  geom_point(size = 2) + 
  facet_wrap(~ term,scales = "free_x") +
  geom_errorbarh(aes(xmin = condval -2*condsd,
                     xmax = condval +2*condsd), height=0.2) +
  labs(title = "c)", y = "", x = "Effect size") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_classic() +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        strip.text = element_blank()
  )

# Actors with Positive Random Effects: Indicate a higher likelihood of the outcome (intention2 = 1) compared to the overall intercept.
# Actor with Negative Random Effects: Indicate a lower likelihood of the outcome compared to the overall intercept


pred_mdlA_outdegree + pred_mdlA_att + plot_re_mdlA
ggsave("outputs/plots/figure_randomeffect_networkmodel.png", width = 14, height = 5)


## 7.3 Figure S1 ----
coordination_metrics_plot <- coordination_metrics_all |>
  ggplot(aes(density, clustering_coefficient, color = country, size = prop_member)) +
  geom_point() +
  labs(x = "Density", y = "Clustering", title = "a)") +
  scale_size(name = "Membership proportion") +   
  scale_color_discrete(name = "Country") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(face = "plain", size = 14))
 

self_keeping <- my_rs |>
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
  labs(y = "Count", x = "", title = "b)" ) +
  scale_y_continuous(breaks = seq(0, 50, by = 20)) +
  geom_text(aes(label= fraction), vjust= 0.9, hjust = 0.8, color= "black", size= 3) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(face = "plain", size = 14))

coordination_metrics_plot + self_keeping
ggsave("outputs/plots/figure_clustering_density.png", width = 10, height = 4)

# Why did the regression model break up with its data? Because it couldn't find a significant relationship!

# THE END