# AIM: Cleaning the data and standardize categorical answers, including translating from Spanish and Portuguese.

# 0. Call the packages ----
library(tidyverse)               # To manipulate data
library(dplyr)                   # To manipulate data
source("functions/my_likert1.R") # Call a home made functions
source("functions/my_binomial_know.R")
source("functions/my_frequency1.R")
source("functions/my_time1.R")
source("functions/my_expert1.R")
source("functions/my_relation1.R")
source("functions/my_age1.R")

# 1. Load the file ----
# This file contains the anonymized answers from the interviews. The survey ran from 3/3/22 to 25/04/2022
cardenal <- read_csv("inputs/questionnary_replies_rs.csv")

# 2. Unify languages ----
# Change answers from Spanish and Portuguese to English.
# Reorder age (edad), experience time (t_cria), affiliation time (t_filiacion) to reflect a gradient.

## 2.1 Age (edad) ----
cardenal$edad <- factor(cardenal$edad, order = TRUE, 
                        levels =c("18 – 20", "21 – 30",'31 – 40', '41 - 50', '51 – 60', '>61'))

## 2.2 Experience in captive breeding (t_cria) ----
cardenal <- cardenal |>
  mutate(t_cria2 = case_when(
    t_cria %in% c("< 1 ano", "1 año", "Menos de 1 año", "1 ano") ~ "1 year",
    t_cria %in% c("2 años", "2 - 3 anos") ~ "2 - 3 years",
    t_cria %in% c("5 – 10 años", "5 – 10 anos") ~ "5 – 10 years",
    t_cria %in% c("Más de 10 años", "mais de 10 anos", "mas 10 anos") ~ "more than 10 years",
    t_cria %in% c("Não criei Pintassilgo da Venzuela.", "No crío Cardenalitos") ~ "I dont breed RS"
  )) 

# Order levels
cardenal$t_cria2 <- factor(cardenal$t_cria2, order = TRUE,
                           levels =c('I dont breed RS', "1 year",
                                     "2 - 3 years", '5 – 10 year',  "more than 10 years"))

levels(cardenal$t_cria2)

## 2.3 Affiliation time  ----
# (t_afiliacion)
cardenal <- cardenal |>
  mutate(t_afiliacion2 = case_when(
    t_afiliacion %in% c("Há muito tempo (mais de 10 anos), não sou afiliado", "No estoy afiliado") ~ "Not affiliated",
    t_afiliacion %in% c("Há muito tempo (mais de 5 anos)", "Hace tiempo (más de 5 años)") ~ "more than 5 years",
    t_afiliacion %in% c("Hace mucho (más de 10 años)", "Hace mucho (más de 10 años)") ~ "more than 10 years",
    t_afiliacion %in% c("Recentemente (1 ano ou menos)", "Recentemente (mais de 1 ano)", "Recientemente (más de 1 año))") ~ "around 1 year"
  )) 

cardenal$t_afiliacion2 <- factor(cardenal$t_afiliacion2, order = TRUE, 
                                 levels =c('Not affiliated', "around 1 year", 
                                           'more than 5 years','more than 10 years'))
levels(cardenal$t_afiliacion2)

## 2.4 Education level  ----
# (instruccion)
cardenal <- cardenal |>
  mutate(instruccion2 = case_when(
    instruccion %in% c("Primário completo", "Primaria completa") ~ "Primary_school",
    instruccion %in% c("Secundário completo", "Secundaria completa") ~ "High_school",
    instruccion %in% c("Universidade", "Universitaria", "Técnico", "Técnica") ~ "University"
  )) 

cardenal$instruccion2 <- factor(cardenal$instruccion2, order = TRUE, 
                                levels =c('Primary school', "High school", 
                                          'Tecnical','University'))
levels(cardenal$instruccion2)

## 2.5 Work ----
# (trabajo)
cardenal <- cardenal |>
  mutate(trabajo2 = case_when(
    trabajo %in% c("Aposentado (a)", "Militar reformado", "Jubilado (a)", "pensionista", "Incapacitado") ~ "Retired",
    trabajo %in% c("Autônomo", "Autónomo", "Comerciante", "Empresário", "Micro empresário","Trabalhador conta própria", "Vendedor de alimentos para pássaros", "Empresario", "Otro") ~ "Business",
    trabajo %in% c("Empregado (a)", "Empleado (a)") ~ "Employed",
    trabajo %in% c("Desempregados (a)", "Desempleado (a)") ~ "Unemployed",
    trabajo %in% c("Estudante", "Estudiante") ~ "Student",
    trabajo %in% c("Trabalho casual", "Escritor", "Trabajo casual") ~ "Casual_job"
  )) 

table(cardenal$trabajo2) # Just to check
table(cardenal$trabajo)

## 2.6 Gender -----
# (genero)
cardenal <- cardenal |>
  mutate(genero2 = case_when(
    genero %in% c("Femenino", "Feminino") ~ "Female",
    genero %in% c("Masculino") ~ "Male",
    genero %in% c("No responde") ~ "Other"
  )) 
table(cardenal$genero, cardenal$genero2)

## 2.7 Current keeping ---- 
# Do you currently have red siskins in your aviary? (cria_cardenal)
cardenal <- cardenal |>
  mutate(cria_cardenal2 = case_when(
    cria_cardenal %in% c("Não", "No") ~ "No",
    cria_cardenal %in% c("Não responder", "No responde") ~ "No answer",
    cria_cardenal %in% c("Sim", "Si") ~ "Yes"
  )) 

## 2.8 Membership ----
# Do you currently hold a membership to a aviculturists association? (afiliacion)
cardenal <- cardenal |>
  mutate(afiliacion2 = case_when(
    afiliacion %in% c("Membro ativo;", "Miembro activo") ~ "Active member",
    afiliacion %in% c("Membro inativo", "Miembro inactivo") ~ "Inactive member",
    afiliacion %in% c("não afiliado", "No afiliado") ~ "No afiliated"
  )) 

## 2.9 Yield ----
# How many red siskins yield your aviary annually? (cosecha)
cardenal <- cardenal |>
  mutate(cosecha2 = case_when(
    cosecha %in% c(">5 Filhotes", "Más de 5 pichones") ~ "Más de 5 pichones",
    cosecha %in% c("Más de 10 pichones") ~ "Más de 10 pichones",
    cosecha %in% c("Más de 50 pichones") ~ "Más de 50 pichones",
    cosecha %in% c("Não crio Cardenalitos/Tarin.", "No crío Cardenalito") ~ "No crío Cardenalito",
    cosecha %in% c("Nenhum, nunca consegui procriar", "Nenhuma, estou a começar.","Ninguno, estoy iniciando","Ninguno, nunca logré la cría") ~ "Ninguno",
  )) 

## 2.10 Country ----
# (pais)
cardenal <- cardenal |>
  mutate(pais2 = case_when(
    pais %in% "Brasil" ~ "Brazil",
    pais %in% "España" ~ "Spain",
    pais %in% "Portugal" ~ "Portugal",
    pais %in% "Venezuela" ~ "Venezuela",
    pais %in% c("França", "Francia") ~ "Francia"
  ))

table(cardenal$pais, cardenal$pais2)

# 3. Reclassify the answers ----
## 3.1 Liker scale ----
# We created the my_likert1 function to reclassify descriptive answers about demand intention, attitudes, perceived control, and perceived social norms into Likert 5-points scale
# Let see what we have
table(cardenal$pais2, cardenal$act_colaboracion2)

# Apply the function
cardenal <- cardenal |>
  mutate(across(c(intencion_tenencia1:control_impostor), my_likert1))

# Check
table(cardenal$pais2, cardenal$act_colaboracion1) # Just checking

## 3.2 Binomial scale ----
# Reclassify the knowledge variables in to binomial variable
cardenal <- cardenal |>
  mutate(across(c(conocimiento_1:conocimiento_10), my_binomial_know))

table(cardenal$conocimiento_10) # Check

## 3.3 Socialization network ----
table(cardenal$red_comunicacion1)

cardenal <- cardenal |>
  mutate(red_comunicacion1b = case_when(
    red_comunicacion1 %in% c("Não consulto ninguém, tondo as minhas próprias decisões quando quero gerir o meu aviário.", "No consulto a nadie, tomo mis propias decisiones cuando quiero manejar mi aviario") ~ "No_network",
    red_comunicacion1 %in% c("Si, tengo mis consultores", "Sim, eu consulto", "Sí consulto", "Sí, consulto con otras personas") ~ "Yes_network",
    red_comunicacion1 %in% c("No me interesa la cría de Cardenalito.", "Não estou interessado") ~ "No_intentions"
  )) 

table(cardenal$red_comunicacion1)
table(cardenal$red_comunicacion1b) # 68% of the interviewed breeders (115 of 169) ask to other breeder when they want to include red siskins in their aviary

## 3.4 Frequency of contact ----
# Transform the descriptive answers into a ordinal variable
table(cardenal$`Persona 4_frecuencia`)

# Apply the function
cardenal <- cardenal |>
  mutate(across(c(`Persona 1_frecuencia`, `Persona 2_frecuencia`, `Persona 3_frecuencia`, `Persona 4_frecuencia`), my_frequency1))

table(cardenal$`Persona 1_frecuencia`) # Checking

## 4.5. Time (tiempo) ---
# For how long do you know these person ? 
table(cardenal$`Persona 1_tiempo`) # What we have

# Apply function
cardenal <- cardenal |>
  mutate(across(c(`Persona 1_tiempo`, `Persona 2_tiempo`, `Persona 3_tiempo`, `Persona 4_tiempo`), my_time1))

table(cardenal$`Persona 1_tiempo`) # Check

## 3.5 Expert level ----
table(cardenal$`Persona 1_experiencia`)

cardenal <- cardenal |>
  mutate(across(c(experiencia, `Persona 1_experiencia`, `Persona 2_experiencia`, 
                  `Persona 3_experiencia`, `Persona 4_experiencia`), my_expert1))

## 3.6 Collaboration categories (relacion) ----
table(cardenal$`Persona 4_relacion`)

cardenal <- cardenal |>
  mutate(across(c(`Persona 1_relacion`, `Persona 2_relacion`, 
                  `Persona 3_relacion`, `Persona 4_relacion`), my_relation1))

## 3.7 Ages categories (edad) ----
table(cardenal$`Persona 1_edad`)

# Apply function
cardenal <- cardenal |>
  mutate(across(c(edad, `Persona 1_edad`, `Persona 2_edad`, 
                  `Persona 3_edad`, `Persona 4_edad`), my_age1))

# 4 Export clean data ----
write_csv(cardenal, "outputs/data_rs_clean.csv")

# Cleaning data is like doing laundry. It’s tedious, it never ends, and there's always one sock that goes missing!

# THE END