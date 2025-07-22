# The role of socialization network to understand the adoption of demand-reduction behaviours: The Red Siskin's breeders community case study

## 1. Background

This repository contains the analysis supporting the publication:

Sánchez-Mercado, A., Moran, L., Cardozo-Urdaneta, A., Bethelmy, L. (2025). *The role of socialization network to understand the adoption of demand-reduction behaviours: The Red Siskin's breeders community case study*. Conservation Science and Practice. DOI:

The unsustainable and illegal trade of passerine birds, such as the threatened Red Siskin (*Spinus cucullatus*), is deeply embedded in social, economic, and cultural systems. Understanding these complexities is essential for designing effective behavioural interventions to reduce unsustainable wildlife demand. We used social network analysis, decision-making models, and interview data from 204 breeders involved in red siskins captive breeding across Venezuela (a source and demand country), Brazil, Spain, and Portugal (demand countries) to explore how socialization networks, shape their decisions to adopt sustainable bird sourcing practices.

The Red Siskin have been captive-bred since the 19th century and despite a stable and big captive population in several European countries, demand for wild-caught red siskins persist. The main motivation is breeders’ belief that genetic variability in the captive stock is achievable if wild-caught are regularly incorporated (genetic refreshment). Barriers hindering sustainable bird sourcing include low proficiency in managing captive-bred specimens efficiently, limited availability, high costs of captive-bred options in South American countries, and legal variability concerning captive breeding operations across countries. However, a more comprehensive understanding of how these barriers influence adoption intentions is necessary. Specifically, we aim to assess: 

1) How Red Siskin breeders' intentions to use sustainable sources in captive breeding are influenced by psychosocial variables (e.g., attitudes, social norms, and perceived behavioral control) and network metrics that account for social influence and information flow, and how these relationships vary depending on the breeders' context (such as age and expertise level).

2) How network-level metrics that describe coordination among actors (e.g., clustering and density) correlate with captive breeding practices, such as membership in avicultural societies.


This document outlines the steps for processing and analyzing data collected through questionnaires to measure pre-campaign behavior indicators and the socialization network among aviculturist community in Venezuela, Portugal, Spain, and Brazil.

Data comes from the behaviour change campaign *Nesting Future* (*Anidando Futuro* in Spanish), which is focused on reduce demand of the Red Siskin by promoting adoption of sustainable bird sourcing. Nesting Future is part of the project *Demand Reduction Behavior Change in Illegal Venezuelan Threatened Bird Markets*, or [FLYING TOGETHER INITIATIVE](www.volandojuntos.org) for short. A project funded by the UK government through the Illegal Wildlife Trade Challenge Fund [IWT102](https://iwt.challengefund.org.uk/project/XXIWT102) and implemented by the Venezuelan [NGO Provita](https://www.provita.org.ve/).

<p align="center">
  <img src="https://github.com/adasanchez/tpb_redsiskin/blob/main/banners-facebook-anidando-futuro_28042023.png" width="900"/>
</p>

## 2. Workflow

In this repository, we will describe the steps and R codes necessary to:

1) Clean and standardize the format of the responses (**session1**): Surveys were conducted in Spanish and Portuguese so we unified the answers into English. Also, participants answered some questions using a Likert scale ranging from "totally disagree" (1) to "totally agree" (5), with a neutral option "Not sure" (3) and we need to convert this in a numerical scale in order to be able to analyse it using the liker package in R.

2) Explore the behavioral variables (**session2**): Just some plots for fun, exploring the distribution of demographic variables among participants as well as the distribution of the answers they provided.

3) Conduct the internal consistency analysis (**session3**): In this step, we developed a Principal Component Analysis to understand the internal structure of a set of statements measuring the component of the behaviour model used, the Theory of Planned Behaviour (TPB). This allowed us to create scales measuring the TPB’s components (attitudes, social norms, and perceived behavioral control) based on the most parsimonious, functional, and internally consistent of statements.

4) Social network analysis (**session4**): Here, we created an actors' typology based on age category (young adult, adults, seniors) and breeding experience levels (amateur, expert, in formation, not currently breeding), resulting in 12 actor types. 
We used graph analysis to build the audience’s socialization network and calculate node-based metrics. Here, we also fitted random effect logistic regressions to evaluate the relationship between the breeders’ intention to adopt sustainable bird sourcing with network metrics at node level and behavioral variables.

### 2.1 Requirements

This workflow requires:

- [R](https://www.r-project.org/) [Free]
- [RStudio](https://posit.co/download/rstudio-desktop/) [Free]

### 2.2 Setup

1. Download [GitHub Desktop](https://github.com/apps/desktop) or any other Git manager to handle Git without the struggle.

2. Clone this repository to a local folder using your chosen Git manager.
   
3. Install the required packages.

You're all set! :rocket: This repository is now ready for the standard workflow described below.

## 3. Folders

### `/scripts`

Contain code that builds data and performs analyses described in the workflow.

### `/inputs`

Original data source with the answers of the questionaries implemented.

### `/outputs`

Contains all analysis objects generated by files in `/scripts` as well as the plots.

# And remember, use your imagination 🌟 :
**Un mismo manantial. Sobre las raíces comunes del arte y la ciencia**. "*...Ambas actividades tienen en común que uno busca entender y hurgar en el mundo como en ti mismo. Lo diferente es la manera o el lenguaje de expresarlo, y eso lo haces a través de la imaginación porque la mayor parte de la existencia o de la vida no es expresable con los sentidos.*" Jesús Alberto León (26.7.2016)
