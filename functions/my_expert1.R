# Function to reclasify experience levels into ordinal categories
my_expert1 <- function(data){
  case_when(
    data %in% c("Aficionado", "Interessado", "Aficionado", "Outros", "Amador", "Eu não sei") ~ "Amateur",
    data %in% c("Em formação", "En formación") ~ "Formation",
    data %in% c("Experiente",  "Experimentado", "Muy experimentado en cría de avifauna de Argentina, canarios y diamantes en todas sus especies.") ~ "Expert",
    data %in% c("Não crio Cardenalitos", "No crío Cardenalitos", "Criei no passado, mas sempre me interesso pelo tema.") ~ "No_breed",
    data %in% c("") ~ as.character(NA)
  )
}