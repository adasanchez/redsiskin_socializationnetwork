# Function to unify different types collaboration among breeders 
my_relation1 <- function(data){
  case_when(
    data %in% c("Colega avicultor", "Colega criador", "Amigo") ~ "Partner_breeder",
    data %in% c("Directivo en una sociedad de cría", "Diretor de um Federação/Clube ornitológico") ~ "Avicultural_directive",
    data %in% c("Grupo de especialistas", "Grupo de expertos") ~ "Experts",
    data %in% c("Veterinario", "Veterinário") ~ "Third_party",
    data %in% c("") ~ as.character(NA)
  )
}