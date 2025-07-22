# Function to reclassy knowlodge answers into a binomial scale
my_binomial_know <- function(data){
  case_when(
    data %in% c(1) ~ 1,
    data %in% c(0) ~ 0,
    data %in% c("") ~ 0,
    data %in% c("No usar Cardenalitos silvestres", "Usar Cardenalito silvestres sólo si es necesario", "Usar únicamente Cardenalito ancestral", "Usar solo mutaciones o híbridos",
                "Ser difusor de buenas prácticas de cría", "No usar Cardenalitos silvestres en el plantel de cría", "Usar cruces genéticos que eviten el uso de Cardenalitos silvestres para renovación de sangre","Promover cruces genéticos dentro del plantel de cría para la obtención de mutaciones", "Educar e instruir a criadores noveles con buenas prácticas de cría para evitar la demanda de Cardenalitos silvestres", "Promover la cría en cautiverio de otras especies de aves diferentes al Cardenalito.") ~ 1)
}
