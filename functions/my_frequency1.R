# Function to transform the descriptive answers about meeting frequency into a ordinal variable
my_frequency1 <- function(data){
  case_when(
    data %in% c("Casi nunca (1 vez al año)", "Quase nunca (uma vez por ano)") ~ "1",
    data %in% c("Frecuentemente (1 o más al mes)", "Frequentemente (1 ou mais por mês)") ~ "3",
    data %in% c("Ocasionalmente (varias veces al año)", "Ocasionalmente (várias vezes por ano)") ~ "2",
    data %in% c("Todos los días", "Todos os dias") ~ "4",
    data %in% c("") ~ as.character(NA))
}