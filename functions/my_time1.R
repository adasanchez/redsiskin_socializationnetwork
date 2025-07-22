# Function to reclasify descriptive time periods into ordinal categories
my_time1 <- function(data){
  case_when(
    data %in% c("Recentemente (< 1 ano)", "Recientemente (más de 1 año)", "Recientemente (< 1 año)") ~ "1",
    data %in% c("Hace poco (1 - 2 años)", "Há pouco (1 - 2 anos)", "Hace poco (1-2 años)") ~ "2",
    data %in% c("Hace mucho (3 -10 años)", "Há muito tempo (3 -10 anos)", "Hace mucho (3-10 años)") ~ "3",
    data %in% c("Desde sempre (>10 anos)", "Desde siempre (más de 10 años)", "Desde siempre (>10 años)") ~ "4",
    data %in% c("") ~ as.character(NA))
}
