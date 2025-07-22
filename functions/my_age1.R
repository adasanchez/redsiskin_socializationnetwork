# Clasify different range of ages into categories of ages
my_age1 <- function(data){
  case_when(
    data %in% c("18 – 20", "18 – 20 años") ~ "Young",
    data %in% c("21 – 30", "21 – 30 anos") ~ "YoungAdults",
    data %in% c('31 – 40', '41 - 50', "31 - 40 anos", "31 – 40 años", "41 -50", "41 -50 anos") ~ "Adults",
    data %in% c('51 – 60', '>61', "51 – 60", "51 – 60 anos", "Mais de 61", "más de 61 años") ~ "Seniors",
    data %in% c("") ~ as.character(NA))
}
