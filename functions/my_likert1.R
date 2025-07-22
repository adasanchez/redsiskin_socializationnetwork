# Function to reclassify descriptive answers into Likert 5-points scale
my_likert1 <- function(data){
  case_when(
    data %in% c("Totalmente en desacuerdo", "1. Nada probable", "Nada probable", 
                "Nada provável", "1. Totalmente en desacuerdo") ~ "1",
    data %in% c("En desacuerdo", "2. Poco probable", "Poco probable", 
                "Pouco provável", "Em desacordo", "Falso") ~ "2",
    data %in% c("No estoy seguro (a)", "3. No estoy seguro (a)",
                "Não tenho certeza (a)") ~ "3",
    data %in% c("De acuerdo", "De acuerdo ", "4. Probable", "Probable", "Concordo", "Verdadeiro", "Provável", "4. De acuerdo") ~ "4",
    data %in% c("Muy de acuerdo", "5. Muy probable", "Muy probable", "Totalmente de acordo", "Totalmente verdadeiro", "Muito provável", "5. Totalmente de acuerdo", "Totalmente em desacordo") ~ "5")
}