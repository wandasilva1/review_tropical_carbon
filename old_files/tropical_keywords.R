# Packages
library(dplyr)
library(stringr)
library(readxl)

artigos_triados <- read_excel("artigos_triados.xlsx", 
                              sheet = "Planilha1")
#capital letters
artigos_triados$TITLE <- toupper(artigos_triados$TITLE)
artigos_triados$ABSTRACT <- toupper(artigos_triados$ABSTRACT)

# 1. Keywords list
keywords <- c('TROPIC', 'NEOTROPIC', 'EQUATORIAL', 'CENTRAL AMERICA', 'SOUTH AMERICA', 'CARIBBEAN', 'SOUTH ASIA', 'SOUTHEAST ASIA', 'MIDDLE EAST', 'AFRICA', 'RAINFOREST', 'SEASONAL FOREST', 'SAVANNA', 'MANGROVE', 'ATLANTIC', 'AMAZON', 'CAATINGA', 'PANTANAL', 'CERRADO', "INDONESIA", "NIGERIA", "ETHIOPIA", "PHILIPPINES", "DR CONGO", "VIETNAM", "THAILAND", "TANZANIA", "KENYA", "COLOMBIA", "SUDAN", "UGANDA", "YEMEN", "ANGOLA", "MALAYSIA", "GHANA", "PERU", "IVORY COAST", "CAMEROON", "VENEZUELA", "NIGER", "MALI", "BURKINA FASO", "SRI LANKA", "MALAWI", "ZAMBIA", "CHAD", "SOMALIA", "SENEGAL", "GUATEMALA", "ECUADOR", "CAMBODIA", "GUINEA", "BENIN", "RWANDA", "BURUNDI", "SOUTH SUDAN", "HAITI", "DOMINICAN REPUBLIC", "HONDURAS", "CUBA", "PAPUA NEW GUINEA", "TOGO", "SIERRA LEONE", "LAOS", "NICARAGUA", "REPUBLIC OF THE CONGO", "EL SALVADOR", "SINGAPORE", "LIBERIA", "CENTRAL AFRICAN REPUBLIC", "MAURITANIA", "COSTA RICA", "PANAMA", "ERITREA", "JAMAICA", "GAMBIA", "GABON", "GUINEA-BISSAU", "EQUATORIAL GUINEA", "TRINIDAD AND TOBAGO", "TIMOR-LESTE", "MAURITIUS", "DJIBOUTI", "FIJI", "COMOROS", "SOLOMON ISLANDS", "GUYANA", "SURINAME", "MALDIVES", "CAPE VERDE", "BELIZE", "GUADELOUPE", "MARTINIQUE", "VANUATU", "FRENCH GUIANA", "BARBADOS", "SAO TOME AND PRINCIPE", "SAMOA", "SAINT LUCIA", "KIRIBATI", "SEYCHELLES", "GRENADA", "MICRONESIA", "ARUBA", "TONGA", "SAINT VINCENT AND THE GRENADINES", "ANTIGUA AND BARBUDA", "UNITED STATES VIRGIN ISLANDS", "DOMINICA", "SAINT KITTS AND NEVIS", "BRITISH VIRGIN ISLANDS", "MARSHALL ISLANDS", "PALAU", "NAURU", "TUVALU", "INDIA", "HAWAII", "BRAZIL", "BANGLADESH", "MEXICO", "EGYPT", "SOUTH AFRICA", "MYANMAR", "ALGERIA", "ARGENTINA", "MOZAMBIQUE", "SAUDI ARABIA", "MADAGASCAR", "AUSTRALIA", "TAIWAN", "CHILE", "ZIMBABWE", "BOLIVIA", "UNITED ARAB EMIRATES", "LIBYA", "PARAGUAY", "OMAN", "NAMIBIA", "BOTSWANA", "WESTERN SAHARA", "BAHAMAS")

# 2. Columns to search
columns <- c("TITLE", "ABSTRACT")

# 3. Código para buscar as palavras e retornar as observações
# A função str_detect() verifica se as colunas contêm alguma das palavras-chave
# A função across() aplica essa verificação em todas as colunas especificadas
# A função filter() seleciona apenas as linhas onde a condição for verdadeira
results <- artigos_triados %>%
  filter(
    if_any(.cols = all_of(columns), 
           .fns = ~ str_detect(., regex(str_c(keywords, collapse = "|"), ignore_case = TRUE)))
  )

View(results)

#install.packages("openxlsx")
library(openxlsx)
write.xlsx(results, file = "tropical.xlsx")

# 'results' agora contém todas as linhas (observações) que
# possuem alguma das palavras-chave nas colunas especificadas.


#procurando por palavras que devem ser evitadas na base filtrada

delete_words <- c('SUBTROPIC', 'EUROPE', 'UNITED STATES')
results_delet <- results %>%
  filter(
    if_any(.cols = all_of(columns), 
           .fns = ~ str_detect(., regex(str_c(delete_words, collapse = "|"), ignore_case = TRUE)))
  )
View(results_delet)
