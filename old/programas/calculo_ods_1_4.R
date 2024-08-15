#----------------------------------------------------------------------------------------------
# PROJETO: Agenda 2030 e Década Afrodescendente
#
# OBJETIVO: Cálculo Indicadores ODS dos Objetivos 1 e 4
#
# PRODUTO: 
#
# AUTOR: Ana Clara
#
# DATA DE CRIACAO: 01/2024
#
# DATA DE MODIFICACAO:
#
# MODIFICACOES: 
#----------------------------------------------------------------------------------

#----Limpa o ambiente--------------------------------------------------------------

rm(list = ls())



# 1. Packages ---------------------------------------------------------------------
pacotes = c("dplyr", 
            "readr",
            "xlsx",
            "PNADcIBGE",
            "survey")


lapply(pacotes, library, character.only = T)

rm(pacotes)

## 1.1 Functions ------------------------------------------------------------------

source("programas/funcoes/fn_calculo_indicador.R")

# 2. Leitura de bases -------------------------------------------------------------

# db <- get_pnadc(
#   year=2023,
#   quarter = 3,
#   deflator = TRUE,
#   defyear = 2017,
#   defperiod = 1,
#   design = TRUE
# )
db = get_pnadc(
  year = 2022,
  interview = 1,
  deflator = TRUE,
  defyear = 2017,
  # defperiod = 1,
  design = TRUE
)

# db = get_pnadc(year=2023, interview = 5, design = TRUE)

# db
# class(db)



# 3. Ajutes de base --------------------------------------------------------------- 

# PPC R$ 2,33 para US$ 1,00 (2017)
# duvida: atualizar para o valor de 2022 (2,56)? https://data.worldbank.org/indicator/PA.NUS.PRVT.PP
ppc = 2.3273771
ln_pobreza_ext = ppc*3.2
ln_pobreza = ppc*6.85

# sexo V2007
# cor/raca V2010
# faixa etaria VD2006
# status de ocupação (IPEA coloca apenas calculo de todos os outros pra pessoas ocupadas somente)
# localização geográfica (urbano/rural) V1022
# território étnico-racial (IPEA usa regiao)


db = update(
  db,
  VD5002_defl = VD5002*CO2e,
  VD5002_defl_dia = VD5002_defl/30,
  dummy_ln_pobreza_ext = as.factor(ifelse(VD5002_defl_dia >= ln_pobreza_ext, 0, 1)),
  dummy_ln_pobreza = as.factor(ifelse(VD5002_defl_dia >= ln_pobreza, 0, 1)),
  regiao = as.factor(case_when(
    UF %in% c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins") ~ "Norte",
    UF %in% c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia") ~ "Nordeste",
    UF %in% c("Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal") ~ "Centro-Oeste",
    UF %in% c("Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo") ~ "Sudeste",
    UF %in% c("Paraná", "Rio Grande do Sul", "Santa Catarina") ~ "Sul",
    TRUE ~ NA_character_
  ))
)

# 4. Calculo ----------------------------------------------------------------------

subgroups <- c(
  "Ano",
  "regiao",
  "V2007", #sexo
  "VD2006", #faixa etária
  "V1022", #localizacao (urbano/rural)
  "VD4002" #condicao de ocupacao
)

# Indicador 1.1.1

result_ind_1.1.1 <- run_analysis(
  data = db, # base no formato svydesign
  variable = "V2010", # variavel de cor/raca que vai ser a desagregação maior
  indicador = "1.1.1", # nome/num do indicador
  subgroups = subgroups, # outras varaveis de desagregação menor (dentro de cor/raca)
  target_variable = "dummy_ln_pobreza_ext", # variavel que estamos calculando o percentual (dummy em que 1 = proporção que queremos encontrar)
  levels_to_exclude = c("Ignorado") # se tiver algum level que em "variable" que queremos ignorar, se não levels_to_exclude = NULL
)

# Indicador 1.2.1

result_ind_1.2.1 <- run_analysis(
  data = db, # base no formato svydesign
  variable = "V2010", # variavel de cor/raca que vai ser a desagregação maior
  indicador = "1.1.1", # nome/num do indicador
  subgroups = subgroups, # outras varaveis de desagregação menor (dentro de cor/raca)
  target_variable = "dummy_ln_pobreza", # variavel que estamos calculando o percentual (dummy em que 1 = proporção que queremos encontrar)
  levels_to_exclude = c("Ignorado") # se tiver algum level que em "variable" que queremos ignorar, se não levels_to_exclude = NULL
)


