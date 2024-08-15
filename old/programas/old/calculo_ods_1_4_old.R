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

# # Total Brasil 2022
# 
# ind_1.1.1_brasil = svyby(
#   formula = ~dummy_ln_pobreza_ext,
#   by = ~Ano,
#   design = db,
#   FUN = svymean,
#   na.rm=T,
#   estimate.only = T
# )
# 
# # Regiao
# 
# ind_1.1.1_regiao = svyby(
#   formula = ~dummy_ln_pobreza_ext,
#   by = ~regiao,
#   design = db,
#   FUN = svymean,
#   na.rm=T,
#   estimate.only = T
# )
# 
# # Cor/raca
# 
# ind_1.1.1_cor = svyby(
#   formula = ~dummy_ln_pobreza_ext,
#   by = ~V2010,
#   design = db,
#   FUN = svymean,
#   na.rm=T,
#   estimate.only = T
# )
# 
# # Sexo
# 
# ind_1.1.1_sexo = svyby(
#   formula = ~dummy_ln_pobreza_ext,
#   by = ~V2007,
#   design = db,
#   FUN = svymean,
#   na.rm=T,
#   estimate.only = T
# )
# 
# # Faixa etária
# 
# ind_1.1.1_idade = svyby(
#   formula = ~dummy_ln_pobreza_ext,
#   by = ~VD2006,
#   design = db,
#   FUN = svymean,
#   na.rm=T,
#   estimate.only = T
# )
# 
# # Urbano/rural
# 
# ind_1.1.1_loc = svyby(
#   formula = ~dummy_ln_pobreza_ext,
#   by = ~V1022,
#   design = db,
#   FUN = svymean,
#   na.rm=T,
#   estimate.only = T
# )
# 
# # Condicao de ocupacao
# 
# ind_1.1.1_ocupa = svyby(
#   formula = ~dummy_ln_pobreza_ext,
#   by = ~VD4002,
#   design = db,
#   FUN = svymean,
#   na.rm=T,
#   estimate.only = T
# )
# 
# # Unindo data frame de Brasil e Regiao
# 
# ind_1.1.1_brasil[1,1] = "Brasil"
# colnames(ind_1.1.1_brasil) = colnames(ind_1.1.1_regiao)
# ind_1.1.1_regiao = rbind(
#   ind_1.1.1_brasil,
#   ind_1.1.1_regiao
# )
# 
# 
# # Subset cor/raca - Branca
# 
# ind_1.1.1_brasil_branca = svyby(
#   formula = ~dummy_ln_pobreza_ext,
#   by = ~Ano,
#   design = subset(db, V2010 == "Branca"),
#   FUN = svymean,
#   na.rm=T,
#   estimate.only = T
# )
# 
# ind_1.1.1_regiao_branca = svyby(
#   formula = ~dummy_ln_pobreza_ext,
#   by = ~regiao,
#   design = subset(db, V2010 == "Branca"),
#   FUN = svymean,
#   na.rm=T,
#   estimate.only = T
# )
# 
# ind_1.1.1_sexo_branca = svyby(
#   formula = ~dummy_ln_pobreza_ext,
#   by = ~V2007,
#   design = subset(db, V2010 == "Branca"),
#   FUN = svymean,
#   na.rm=T,
#   estimate.only = T
# )
# 
# ind_1.1.1_idade_branca = svyby(
#   formula = ~dummy_ln_pobreza_ext,
#   by = ~VD2006,
#   design = subset(db, V2010 == "Branca"),
#   FUN = svymean,
#   na.rm=T,
#   estimate.only = T
# )
# 
# ind_1.1.1_loc_branca = svyby(
#   formula = ~dummy_ln_pobreza_ext,
#   by = ~V1022,
#   design = subset(db, V2010 == "Branca"),
#   FUN = svymean,
#   na.rm=T,
#   estimate.only = T
# )
# 
# ind_1.1.1_ocupa_branca = svyby(
#   formula = ~dummy_ln_pobreza_ext,
#   by = ~VD4002,
#   design = subset(db, V2010 == "Branca"),
#   FUN = svymean,
#   na.rm=T,
#   estimate.only = T
# )
# 
# ind_1.1.1_brasil_branca[1,1] = "Brasil"
# colnames(ind_1.1.1_brasil_branca) = colnames(ind_1.1.1_regiao_branca)
# ind_1.1.1_regiao_branca = rbind(
#   ind_1.1.1_brasil_branca,
#   ind_1.1.1_regiao_branca
# )


subgroups <- c("Ano", "regiao")#, "V2007", "VD2006", "V1022", "VD4002")
result <- run_analysis(
  data = db, # base no formato svydesign
  variable = "V2010", # variavel de cor/raca que vai ser a desagregação maior
  indicador = "1.1.1", # nome/num do indicador
  subgroups = subgroups, # outras varaveis de desagregação menor (dentro de cor/raca)
  target_variable = "dummy_ln_pobreza_ext", # variavel que estamos calculando o percentual (dummy em que 1 = proporção que queremos encontrar)
  levels_to_exclude = c("Ignorado") # se tiver algum level que em "variable" que queremos ignorar, se não levels_to_exclude = NULL
)
