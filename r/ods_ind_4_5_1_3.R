

# 1. Pacotes --------------------------------------------------------------

pacotes = c("dplyr",
            "here",
            "purrr",
            "readr",
            "survey")

lapply(pacotes, library, character.only = T)


### 3. Calculo --------------------------------------------------------------- 

# Percentual de estudantes do 3º ano com proficiência básica ou adequada em matemática e em língua portuguesa - Rede Pública de Ensino (em %)

# faixa de corte nível basico 3 ano: LP maior ou igual 250 ; MT maior ou igual 275
LP_3EM_2015 = svyby(
  formula = as.formula(~PROFICIENCIA_LP_SAEB >= 250),
  by = as.formula(~SEXO+RACA),
  design = design,
  FUN = svymean,
  na.rm = TRUE,
  estimate.only = TRUE
)

MT_3EM_2015 = svyby(
  formula = as.formula(~PROFICIENCIA_MT_SAEB >= 275),
  by = as.formula(~SEXO+RACA),
  design = design,
  FUN = svymean,
  na.rm = TRUE,
  estimate.only = TRUE
)


## 2021
# Percentual de estudantes do 3º ano com proficiência básica ou adequada em matemática e em língua portuguesa - Rede Pública de Ensino (em %)

# faixa de corte nível basico 3 ano: LP maior ou igual 250 ; MT maior ou igual 275

LP_3EM_2021 = svyby(
  formula = as.formula(~PROFICIENCIA_LP_SAEB >= 250),
  by = as.formula(~SEXO+RACA),
  design = design,
  FUN = svymean,
  na.rm = TRUE,
  estimate.only = TRUE
)

MT_3EM_2021 = svyby(
  formula = as.formula(~PROFICIENCIA_MT_SAEB >= 275),
  by = as.formula(~SEXO+RACA),
  design = design,
  FUN = svymean,
  na.rm = TRUE,
  estimate.only = TRUE
)


