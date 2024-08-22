#----------------------------------------------------------------------------------------------
# PROJETO: Agenda 2030 e Década Afrodescendente
#
# OBJETIVO: Cálculo Indicadores ODS 4.5 e 4.6
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
            "lapply",
            "survey")


lapply(pacotes, library, character.only = T)

rm(pacotes)

# 2015 --------------------------------------------------------------------------

## 9 ano EF ---------------------------------------------------------------------

### 1. Leitura de bases -------------------------------------------------------------


db = read.csv2("data/1_raw/saeb/microdados_saeb_2015/DADOS/TS_ALUNO_9EF.csv",
               sep = ",", dec = ".")

### 2. Filtrando a base -------------------------------------------------------------

# checando se o peso de matematica e portugues e o mesmo
# prop.table(table(db$PESO_ALUNO_LP == db$PESO_ALUNO_MT))

# Transformando variavel de sexo e raca

db = db %>%
  rename(SEXO = TX_RESP_Q001, RACA = TX_RESP_Q002) %>%  # Renomeia as variáveis
  mutate(
    SEXO = recode(SEXO,
                  "A" = "Masculino",
                  "B" = "Feminino",
                  .default = "NS/NR",  # Qualquer outra categoria será "NS/NR"
                  .missing = "NS/NR"), # NA será "NS/NR"
    RACA = recode(RACA,
                  "A" = "Branco(a)",
                  "B" = "Pardo(a)",
                  "C" = "Preto(a)",
                  "D" = "Amarelo(a)",
                  "E" = "Indígena",
                  "F" = "NS/NR",  # "F" será recodificado como "NS/NR"
                  .default = "NS/NR",  # Qualquer outra categoria será "NS/NR"
                  .missing = "NS/NR")  # NA será "NS/NR"
  )


prop.table(table((db$IN_PREENCHIMENTO_PROVA &
                    is.na(db$PESO_ALUNO_LP) == FALSE &
                    db$IN_PROFICIENCIA == 1 &
                    db$IN_PUBLICA == 1)))
db_filt = db[(
  db$IN_PREENCHIMENTO_PROVA == 1 &
    is.na(db$PESO_ALUNO_LP) == FALSE &
    db$IN_PROFICIENCIA == 1 &
    db$IN_PUBLICA == 1
           ),]

design = svydesign(id = ~ID_ALUNO, 
                    strata = ~ESTRATO_ANEB, 
                    weights = ~PESO_ALUNO_LP, 
                    data = db_filt, 
                    nest = TRUE)



### 3. Calculo --------------------------------------------------------------- 

# Percentual de estudantes do 9º ano com proficiência básica ou adequada em matemática e em língua portuguesa - Rede Pública de Ensino (em %)

# faixa de corte nível basico 9 ano: LP maior ou igual 200 ; MT maior ou igual 225

LP_9EF_2015 = svyby(
  formula = as.formula(~PROFICIENCIA_LP_SAEB >= 200),
  by = as.formula(~SEXO+RACA),
  design = design,
  FUN = svymean,
  na.rm = TRUE,
  estimate.only = TRUE
)

MT_9EF_2015 = svyby(
  formula = as.formula(~PROFICIENCIA_MT_SAEB >= 225),
  by = as.formula(~SEXO+RACA),
  design = design,
  FUN = svymean,
  na.rm = TRUE,
  estimate.only = TRUE
)

## 5 ano EF --------------------------------------------------------------

### 1. Leitura de bases -------------------------------------------------------------


db = read.csv2("data/1_raw/saeb/microdados_saeb_2015/DADOS/TS_ALUNO_5EF.csv",
               sep = ",", dec = ".")

### 2. Filtrando a base -------------------------------------------------------------

# checando se o peso de matematica e portugues e o mesmo
# prop.table(table(db$PESO_ALUNO_LP == db$PESO_ALUNO_MT))

# Transformando variavel de sexo e raca

db = db %>%
  rename(SEXO = TX_RESP_Q001, RACA = TX_RESP_Q002) %>%  # Renomeia as variáveis
  mutate(
    SEXO = recode(SEXO,
                  "A" = "Masculino",
                  "B" = "Feminino",
                  .default = "NS/NR",  # Qualquer outra categoria será "NS/NR"
                  .missing = "NS/NR"), # NA será "NS/NR"
    RACA = recode(RACA,
                  "A" = "Branco(a)",
                  "B" = "Pardo(a)",
                  "C" = "Preto(a)",
                  "D" = "Amarelo(a)",
                  "E" = "Indígena",
                  "F" = "NS/NR",  # "F" será recodificado como "NS/NR"
                  .default = "NS/NR",  # Qualquer outra categoria será "NS/NR"
                  .missing = "NS/NR")  # NA será "NS/NR"
  )

db_filt = db[(
  db$IN_PREENCHIMENTO_PROVA == 1 &
    is.na(db$PESO_ALUNO_LP) == FALSE &
    db$IN_PROFICIENCIA == 1 &
    db$IN_PUBLICA == 1
),]


design = svydesign(id = ~ID_ALUNO, 
                    strata = ~ESTRATO_ANEB, 
                    weights = ~PESO_ALUNO_LP, 
                    data = db_filt, 
                    nest = TRUE)



### 3. Calculo --------------------------------------------------------------- 

# Percentual de estudantes do 5º ano com proficiência básica ou adequada em matemática e em língua portuguesa - Rede Pública de Ensino (em %)

# faixa de corte nível basico 5 ano: LP maior ou igual 150 ; MT maior ou igual 175

LP_5EF_2015 = svyby(
  formula = as.formula(~PROFICIENCIA_LP_SAEB >= 150),
  by = as.formula(~SEXO+RACA),
  design = design,
  FUN = svymean,
  na.rm = TRUE,
  estimate.only = TRUE
)

MT_5EF_2015 = svyby(
  formula = as.formula(~PROFICIENCIA_MT_SAEB >= 175),
  by = as.formula(~SEXO+RACA),
  design = design,
  FUN = svymean,
  na.rm = TRUE,
  estimate.only = TRUE
)

## 3 ano EM ---------------------------------------------------------------------

### 1. Leitura de bases -------------------------------------------------------------


db = read.csv2("data/1_raw/saeb/microdados_saeb_2015/DADOS/TS_ALUNO_3EM.csv",
               sep = ",", dec = ".")

### 2. Filtrando a base -------------------------------------------------------------

# checando se o peso de matematica e portugues e o mesmo
# prop.table(table(db$PESO_ALUNO_LP == db$PESO_ALUNO_MT))

# Transformando variavel de sexo e raca

db = db %>%
  rename(SEXO = TX_RESP_Q001, RACA = TX_RESP_Q002) %>%  # Renomeia as variáveis
  mutate(
    SEXO = recode(SEXO,
                  "A" = "Masculino",
                  "B" = "Feminino",
                  .default = "NS/NR",  # Qualquer outra categoria será "NS/NR"
                  .missing = "NS/NR"), # NA será "NS/NR"
    RACA = recode(RACA,
                  "A" = "Branco(a)",
                  "B" = "Pardo(a)",
                  "C" = "Preto(a)",
                  "D" = "Amarelo(a)",
                  "E" = "Indígena",
                  "F" = "NS/NR",  # "F" será recodificado como "NS/NR"
                  .default = "NS/NR",  # Qualquer outra categoria será "NS/NR"
                  .missing = "NS/NR")  # NA será "NS/NR"
  )

db_filt = db[(
  db$IN_PREENCHIMENTO_PROVA == 1 &
    is.na(db$PESO_ALUNO_LP) == FALSE &
    db$IN_PROFICIENCIA == 1 &
    db$IN_PUBLICA == 1
),]

design = svydesign(id = ~ID_ALUNO, 
                    strata = ~ESTRATO_ANEB, 
                    weights = ~PESO_ALUNO_LP, 
                    data = db_filt, 
                    nest = TRUE)



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


# 2021 -------------------------------------------------------------------------

## 9 ano EF ---------------------------------------------------------------------

### 1. Leitura de bases -------------------------------------------------------------


db = read.csv2("data/1_raw/saeb/microdados_saeb_2021/DADOS/TS_ALUNO_9EF.csv",
               sep = ";", dec = ".")

### 2. Filtrando a base -------------------------------------------------------------

# checando se o peso de matematica e portugues e o mesmo
# prop.table(table(db$PESO_ALUNO_LP == db$PESO_ALUNO_MT))

# Transformando variavel de sexo e raca

db = db %>%
  rename(SEXO = TX_RESP_Q01, RACA = TX_RESP_Q04) %>%  # Renomeia as variáveis
  mutate(
    SEXO = recode(SEXO,
                  "A" = "Masculino",
                  "B" = "Feminino",
                  .default = "NS/NR",  # Qualquer outra categoria será "NS/NR"
                  .missing = "NS/NR"), # NA será "NS/NR"
    RACA = recode(RACA,
                  "A" = "Branco(a)",
                  "C" = "Pardo(a)",
                  "B" = "Preto(a)",
                  "D" = "Amarelo(a)",
                  "E" = "Indígena",
                  "F" = "NS/NR",  # "F" será recodificado como "NS/NR"
                  .default = "NS/NR",  # Qualquer outra categoria será "NS/NR"
                  .missing = "NS/NR")  # NA será "NS/NR"
  )

prop.table(table((#db$IN_AMOSTRA == 1 &
                    is.na(db$PESO_ALUNO_LP) == FALSE &
                    (db$IN_PROFICIENCIA_LP == 1 |
                    db$IN_PROFICIENCIA_MT == 1) &
                    db$IN_PUBLICA == 1)))

db_filt = db[(
  is.na(db$PESO_ALUNO_LP) == FALSE &
    (db$IN_PROFICIENCIA_LP == 1 |
    db$IN_PROFICIENCIA_MT == 1) &
    db$IN_PUBLICA == 1
),]

design = svydesign(id = ~ID_ALUNO, 
                    strata = ~ESTRATO, 
                    weights = ~PESO_ALUNO_LP, 
                    data = db_filt, 
                    nest = TRUE)



### 3. Calculo --------------------------------------------------------------- 

# Percentual de estudantes do 9º ano com proficiência básica ou adequada em matemática e em língua portuguesa - Rede Pública de Ensino (em %)

# faixa de corte nível basico 9 ano: LP maior ou igual 200 ; MT maior ou igual 225

LP_9EF_2021 = svyby(
  formula = as.formula(~PROFICIENCIA_LP_SAEB >= 200),
  by = as.formula(~SEXO+RACA),
  design = design,
  FUN = svymean,
  na.rm = TRUE,
  estimate.only = TRUE
)

MT_9EF_2021 = svyby(
  formula = as.formula(~PROFICIENCIA_MT_SAEB >= 225),
  by = as.formula(~SEXO+RACA),
  design = design,
  FUN = svymean,
  na.rm = TRUE,
  estimate.only = TRUE
)

## 5 ano EF --------------------------------------------------------------

### 1. Leitura de bases -------------------------------------------------------------


db = read.csv2("data/1_raw/saeb/microdados_saeb_2021/DADOS/TS_ALUNO_5EF.csv",
               sep = ",", dec = ".")

### 2. Filtrando a base -------------------------------------------------------------

# checando se o peso de matematica e portugues e o mesmo
# prop.table(table(db$PESO_ALUNO_LP == db$PESO_ALUNO_MT))

# Transformando variavel de sexo e raca

db = db %>%
  rename(SEXO = TX_RESP_Q01, RACA = TX_RESP_Q04) %>%  # Renomeia as variáveis
  mutate(
    SEXO = recode(SEXO,
                  "A" = "Masculino",
                  "B" = "Feminino",
                  .default = "NS/NR",  # Qualquer outra categoria será "NS/NR"
                  .missing = "NS/NR"), # NA será "NS/NR"
    RACA = recode(RACA,
                  "A" = "Branco(a)",
                  "C" = "Pardo(a)",
                  "B" = "Preto(a)",
                  "D" = "Amarelo(a)",
                  "E" = "Indígena",
                  "F" = "NS/NR",  # "F" será recodificado como "NS/NR"
                  .default = "NS/NR",  # Qualquer outra categoria será "NS/NR"
                  .missing = "NS/NR")  # NA será "NS/NR"
  )


db_filt = db[(
  is.na(db$PESO_ALUNO_LP) == FALSE &
    (db$IN_PROFICIENCIA_LP == 1 |
       db$IN_PROFICIENCIA_MT == 1) &
    db$IN_PUBLICA == 1
),]


design = svydesign(id = ~ID_ALUNO, 
                    strata = ~ESTRATO, 
                    weights = ~PESO_ALUNO_LP, 
                    data = db_filt, 
                    nest = TRUE)




### 3. Calculo --------------------------------------------------------------- 

# Percentual de estudantes do 5º ano com proficiência básica ou adequada em matemática e em língua portuguesa - Rede Pública de Ensino (em %)

# faixa de corte nível basico 5 ano: LP maior ou igual 150 ; MT maior ou igual 175

LP_5EF_2021 = svyby(
  formula = as.formula(~PROFICIENCIA_LP_SAEB >= 150),
  by = as.formula(~SEXO+RACA),
  design = design,
  FUN = svymean,
  na.rm = TRUE,
  estimate.only = TRUE
)

MT_5EF_2021 = svyby(
  formula = as.formula(~PROFICIENCIA_MT_SAEB >= 175),
  by = as.formula(~SEXO+RACA),
  design = design,
  FUN = svymean,
  na.rm = TRUE,
  estimate.only = TRUE
)

## 3 ano EM ---------------------------------------------------------------------

### 1. Leitura de bases -------------------------------------------------------------


db = read.csv2("data/1_raw/saeb/microdados_saeb_2021/DADOS/TS_ALUNO_34EM.csv",
               sep = ",", dec = ".")

### 2. Filtrando a base -------------------------------------------------------------

# checando se o peso de matematica e portugues e o mesmo
# prop.table(table(db$PESO_ALUNO_LP == db$PESO_ALUNO_MT))

# Transformando variavel de sexo e raca

db = db %>%
  rename(SEXO = TX_RESP_Q01, RACA = TX_RESP_Q04) %>%  # Renomeia as variáveis
  mutate(
    SEXO = recode(SEXO,
                  "A" = "Masculino",
                  "B" = "Feminino",
                  .default = "NS/NR",  # Qualquer outra categoria será "NS/NR"
                  .missing = "NS/NR"), # NA será "NS/NR"
    RACA = recode(RACA,
                  "A" = "Branco(a)",
                  "C" = "Pardo(a)",
                  "B" = "Preto(a)",
                  "D" = "Amarelo(a)",
                  "E" = "Indígena",
                  "F" = "NS/NR",  # "F" será recodificado como "NS/NR"
                  .default = "NS/NR",  # Qualquer outra categoria será "NS/NR"
                  .missing = "NS/NR")  # NA será "NS/NR"
  )


db_filt = db[(
  is.na(db$PESO_ALUNO_LP) == FALSE &
    (db$IN_PROFICIENCIA_LP == 1 |
       db$IN_PROFICIENCIA_MT == 1) &
    db$IN_PUBLICA == 1
),]


design = svydesign(id = ~ID_ALUNO, 
                    strata = ~ESTRATO, 
                    weights = ~PESO_ALUNO_LP, 
                    data = db_filt, 
                    nest = TRUE)



### 3. Calculo --------------------------------------------------------------- 

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
