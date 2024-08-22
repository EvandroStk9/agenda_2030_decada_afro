
# 1. Pacotes --------------------------------------------------------------

pacotes = c("dplyr",
            "here",
            "tidylog",
            "purrr",
            "readr",
            "survey")

lapply(pacotes, library, character.only = T)


# 2. Leitura de bases -----------------------------------------------------


## 3 ano EM ---------------------------------------------------------------------

db_2015_raw <- read.csv2(here("data", "1_raw", "saeb", "microdados_saeb_2015", "DADOS", "TS_ALUNO_3EM.csv"),
                         sep = ",", dec = ".")

db_2021_raw <- read.csv2(here("data", "1_raw", "saeb", "microdados_saeb_2021", "DADOS", "TS_ALUNO_34EM.csv"),
                         sep = ";", dec = ".")


# 3. Tratamento -----------------------------------------------------------

# 2015

# Transformando variavel de sexo e raca
db_2015 <- db_2015_raw %>%
  rename(ANO = ID_PROVA_BRASIL, SEXO = TX_RESP_Q001, RACA = TX_RESP_Q002) %>%  # Renomeia as variáveis
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
  ) %>%
  filter(
    IN_PREENCHIMENTO_PROVA == 1 &
      is.na(PESO_ALUNO_LP) == FALSE &
      IN_PROFICIENCIA == 1 &
      IN_PUBLICA == 1
  ) %>%
  transmute(
    N_PROFICIENCIA = if_else(PROFICIENCIA_LP_SAEB >= 250 & PROFICIENCIA_MT_SAEB >= 275, 1, 0),
    ANO,
    SEXO,
    RACA,
    ID_ALUNO,
    ESTRATO_ANEB,
    PESO_ALUNO_LP
  ) %>%
  slice_sample(prop = 0.01) %>%
  group_by(ESTRATO_ANEB) %>%
  filter(
    n() > 1
  ) %>%
  ungroup()

# Criando o design
svy_2015 <- svydesign(id = ~ID_ALUNO, 
                      strata = ~ESTRATO_ANEB, 
                      weights = ~PESO_ALUNO_LP, 
                      data = db_2015, 
                      nest = TRUE)

# 2021 

# Transformando variavel de sexo e raca
db_2021 <- db_2021_raw %>%
  rename(ANO = ID_SAEB, SEXO = TX_RESP_Q01, RACA = TX_RESP_Q04) %>%  # Renomeia as variáveis
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
  ) %>%
  filter(
    is.na(PESO_ALUNO_LP) == FALSE &
      (IN_PROFICIENCIA_LP == 1 |
         IN_PROFICIENCIA_MT == 1) &
      IN_PUBLICA == 1
  ) %>%
  transmute(
    N_PROFICIENCIA = if_else(PROFICIENCIA_LP_SAEB >= 250 & PROFICIENCIA_MT_SAEB >= 275, 1, 0),
    ANO,
    SEXO,
    RACA,
    ID_ALUNO,
    ESTRATO,
    PESO_ALUNO_LP
  ) %>%
  slice_sample(prop = 0.01) %>%
  group_by(ESTRATO) %>%
  filter(
    n() > 1
  ) %>%
  ungroup()

# Criando o design
svy_2021 <- svydesign(id = ~ID_ALUNO, 
                      strata = ~ESTRATO, 
                      weights = ~PESO_ALUNO_LP, 
                      data = db_2021, 
                      nest = TRUE)

list_svy <- list(
  svy_2015 = svy_2015,
  svy_2021 = svy_2021
)


# 4. Exporta --------------------------------------------------------------

#
fs::dir_create(here("data", "2_clean"))

#
saveRDS(
  list_svy,
  here("data", "2_clean", "ods_saeb_clean.rds")
)
