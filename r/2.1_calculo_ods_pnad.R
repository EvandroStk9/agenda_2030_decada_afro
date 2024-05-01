
# 1. Packages ---------------------------------------------------------------------
pacotes = c("dplyr",
            "purrr",
            "tibble",
            "stringr",
            "tidyr",
            "readr",
            "xlsx",
            "here",
            "survey",
            "tictoc")

#
lapply(pacotes, library, character.only = T)

# 2. Leitura de bases -------------------------------------------------------------

list_db_raw <- list(
  read_rds(here("data", "1_raw", "pnad_1_2015.rds")),
  read_rds(here("data", "1_raw", "pnad_1_2022.rds"))
)

# PPC R$ 2,33 para US$ 1,00 (2017)
# duvida: atualizar para o valor de 2022 (2,56)? https://data.worldbank.org/indicator/PA.NUS.PRVT.PP
ppc = 2.3273771
ln_pobreza_ext = ppc*3.2
ln_pobreza = ppc*6.85

# 3. Ajutes de base --------------------------------------------------------------- 

# sexo V2007
# cor/raca V2010
# faixa etaria VD2006
# status de ocupação (IPEA coloca apenas calculo de todos os outros pra pessoas ocupadas somente)
# localização geográfica (urbano/rural) V1022
# território étnico-racial (IPEA usa regiao)

# VD5002- Rendimento (efetivo) domiciliar per capita 
# (exclusive rendimentos em cartão/tíquete transporte ou alimentação) 
# (exclusive o rendimento das pessoas cuja condição na unidade domiciliar era pensionista, 
# empregado doméstico ou parente do empregado doméstico) 


list_db <- map(
  list_db_raw,
  ~update(
    .x,
    VD5002_defl = VD5002*CO2e,
    VD5002_defl_dia = VD5002_defl/30,
    renda_hora_defl = VD5002_defl_dia/24,
    # dummy_ln_pobreza_ext = as.factor(ifelse(VD5002_defl_dia >= ln_pobreza_ext, 0, 1)),
    # dummy_ln_pobreza = as.factor(ifelse(VD5002_defl_dia >= ln_pobreza, 0, 1)),
    n_abaixo_ln_pobreza_ext = case_when(
      VD5002_defl_dia < ln_pobreza_ext ~ 1,
      VD5002_defl_dia >= ln_pobreza_ext ~ 0,
      TRUE ~ NA_real_),
    # n_acima_ln_pobreza_ext = case_when(
    #   VD5002_defl_dia >= ln_pobreza_ext ~ 1,
    #   VD5002_defl_dia < ln_pobreza_ext ~ 0,
    #   TRUE ~ NA_real_),
    n_abaixo_ln_pobreza = case_when(
      VD5002_defl_dia < ln_pobreza ~ 1,
      VD5002_defl_dia >= ln_pobreza ~ 0,
      TRUE ~ NA_real_),
    n_desocupados = case_when(
      VD4002 == "Pessoas desocupadas" ~ 1,
      VD4002 == "Pessoas ocupadas" ~ 0,
      TRUE ~ NA_real_),
    n_ocupados = case_when(
      VD4002 == "Pessoas desocupadas" ~ 0,
      VD4002 == "Pessoas ocupadas" ~ 1,
      TRUE ~ NA_real_),
    raca = case_when(
      V2010 %in% c("Preta", "Parda") ~ "Negra",
      TRUE ~ V2010), 
    regiao = as.factor(case_when(
      UF %in% c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins") ~ "Norte",
      UF %in% c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", 
                "Pernambuco", "Alagoas", "Sergipe", "Bahia") ~ "Nordeste",
      UF %in% c("Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal") ~ "Centro-Oeste",
      UF %in% c("Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo") ~ "Sudeste",
      UF %in% c("Paraná", "Rio Grande do Sul", "Santa Catarina") ~ "Sul",
      TRUE ~ NA_character_
    )),
    n_ef_comp = case_when(
      VD3004 %in% c(
        "Fundamental completo ou equivalente",
        "Médio incompleto ou equivalente",
        "Médio completo ou equivalente",          
        "Superior incompleto ou equivalente",
        "Superior completo"
      )~ 1,
      VD3004 %in% c(
        "Sem instrução e menos de 1 ano de estudo",
        "Fundamental incompleto ou equivalente"
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    n_em_comp = case_when(
      VD3004 %in% c(
        "Médio completo ou equivalente",          
        "Superior incompleto ou equivalente",
        "Superior completo"
      ) ~ 1,
      VD3004 %in% c(
        "Sem instrução e menos de 1 ano de estudo",
        "Fundamental incompleto ou equivalente",
        "Fundamental completo ou equivalente",
        "Médio incompleto ou equivalente"
      ) ~ 0,
      TRUE ~ NA_real_
    )
  )
)

#
rm(list_db_raw)

# 3.  ---------------------------------------------------------------------


subgroups <- c(
  "Ano","V2007","raca", "V1022"
)

#
expr <- paste0("interaction(", paste(subgroups, collapse = ", "), ")")


# Rendimento médio por hora real das pessoas de 15 anos ou mais de idade ocupadas na semana de referência 
# com rendimento de trabalho, habitualmente recebido em todos os trabalhos, 
# por sexo, grupo de idade, grupamento ocupacional do trabalho principal e existência de deficiência

# 37.073 sec elapsed
# 81.706 sec elapsed
tic()
ind_1_1_1 <- map_df(
  list_db,
  ~svyby(
    formula = ~n_abaixo_ln_pobreza_ext,
    by = as.formula(paste0("~", expr)),
    design = .x,
    FUN = svymean,
    vartype = "ci",
    na.rm = TRUE) %>%
    as.data.frame() %>%
    separate_wider_delim(expr, delim = ".", names = subgroups)
)
toc()


tic()
ind_1_2_1 <- map_df(
  list_db,
  ~svyby(
    formula = ~n_abaixo_ln_pobreza,
    by = as.formula(paste0("~", expr)),
    design = .x
    FUN = svytotal,
    vartype = "ci",
    na.rm = TRUE) %>%
    as.data.frame() %>%
    separate_wider_delim(expr, delim = ".", names = subgroups)
)
toc()

tic()
ind_4_3_1 <- map_df(
  list_db,
  ~svyby(
    formula = ~n_ef_comp,
    by = as.formula(paste0("~", expr)),
    design = subset(.x, V2009 >= 15 & V2009 <= 17),
    FUN = svymean,
    vartype = "ci",
    na.rm = TRUE) %>%
    as.data.frame() %>%
    separate_wider_delim(expr, delim = ".", names = subgroups)
)
toc()


tic()
ind_4_3_2 <- map_df(
  list_db,
  ~svyby(
    formula = ~n_em_comp,
    by = as.formula(paste0("~", expr)),
    design = subset(.x, V2009 >= 18 & V2009 <= 20),
    FUN = svymean,
    vartype = "ci",
    na.rm = TRUE) %>%
    as.data.frame() %>%
    separate_wider_delim(expr, delim = ".", names = subgroups)
)
toc()



# 38.668 sec elapsed
# 79.845 sec elapsed
tic()
ind_8_5_1 <- 
  map_df(
    list_db,
    ~svyby(
      formula = ~renda_hora_defl,
      by = as.formula(paste0("~", expr)),
      design = .x,
      FUN = svymean,
      vartype = "ci",
      na.rm = TRUE) %>%
      as.data.frame() %>%
      separate_wider_delim(expr, delim = ".", names = subgroups)
  )
toc()


# 30.897 sec elapsed
tic()
ind_8_5_2 <- 
  map_df(
    list_db,
    ~svyby(
      formula = ~n_desocupados + n_ocupados,
      by = as.formula(paste0("~", expr)),
      design = .x,
      FUN = svytotal,
      na.rm = TRUE) %>%
      as.data.frame() %>%
      separate_wider_delim(expr, delim = ".", names = subgroups) %>%
      mutate(tx_desocupacao = n_desocupados/(n_desocupados + n_ocupados))
  )
toc()


# 4.  ---------------------------------------------------------------------

# 
list_outputs <- mget(ls(.GlobalEnv))

#
names(list_outputs) %>%
  map(~fs::dir_create(here("outputs", "mvp", .x)))

#
mget(ls(list_outputs, pattern = "^ind_.*")) %>%
  map2(names(.),
       ~writexl::write_xlsx(.x,
                            here("outputs", "mvp", .y,
                                 paste0(.y, ".xlsx")))
  )
