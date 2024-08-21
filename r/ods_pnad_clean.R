
# 1. Packages ---------------------------------------------------------------------
packages = c("dplyr",
            "purrr",
            "tibble",
            "stringr",
            "tidyr",
            "readr",
            "fs",
            "here",
            "survey")

#
packages_installed <- data.frame(
  packages = packages,
  installed = packages %in% rownames(installed.packages())
) 

#
if (any(packages %in% packages_installed$installed == FALSE)) {
  install.packages(packages[!packages_installed$installed])
}

#
invisible(lapply(packages, library, character.only = TRUE))

# 2. Leitura de bases -------------------------------------------------------------

list_db_raw <- list(
  read_rds(here("data", "1_raw", "pnad", "pnad_1_2015.rds")),
  read_rds(here("data", "1_raw", "pnad", "pnad_1_2022.rds"))
)

# 3. Parâmetros -----------------------------------------------------------

# PPC R$ 2,33 para US$ 1,00 (2017)
# duvida: atualizar para o valor de 2022 (2,56)? https://data.worldbank.org/indicator/PA.NUS.PRVT.PP
ppc = 2.3273771
ln_pobreza_ext = ppc*3.2
ln_pobreza = ppc*6.85

# 4. Ajutes de base --------------------------------------------------------------- 

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
    ano = Ano,
    sexo = case_when(
      V2007 %in% c("Homem", "Masculino") ~ "Masculino",
      V2007 %in% c("Mulher", "Feminino") ~ "Feminino",
      TRUE ~ V2007
    ),
    local = V1022,
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
      V2010 %in% c("Preta", "Parda") ~ "Preto ou pardo",
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
    ),
    n_es_comp = case_when(
      VD3004 %in% c(
        "Superior completo"
      ) ~ 1,
      VD3004 %in% c(
        "Sem instrução e menos de 1 ano de estudo",
        "Fundamental incompleto ou equivalente",
        "Fundamental completo ou equivalente",
        "Médio incompleto ou equivalente",
        "Médio completo ou equivalente"
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    n_es = case_when(
      VD3004 %in% c(
        "Superior completo",
        "Superior incompleto ou equivalente"
      ) ~ 1,
      VD3004 %in% c(
        "Sem instrução e menos de 1 ano de estudo",
        "Fundamental incompleto ou equivalente",
        "Fundamental completo ou equivalente",
        "Médio incompleto ou equivalente",
        "Médio completo ou equivalente"
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    n_pre_escola = case_when(
      V3003A %in% c(
        "Pré-escola"
      ) ~ 1,
      TRUE ~ 0
    )
  )
)

# 5. Exporta --------------------------------------------------------------

#
fs::dir_create(here("data", "2_clean"))

#
saveRDS(
  list_db,
  here("data", "2_clean", "ods_pnad_clean.rds")
)


