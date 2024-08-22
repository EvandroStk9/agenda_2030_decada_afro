
# 1. Packages ---------------------------------------------------------------------

#
#options(survey.lonely.psu = "adjust")

#
pacotes = c("dplyr",
            "purrr",
            "furrr",
            "tibble",
            "stringr",
            "tidyr",
            "readr",
            "writexl",
            "here",
            "survey",
            "tictoc")

#
lapply(pacotes, library, character.only = TRUE)

#
plan(multisession, workers = parallel::detectCores())

# 2. Leitura de bases -----------------------------------------------------
list_db <- read_rds(
  here("data", "2_clean", "ods_saeb_clean.rds")
)

# 3. Parâmetros  ---------------------------------------------------------------------

#
list_subgroups <- list(
  c("ANO"),
  c("ANO","SEXO"),
  c("ANO","RACA"),
  c("ANO","SEXO","RACA")
)

#
list_expr <- map(list_subgroups, ~paste0("interaction(", paste(.x, collapse = ", "), ")"))


# 3. Função ---------------------------------------------------------------


#
get_ind_4_5_1_3 <- function(db) {
  future_map2(
    list_subgroups,
    list_expr,
    ~svyby(
      formula = ~N_PROFICIENCIA,
      by = as.formula(paste0("~", .y)),
      design = db,
      FUN = svymean,
      vartype = "ci",
      na.rm = TRUE) %>%
      as.data.frame() %>%
      separate_wider_delim(.y, delim = ".", names = .x)
  ) %>%
    bind_rows()
}

# 4. Estimativa -----------------------------------------------------------


# Amostra de 1% -> 62.998 sec elapsed (1 min 3 sec)
# Esperado para 100%: 17 hrs 29 min 58 sec
tic()
ind_4_5_1_3 <- future_map_dfr(
  list_db,
  ~get_ind_4_5_1_3(db = .x)
) %>%
  mutate(across(where(is_character), 
                ~replace_na(.x, "Total"))
  )
toc()

#
plan(sequential)


# 5. Exporta --------------------------------------------------------------

# Cria lista com objetos do environment 
list_outputs <- mget(ls(.GlobalEnv, pattern = "^ind_.*"))

# Cria pastas nomeadas de acordo com os nomes dos objetos
names(list_outputs) %>%
  map(~fs::dir_create(here("outputs", "mvp", .x)))

# Cria arquivos em formato planilha/.xlsx
list_outputs %>%
  map2(names(.),
       ~writexl::write_xlsx(.x,
                            here("outputs", "mvp", .y,
                                 paste0(.y, ".xlsx")))
  )



