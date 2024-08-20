
# 1. Packages ---------------------------------------------------------------------
pacotes = c("dplyr",
            "purrr",
            "tibble",
            "stringr",
            "tidyr",
            "readr",
            "writexl",
            "here",
            "survey",
            "tictoc")

#
lapply(pacotes, library, character.only = T)

# 2. Leitura de bases -------------------------------------------------------------

list_db <- read_rds(
  here("data", "2_clean", "ods_pnad_clean.rds")
)

# 3. Parâmetros  ---------------------------------------------------------------------

#
list_subgroups <- list(
  c("ano"),
  c("ano","sexo"),
  c("ano","raca"),
  c("ano","local"),
  c("ano","sexo","raca"),
  c("ano","sexo","local"),
  c("ano","raca","local"),
  c("ano","sexo","raca","local")
)

#
list_expr <- map(list_subgroups, ~paste0("interaction(", paste(.x, collapse = ", "), ")"))


# 3. Função ------------------------------------------------------------------

#
get_ind_8_5_2 <- function(db) {
  map2(
    list_subgroups,
    list_expr,
    ~svyby(
      formula = ~n_desocupados + n_ocupados,
      by = as.formula(paste0("~", .y)),
      design = db,
      FUN = svytotal,
      na.rm = TRUE) %>%
      as.data.frame() %>%
      separate_wider_delim(.y, delim = ".", names = .x)  %>%
      mutate(tx_desocupacao = n_desocupados/(n_desocupados + n_ocupados))
  )
}

# 4. Estimativa -----------------------------------------------------------

#
tic()
ind_8_5_2 <- map_df(
  list_db,
  ~get_ind_8_5_2(db = .x)
) %>%
  mutate(across(where(is_character), 
                ~replace_na(.x, "Total"))
  )
toc()

# 5. Exporta -------------------------------------------------------------------

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
