
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
get_ind_4_1_2_1 <- function(db) {
  map2(
    list_subgroups,
    list_expr,
    ~svyby(
      formula = ~n_ef_comp,
      by = as.formula(paste0("~", .y)),
      design = db,
      FUN = svymean,
      vartype = "ci",
      na.rm = TRUE) %>%
      as.data.frame() %>%
      separate_wider_delim(.y, delim = ".", names = .x)
  )
}


# 4. Estimativa -----------------------------------------------------------


#
tic()
ind_4_1_2_1 <- map_df(
  map(list_db, ~subset(.x, V2009 >= 15 & V2009 <= 17)),
  ~get_ind_4_3_1(db = .x)
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
