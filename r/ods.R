
# 3.  ---------------------------------------------------------------------


subgroups <- c(
  "ano","V2007","raca", "V1022"
)

list_subgroups <- list(
  c("ano"),
  c("ano","sexo"),
  c("ano","V2007","raca"),
  c("ano","V2007","raca", "V1022")
)

#
expr <- paste0("interaction(", paste(subgroups, collapse = ", "), ")")
list_expr <- map(list_subgroups, ~paste0("interaction(", paste(.x, collapse = ", "), ")"))


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
    design = .x,
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


# Testes ------------------------------------------------------------------

