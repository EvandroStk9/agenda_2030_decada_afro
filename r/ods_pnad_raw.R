
# 1. Packages ---------------------------------------------------------------------
pacotes = c("PNADcIBGE",
            "purrr",
            "fs",
            "here")


lapply(pacotes, library, character.only = T)

rm(pacotes)


# 2. Leitura de bases -------------------------------------------------------------

# db <- get_pnadc(
#   year=2023,
#   quarter = 3,
#   deflator = TRUE,
#   defyear = 2017,
#   defperiod = 1,
#   design = TRUE
# )

pnad_1_2015 <- get_pnadc(
      year = 2015,
      interview = 1,
      deflator = TRUE,
      defyear = 2017,
      labels = TRUE,
      # defperiod = 1,
      design = TRUE
    )

#
file.remove(tempfile())

#
pnad_1_2022 <- get_pnadc(
  year = 2022,
  interview = 1,
  deflator = TRUE,
  defyear = 2017,
  labels = TRUE,
  # defperiod = 1,
  design = TRUE
)

#
file.remove(tempfile())


# #
# pnad_5_2015 <- get_pnadc(
#   year = 2015,
#   interview = 5,
#   deflator = TRUE,
#   defyear = 2017,
#   labels = TRUE,
#   # defperiod = 1,
#   design = TRUE
# )
# 
# #
# file.remove(tempfile())
# 
# #
# pnad_5_2022 <- get_pnadc(
#   year = 2022,
#   interview = 5,
#   deflator = TRUE,
#   defyear = 2017,
#   labels = TRUE,
#   # defperiod = 1,
#   design = TRUE
# )
# 
# #
# file.remove(tempfile())

# 3. Exporta --------------------------------------------------------------

#
fs::dir_create(here("data", "1_raw"))

#
saveRDS(
  pnad_1_2015,
  here("data", "1_raw", "pnad_1_2015.rds")
)

#
saveRDS(
  pnad_1_2022,
  here("data", "1_raw", "pnad_1_2022.rds")
)




