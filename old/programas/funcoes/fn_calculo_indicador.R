#----------------------------------------------------------------------------------------------
# PROJETO: Agenda 2030 e Década Afrodescendente
#
# OBJETIVO: Funcao de automatização de cálculo dos indicadores ODS
#
# PRODUTO: 
#
# AUTOR: Ana Clara
#
# DATA DE CRIACAO: 02/2024
#
# DATA DE MODIFICACAO:
#
# MODIFICACOES: 
#----------------------------------------------------------------------------------

# 1. Packages ---------------------------------------------------------------------
pacotes = c("dplyr", 
            "survey")


lapply(pacotes, library, character.only = T)

rm(pacotes)

# 2. Funcao -----------------------------------------------------------------------

# Versao anterior

# run_analysis <- function(data, variable, indicador, subgroups, target_variable, levels_to_exclude) {
#   if (length(levels_to_exclude) == 0) {
#     levels_to_run <- levels(data$variables[[variable]])
#   } else {
#     levels_to_run <- setdiff(levels(data$variables[[variable]]), levels_to_exclude)
#   }
#   
#   results <- list()
#   
#   for (level in levels_to_run) {
#     table_name <- paste0("ind_", indicador, "_", tolower(level))
#     combined_table <- NULL
#     
#     for (subgroup in subgroups) {
#       subset_data <- subset(data, data$variables[[variable]] == level)
#       
#       result <- svyby(
#         formula = as.formula(paste0("~", target_variable)),
#         by = as.formula(paste0("~", subgroup)),
#         design = subset_data,
#         FUN = svymean,
#         na.rm = TRUE,
#         estimate.only = TRUE
#       )
#       
#       result <- result[, c(1, 3, 5)]
#       colnames(result) <- names_col
#       
#       if (is.null(combined_table)) {
#         combined_table <- result
#       } else {
#         combined_table <- rbind(combined_table, result)
#       }
#     }
#     
#     results[[table_name]] <- combined_table
#   }
#   
#   
#   return(results)
# }
# 
# # Exemplo de uso
# # names_col <- c("subgrupo", "percentual_indicador", "erro_padrao")
# # subgroups <- c("Ano", "regiao", "V2007", "VD2006", "V1022", "VD4002")
# # results_ind_1.1.1 <- run_analysis(
# #   data = db, # base no formato svydesign
# #   variable = "V2010", # variavel de cor/raca que vai ser a desagregação maior
# #   indicador = "1.1.1", # nome/num do indicador
# #   subgroups = subgroups, # outras varaveis de desagregação menor (dentro de cor/raca)
# #   target_variable = "dummy_ln_pobreza_ext", # variavel que estamos calculando o percentual (dummy em que 1 = proporção que queremos encontrar)
# #   levels_to_exclude = c("Ignorado") # se tiver algum level que em "variable" que queremos ignorar, se não levels_to_exclude = NULL
# # )

# Versao atualizada

# run_analysis <- function(data, variable, indicador, subgroups, target_variable, levels_to_exclude) {
#   if (length(levels_to_exclude) == 0) {
#     levels_to_run <- levels(data$variables[[variable]])
#   } else {
#     levels_to_run <- setdiff(levels(data$variables[[variable]]), levels_to_exclude)
#   }
#   
#   all_results <- list()
#   
#   for (level in levels_to_run) {
#     level_name <- tolower(level)
#     
#     level_results <- list()
#     
#     for (subgroup in subgroups) {
#       subset_data <- subset(data, data$variables[[variable]] == level)
#       
#       result <- svyby(
#         formula = as.formula(paste0("~", target_variable)),
#         by = as.formula(paste0("~", subgroup)),
#         design = subset_data,
#         FUN = svymean,
#         na.rm = TRUE,
#         estimate.only = TRUE
#       )
#       
#       result <- result[, c(1, 3)]
#       colnames(result) <- c("subgrupo", level_name)
#       
#       # Alterar valor na primeira linha e primeira coluna para "Brasil" quando subgrupo for igual a "Ano"
#       if (subgroup == "Ano") {
#         result[1, 1] <- "Brasil"
#       }
#       
#       level_results[[subgroup]] <- result
#     }
#     
#     combined_results <- do.call(rbind, level_results)
#     all_results[[level_name]] <- combined_results
#   }
#   
#   # Usar o primeiro data frame como base
#   final_result <- all_results[[tolower(levels_to_run[1])]]
#   
#   # Adicionar colunas dos outros níveis (pegando apenas a segunda coluna)
#   for (i in 2:length(levels_to_run)) {
#     final_result <- cbind(final_result, all_results[[tolower(levels_to_run[i])]][, 2])
#   }
#   
#   # Renomear colunas
#   colnames(final_result)[-1] <- tolower(levels_to_run)
#   
#   rownames(final_result) <- NULL
#   
#   return(final_result)
# }

# Exemplo de uso
# subgroups <- c("Ano", "regiao", "V2007", "VD2006", "V1022", "VD4002")
# result <- run_analysis(
#   data = db, # base no formato svydesign
#   variable = "V2010", # variavel de cor/raca que vai ser a desagregação maior
#   indicador = "1.1.1", # nome/num do indicador
#   subgroups = subgroups, # outras varaveis de desagregação menor (dentro de cor/raca)
#   target_variable = "dummy_ln_pobreza_ext", # variavel que estamos calculando o percentual (dummy em que 1 = proporção que queremos encontrar)
#   levels_to_exclude = c("Ignorado") # se tiver algum level que em "variable" que queremos ignorar, se não levels_to_exclude = NULL
# )

run_analysis <- function(data, variable, indicador, subgroups, target_variable, levels_to_exclude) {
  if (length(levels_to_exclude) == 0) {
    levels_to_run <- levels(data$variables[[variable]])
  } else {
    levels_to_run <- setdiff(levels(data$variables[[variable]]), levels_to_exclude)
  }
  
  all_results <- list()
  
  # Calcular o total para cada subgrupo
  total_results <- list()
  
  for (subgroup in subgroups) {
    total_result <- svyby(
      formula = as.formula(paste0("~", target_variable)),
      by = as.formula(paste0("~", subgroup)),
      design = data,
      FUN = svymean,
      na.rm = TRUE,
      estimate.only = TRUE
    )
    
    total_result <- total_result[, c(1, 3)]
    colnames(total_result) <- c("subgrupo", "total")
    
    total_results[[subgroup]] <- total_result
  }
  
  combined_total_results <- do.call(rbind, total_results)
  all_results[["total"]] <- combined_total_results
  
  for (level in levels_to_run) {
    level_name <- tolower(level)
    
    level_results <- list()
    
    for (subgroup in subgroups) {
      subset_data <- subset(data, data$variables[[variable]] == level)
      
      result <- svyby(
        formula = as.formula(paste0("~", target_variable)),
        by = as.formula(paste0("~", subgroup)),
        design = subset_data,
        FUN = svymean,
        na.rm = TRUE,
        estimate.only = TRUE
      )
      
      result <- result[, c(1, 3)]
      colnames(result) <- c("subgrupo", level_name)
      
      # Alterar valor na primeira linha e primeira coluna para "Brasil" quando subgrupo for igual a "Ano"
      if (subgroup == "Ano") {
        result[1, 1] <- "Brasil"
      }
      
      level_results[[subgroup]] <- result
    }
    
    combined_results <- do.call(rbind, level_results) # Pegar apenas a segunda coluna
    all_results[[level_name]] <- combined_results
  }
  
  # Usar o total como base
  final_result <- all_results[["total"]]
  
  # Adicionar colunas dos outros níveis (pegando apenas a segunda coluna)
  for (i in 1:length(levels_to_run)) {
    final_result <- cbind(final_result, all_results[[tolower(levels_to_run[i])]][, 2])
  }
  
  # Renomear colunas
  colnames(final_result)[-1] <- c("total", tolower(levels_to_run))
  
  rownames(final_result) <- NULL
  
  return(final_result)
}
