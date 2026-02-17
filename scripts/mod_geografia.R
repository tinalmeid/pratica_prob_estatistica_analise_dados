#' @file mod_geografia.R
#' @description Módulo para análises os pontos críticos de acidentes da PRF 2024
#' Identifica os municípios com maior número de acidentes para fins preventivos ou planejamento de ações de segurança viária.
#'
#' @title Aula Prática Disciplina Probabilidade e Estatística para Análise de Dados - Unidade I Aula 4
#' @name Manipulação e análise de dados com R
#'
#' @author Cristina de Almeida
#' @version 1.0
#' @date Fevereiro 2026

# --- CONFIGURAÇÃO INICIAL ---
options(radian.enabled = TRUE) # Configurar para usar R no VS Code
options(OutDec = ",", big.mark = ".") # Exibir "," como separador decimal e "." como separador de milhar
options("repos" = c(CRAN = "https://cloud.r-project.org/")) # Definir o repositório padrão do CRAN para instalação de pacotes

#' @description Função para identificar os municípios com maior número de acidentes
#' @param df_geografia Data frame contendo os dados tratados no 'data_preparation.R'
#' @return Data frame métricas da geografia dos acidentes calculadas

analisar_geografia <- function(df_geografia) {
  cat("************************************************************************\n")
  cat("MÓDULO GEOGRAFIA - Iniciando análises geográfica dos acidentes da PRF 2024...\n")
  cat("\n")

  # Validar se df possui as colunas necessárias para análise de geografia
  colunas_necessarias <- c("municipio", "uf")
  if (!all(colunas_necessarias %in% colnames(df_geografia))) {
    stop("Erro: Colunas geográficas não encontradas no data frame.")
  }

  # --- 1. CALCULO UF COM MAIOR NÚMERO DE ACIDENTES ---
  df_geografia_uf <- df_geografia |>
    group_by(uf, municipio) |>
    summarise(
      total_acidentes = dplyr::n(),
      .groups = "drop"
    ) |>
    arrange(desc(total_acidentes)) |>
    mutate(
      # Criar um ranking para identificar os municípios mais críticos
      ranking = row_number()
    )

  cat("========================================================================\n")
  cat("As 5 primeiras cidades com maior número de acidentes:\n")
  print(as_tibble(head(df_geografia_uf, 5)))
  cat("\n")

  cat("Fim das análises geográficas.\n")

  # Retornar o data frame para uso análises gráficas
  return(df_geografia_uf)
}

# log
message("Módulo 'mod_geografia.R' carregado com sucesso.")

# Fim do arquivo mod_geografia.R
