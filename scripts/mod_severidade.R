#' @file mod_severidade.R
#' @description Módulo para análise de feridos em acidentes da PRF 2024.
#' Calcula métricas de tendência central (Média e Mediana) para apoiar a tomada de decisão da sobre alocação de recursos de resgate.
#'
#' @title Aula Prática Disciplina Probabilidade e Estatística para Análise de Dados - Unidade I Aula 4
#' @name Manipulação e análise de dados com R
#'
#' @author Cristina de Almeida
#' @version 1.0
#' @date Fevereiro 2026

# --- CONFIGURAÇÃO INICIAL ---
#Configuração para usar R no VS Code
options(OutDec = ",", big.mark = ".") # Exibir "," como separador decimal e "." como separador de milhar
options("repos" = c(CRAN = "https://cloud.r-project.org/"))

#' @description Função para calcular métricas de severidade dos acidentes
#' @param df_severidade Data frame contendo os dados tratados no 'data_preparation.R'
#' @return Data frame com as métricas de severidade calculadas

analisar_severidade <- function(df_severidade) {
  cat("************************************************************************\n")
  cat("MÓDULO SEVERIDADE - Iniciando análises de severidade dos acidentes da PRF 2024...\n")
  cat("\n")

  # Validar se df possui as colunas necessárias para análise de severidade
  colunas_necessarias <- c("feridos", "feridos_leves", "feridos_graves")
  if (!all(colunas_necessarias %in% colnames(df_severidade))) {
    stop("Erro: Colunas de severidade não encontradas no data frame.")
  }

  # --- 1. CALCULO DA REFERENCIA DE SEVERIDADE ---
  # Calcular uma média para servir como referência de severidade
  media_nacional <- mean(df_severidade$feridos, na.rm = TRUE)

  # Calcular a mediana para entender a distribuição dos feridos
  mediana_nacional <- median(df_severidade$feridos, na.rm = TRUE)

  # --- 2. CALCULO COM AS UF COM MAIOR SEVERIDADE ---
  # Agrupa as UFs por média de feridos para identificar quais estados têm maior severidade
  df_result_severidade <- df_severidade |>
    group_by(uf) |>
    summarise(
        media_feridos = mean(feridos, na.rm = TRUE),
        total_feridos = dplyr::n(),
        .groups = "drop"
    ) |>
    mutate(
        # Criar a coluna com status de severidade em relação à nacional
        referencia_nacional = media_nacional,
        status = ifelse(
            media_feridos > referencia_nacional,
            "Acima da Média Nacional",
            "Abaixo da Média Nacional"
        )
    ) |>
    arrange(desc(media_feridos))

  cat("************************************************************************\n")
  cat("Média nacional de feridos é:", round(media_nacional, 3), "\n")
  cat("\n")

  cat("As 5 primeiras UFs com média de feridos maior que a média nacional:\n")
  print(as_tibble(head(df_result_severidade, 5)))
  cat("\n")

  cat("Fim das análises de severidade.\n")
  cat("\n")

  # Retornar o data frame para uso análises gráficas
  return(df_result_severidade)
}

# log
message("Módulo 'mod_severidade.R' carregado com sucesso.")

# Fim do arquivo mod_severidade.R
