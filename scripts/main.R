#' @file main.R
#' @description Script principal para executar a análise exploratória e probabilidade dos acidentes da PRF 2024
#'
#' @title Aula Prática Disciplina Probabilidade e Estatística para Análise de Dados - Unidade I Aula 4
#' @name Manipulação e análise de dados com R
#'
#' @author Cristina de Almeida
#' @version 1.0
#' @date Fevereiro 2026

# --- CONFIGURAÇÃO INICIAL ---
#Configuração para usar R no VS Code
options(radian.enabled = TRUE)
options(OutDec = ",", big.mark = ".") # Exibir "," como separador decimal e "." como separador de milhar
options("repos" = c(CRAN = "https://cloud.r-project.org/"))

# Carregar pacotes necessários
library(testthat)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr) # replace_na() para lidar com valores ausentes

# --- IMPORTAÇÃO DOS MÓDULOS DE ANÁLISE ---
# Carregar o módulo de preparação dos dados
source("scripts/data_preparation.R")

# Carregar o módulo de análise de causalidade
source("scripts/mod_casualidade.R")

# Carregar o módulo de análise geográfica
source("scripts/mod_geografia.R")

# Carregar o módulo de análise de severidade
source("scripts/mod_severidade.R")

# Carregar o módulo de análise de temporalidade
source("scripts/mod_temporal.R")

# --- IMPORTAÇÃO DE MÓDULOS DE TESTES
# WIP
#
#
#

# --- IMPORTAÇÃO DE MÓDULOS DE GRÁFICOS
#  WIP
#
#
#


# --- EXECUÇÃO PRINCIPAL ---

# Validar se o arquivo de dados existe antes de tentar carregar
if (!exists("df_detran_prf2024")) {
  stop("ERRO: O data frame 'df_detran_prf2024' não foi criado. Verifique o módulo data_preparation.R.")
}
# 1. Executar as análises exploratórias e de probabilidade
analises_causalidade <- analisar_casualidade(df_detran_prf2024)
analises_geografia <- analisar_geografia(df_detran_prf2024)
analises_severidade <- analisar_severidade(df_detran_prf2024)
analises_temporalidade <- analisar_temporal(df_detran_prf2024)
# 2. Validar os resultados das análises com testes unitários
# WIP
#
#
#

# --- GERAÇÃO DOS GRÁFICOS DAS ANÁLISES ---
cat("************************************************************************\n")
cat("Iniciando geração dos gráficos...\n")
cat("\n")
# WIP
#
#
#

cat("*===============================  FIM  =================================*\n")
cat("Análises exploratórias e de probabilidade dos acidentes da PRF 2024 concluídas com sucesso!\n")
cat("\n")
cat("Todos os gráficos foram salvos na pasta 'graficos/'.\n")

# Fim do arquivo main.R
