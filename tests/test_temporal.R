#' @file test_temporal.R
#' @description  Testes unitários para validar as funções do módulo mod_temporal.R, garantindo a precisão e confiabilidade das análises.
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

MODO_TESTE <- TRUE

# --- CARREGAMENTO DO MÓDULO DE PREPARAÇÃO DE DADOS ---
library(testthat)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

if (file.exists("scripts/mod_temporal.R")) {
  source("scripts/mod_temporal.R")
} else if (file.exists("../scripts/mod_temporal.R")) {
  source("../scripts/mod_temporal.R")
} else {
  stop("ERRO: O arquivo 'mod_temporal.R' não foi encontrado. Verifique o caminho e o nome do arquivo.")
}

cat("************************************************************************\n")
cat("INICIANDO TESTES UNITÁRIOS - Módulo de Temporal\n")
cat("\n")

# Ciclo de Testes para Probabilidade e Categorização Temporal

test_that("Cálculo de Probabilidades e Categorização Temporal", {

    # Criar Mock de dados para teste - 10 acidentes distribuídos em diferentes horários e dias
    # Cenário 1: Madrugada de Fim de Semana (2 casos = 20%)
    # Cenário 2: Manhã de Dia Útil (3 casos = 30%)
    # Cenário 3: Noite de Dia Útil (5 casos = 50%)
    df_teste <- data.frame(
        # Horários formatados HH:MM
        horario = c(
            rep("02:00", 2), # 02h = Madrugada
            rep("08:00", 3), # 08h = Manhã
            rep("20:00", 5)  # 20h = Noite
    ),
        # Dias da semana (Texto exato como seu script espera)
        dia_semana = c(
            rep("sábado", 2),         # Fim de Semana
            rep("segunda-feira", 3),  # Dia Útil
            rep("quarta-feira", 5)    # Dia Útil
    ),
        # Data inversa (apenas para contrato, não usada no cálculo direto)
        data_inversa = rep("2024-01-01", 10),
        stringsAsFactors = FALSE
  )

  # Executar a função do módulo temporal com o mock
  resultado_prob <- analisar_temporal(df_teste)

  # Teste 1: Verifica se criou as colunas de agrupamento corretamente
  expect_true("periodo_dia" %in% colnames(resultado_prob))
  expect_true("ciclo_12h" %in% colnames(resultado_prob))

  # Teste 2: Madrugada FDS (2 acidentes em 10) -> Deve ser 20%
  grupo_madrugada <- resultado_prob |>
    filter(periodo_dia == "Madrugada" & eh_fim_semana == "Fim de Semana")
  expect_equal(nrow(grupo_madrugada), 1)
  expect_equal(grupo_madrugada$probabilidade, 20)

  # Teste 3: Manhã Dia Útil (3 acidentes em 10) -> Deve ser 30%
  grupo_manha <- resultado_prob |>
    filter(periodo_dia == "Manhã" & eh_fim_semana == "Dia Útil")
  expect_equal(grupo_manha$probabilidade, 30)

  # Teste 4: Noite Dia Útil (5 acidentes em 10) -> Deve ser 50%
  grupo_noite <- resultado_prob |>
    filter(periodo_dia == "Noite" & eh_fim_semana == "Dia Útil")
  expect_equal(grupo_noite$probabilidade, 50)

  # Teste 5: Verificar se a soma das probabilidades é igual a 100%
  expect_equal(sum(resultado_prob$probabilidade), 100)
})

cat("TESTES UNITÁRIOS FINALIZADOS - Módulo de Temporal\n")

# Fim do arquivo test_temporal.R
