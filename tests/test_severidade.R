#' @file test_severidade.R
#' @description  Testes unitários para validar as funções do módulo mod_severidade.R, garantindo a precisão e confiabilidade das análises.
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

if (file.exists("scripts/mod_severidade.R")) {
  source("scripts/mod_severidade.R")
} else if (file.exists("../scripts/mod_severidade.R")) {
  source("../scripts/mod_severidade.R")
} else {
  stop("ERRO: O arquivo 'mod_severidade.R' não foi encontrado. Verifique o caminho e o nome do arquivo.")
}

cat("************************************************************************\n")
cat("INICIANDO TESTES UNITÁRIOS - Módulo de Severidade\n")
cat("\n")

# Ciclo de Testes para Calculo Média, Mediana e Classificação
test_that("Cálculo de Média, Mediana e Classificação", {
    # Mock para teste - cenário controlado com 4 acidentes e um total de 8 feridos
    df_teste <- data.frame(
        uf = rep("MG", 4),
        feridos = c(1, 1, 1, 5),      # Total de 8 feridos
        feridos_leves = c(1, 1, 1, 0), # 3 leves
        feridos_graves = c(0, 0, 0, 5),# 1 grave (acidente com 5)
        municipio = "BELO HORIZONTE",  # Adicionado por segurança
        stringsAsFactors = FALSE
    )

    # Execução do módulo de severidade com o mock
    resultado <- analisar_severidade(df_teste)
    expect_s3_class(resultado, "data.frame")

    # Teste 1: Verificar se as colunas de média, mediana e status foram criadas
    expect_true("media_feridos" %in% colnames(resultado))
    expect_true("mediana_feridos" %in% colnames(resultado))
    expect_true("status" %in% colnames(resultado))

    # Teste 2: Validar os cálculos de média, mediana e classificação
    # Média Nacional = Total de feridos / Total de acidentes = 8 / 4 = 2
    expect_true("referencia_media_nacional" %in% colnames(resultado))
    expect_equal(unique(resultado$referencia_media_nacional), 2)

    # Teste 3: Valida a Referência da MEDIANA Nacional (Mediana de 1,1,1,5 = 1)
    expect_true("referencia_mediana_nacional" %in% colnames(resultado))
    expect_equal(unique(resultado$referencia_mediana_nacional), 1)

    # Teste 4: Valida a Classificação (Status)
    expect_true("status" %in% colnames(resultado))
})

cat("TESTES UNITÁRIOS FINALIZADOS - Módulo de Severidade\n")

# Fim do arquivo test_severidade.R
