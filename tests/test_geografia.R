#' @file test_geografia.R
#' @description  Testes unitários para validar as funções do módulo mod_geografia.R, garantindo a precisão e confiabilidade das análises.
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

if (file.exists("scripts/mod_geografia.R")) {
  source("scripts/mod_geografia.R")
} else if (file.exists("../scripts/mod_geografia.R")) {
  source("../scripts/mod_geografia.R")
} else {
  stop("ERRO: O arquivo 'mod_geografia.R' não foi encontrado. Verifique o caminho e o nome do arquivo.")
}

cat("************************************************************************\n")
cat("INICIANDO TESTES UNITÁRIOS - Módulo de Geografia\n")
cat("\n")

# Ciclo de Testes para a função analisar_geografia()
test_that("analisar_geografia retorna estrutura correta", {

    # Criar Mock de dados para teste
    df_teste <- data.frame(
        municipio = c("BELO HORIZONTE", "CONTAGEM", "BETIM"),
        uf = c("MG", "MG", "MG"),
        stringsAsFactors = FALSE
    )

    # Executar a função de análise geográfica
    resultado_geografia <- analisar_geografia(df_teste)

    # Teste 1: Verificar se o retorno é um data.frame
    expect_s3_class(resultado_geografia, "data.frame")

    # Teste 2: Verificar se as colunas esperadas estão presentes
    expect_true(all(c("uf", "municipio", "total_acidentes", "ranking") %in% colnames(resultado_geografia)))
})

# Ciclo de Testes para Verificar Ranking de municípios por total de acidentes
test_that("Ranking de municípios é calculado corretamente", {
    # Criar Mock de dados para teste
    df_teste <- data.frame(
        municipio = c(
            rep("BELO HORIZONTE", 3), # 3 Acidentes (Será 1º)
            rep("CONTAGEM", 2),       # 2 Acidentes (Será 2º)
            rep("BETIM", 1)           # 1 Acidente  (Será 3º)
    ),
    uf = "MG",
    stringsAsFactors = FALSE
    )

    # Executar a função de análise geográfica
    resultado_geografia <- analisar_geografia(df_teste)

    # Teste 1: Verificar se o ranking respeitou a quantidade de linhas (Betim 1, Contagem 2, Belo Horizonte 3)
    expect_equal(resultado_geografia$ranking[resultado_geografia$municipio == "BELO HORIZONTE"], 1)
    expect_equal(resultado_geografia$ranking[resultado_geografia$municipio == "CONTAGEM"], 2)
    expect_equal(resultado_geografia$ranking[resultado_geografia$municipio == "BETIM"], 3)
})

# Ciclo de Testes para Calcula de total de acidentes por UF
test_that("Soma de acidentes por município é calculada corretamente", {
    # Criar Mock de dados para teste (BH: 3 acidentes, Contagem: 2 acidentes, Betim: 1 acidente, Total Geral: 6 acidentes)
    df_teste <- data.frame(
        municipio = c(
            rep("BELO HORIZONTE", 3),
            rep("CONTAGEM", 2),
            rep("BETIM", 1)
        ),
        uf = "MG", # Adicionamos UF pois o group_by usa ela
        stringsAsFactors = FALSE
    )

    # Executar a função de análise geográfica
    resultado_geografia <- analisar_geografia(df_teste)

    # Teste 1: Verificar se a soma absoluta de Belo Horizonte bate (3)
    total_bh <- resultado_geografia$total_acidentes[resultado_geografia$municipio == "BELO HORIZONTE"]
    expect_equal(total_bh, 3)

    # Teste 2: Verifica se a soma absoluta de Contagem bate (2)
    total_contagem <- resultado_geografia$total_acidentes[resultado_geografia$municipio == "CONTAGEM"]
    expect_equal(total_contagem, 2)

    # Teste 3: Verificar se a soma absoluta de Betim bate (1)
    total_betim <- resultado_geografia$total_acidentes[resultado_geografia$municipio == "BETIM"]
    expect_equal(total_betim, 1)

    # Teste 4: Verificar se a soma total de acidentes bate com o número de linhas do data frame (6)
    expect_equal(sum(resultado_geografia$total_acidentes), nrow(df_teste))
})

cat("TESTES UNITÁRIOS FINALIZADOS - Módulo de Geografia\n")

# Fim do arquivo test_geografia.R

