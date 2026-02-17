#' @file test_data_preparation.R
#' @description  Testes unitários para validar as funções do módulo mod_data_preparation.R, garantindo a precisão e confiabilidade das análises.
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

if (file.exists("scripts/data_preparation.R")) {
  source("scripts/data_preparation.R")
} else if (file.exists("../scripts/data_preparation.R")) {
  source("../scripts/data_preparation.R")
} else {
  stop("ERRO: O arquivo 'data_preparation.R' não foi encontrado. Verifique o caminho e o nome do arquivo.")
}

cat("************************************************************************\n")
cat("INICIANDO TESTES UNITÁRIOS - Módulo de Data Preparation\n")
cat("\n")

# Ciclo de Testes para a função dados_carregados()
test_that("dados_carregados retorna estrutura correta", {

    # Criar Mock de dados para teste
    csv_content <- c(
        "id;data_inversa;dia_semana;horario;fase_dia;condicao_metereologica;municipio;uf;veiculos;tipo_acidente;causa_acidente;feridos;feridos_leves;feridos_graves;classificacao_acidente",
        "1;2024-01-01;segunda-feira;12:00;Pleno dia;Ceu Claro;BELO HORIZONTE;MG;2;Colisao;Alcool;1;1;0;Com Vitimas"
    )

    temp_file <- tempfile(fileext = ".csv")
    writeLines(csv_content, temp_file)

    # Teste 1: Verificar se a função retorna uma lista com o data frame
    resultado_lista <- dados_carregados(temp_file)

    # Teste 2: Verificar se a lista contém o data frame e se as colunas estão corretas
    expect_type(resultado_lista, "list")
    expect_true("df" %in% names(resultado_lista))

    # Teste 3: Data Frame
    df_resultado <- resultado_lista$df
    expect_s3_class(df_resultado, "data.frame")

    # Teste 4: Verificar se o data frame contém as colunas esperadas
    expect_equal(as.character(df_resultado$uf[1]), "MG")

    # Teste 5: Verificar se a coluna 'data_inversa' está presente e é do tipo Date
    expect_true("data_inversa" %in% colnames(df_resultado))
    expect_s3_class(df_resultado$data_inversa, "Date")

    # Limpar o arquivo temporário após os testes
    unlink(temp_file)
})

# Ciclo de Testes para a função dados_carregados() - Verificar comportamento com arquivo ausente
test_that("dados_carregados lida com arquivo ausente", {
    expect_error(dados_carregados("caminho/inexistente.csv"), "ERRO: O arquivo caminho/inexistente.csv não foi encontrado. Verifique o caminho e o nome do arquivo.")
})

# Ciclo de Testes para a função dados_carregados() - Verificar comportamento com colunas ausentes
test_that("dados_carregados lida com colunas ausentes", {
    # Criar Mock de dados para teste com colunas faltando
    csv_content_incompleto <- c(
        "id;data_inversa;dia_semana;horario;fase_dia;condicao_metereologica;municipio;uf;veiculos;tipo_acidente;causa_acidente;feridos;feridos_leves",
        "1;2024-01-01;segunda-feira;12:00;Pleno dia;Ceu Claro;BELO HORIZONTE;MG;2;Colisao;Alcool;1;1"
    )

    temp_file_incompleto <- tempfile(fileext = ".csv")
    writeLines(csv_content_incompleto, temp_file_incompleto)

    # Teste 1: Verificar se a função retorna um erro indicando as colunas ausentes
    expect_error(dados_carregados(temp_file_incompleto), "As seguintes colunas estão ausentes no arquivo CSV: feridos_graves")

    # Limpar o arquivo temporário após os testes
    unlink(temp_file_incompleto)
})

# Ciclo de Testes para a função dados_carregados() - Verificar comportamento com colunas extras
test_that("dados_carregados lida com colunas extras", {
    # Criar Mock de dados para teste com colunas extras
    csv_content_extras <- c(
        "id;data_inversa;dia_semana;horario;fase_dia;condicao_metereologica;municipio;uf;veiculos;tipo_acidente;causa_acidente;feridos;feridos_leves;feridos_graves;classificacao_acidente;coluna_extra",
        "1;2024-01-01;segunda-feira;12:00;Pleno dia;Ceu Claro;BELO HORIZONTE;MG;2;Colisao;Alcool;1;1;0;Com Vitimas;Valor Extra"
    )

    temp_file_extras <- tempfile(fileext = ".csv")
    writeLines(csv_content_extras, temp_file_extras)

    resultado_lista_extras <- dados_carregados(temp_file_extras)
    df_resultado_extras <- resultado_lista_extras$df

    # Teste 1: Verificar se a coluna extra não está presente no data frame final
    expect_false("coluna_extra" %in% colnames(df_resultado_extras))
    expect_true("data_inversa" %in% colnames(df_resultado_extras))

    # Limpar o arquivo temporário após os testes
    unlink(temp_file_extras)
})

cat("TESTES UNITÁRIOS FINALIZADOS - Módulo de Data Preparation\n")

# Fim do arquivo test_data_preparation.R
