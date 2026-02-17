#' @file test_casualidade.R
#' @description  Testes unitários para validar as funções do módulo mod_casualidade.R, garantindo a precisão e confiabilidade das análises.
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

if (file.exists("scripts/mod_casualidade.R")) {
  source("scripts/mod_casualidade.R")
} else if (file.exists("../scripts/mod_casualidade.R")) {
  source("../scripts/mod_casualidade.R")
} else {
  stop("ERRO: O arquivo 'mod_casualidade.R' não foi encontrado. Verifique o caminho e o nome do arquivo.")
}

cat("************************************************************************\n")
cat("INICIANDO TESTES UNITÁRIOS - Módulo de Casualidade\n")
cat("\n")

# Ciclo de Testes para a função analisar_casualidade()
test_that("analisar_casualidade retorna estrutura correta", {

    # Criar Mock de dados para teste
    csv_content <- c(
        "id;data_inversa;dia_semana;horario;fase_dia;condicao_metereologica;municipio;uf;veiculos;tipo_acidente;causa_acidente;feridos;feridos_leves;feridos_graves;classificacao_acidente",
        "1;2024-01-01;segunda-feira;12:00;Pleno dia;Ceu Claro;BELO HORIZONTE;MG;2;Colisao;Alcool;1;1;0;Com Vitimas"
    )

    temp_file <- tempfile(fileext = ".csv")
    writeLines(csv_content, temp_file)

    # Carregar o arquivo CSV em um data.frame
    df_teste <- read.csv(temp_file, sep = ";")

    # Teste 1: Verificar se a função retorna uma lista
    resultado_causalidade <- analisar_casualidade(df_teste)
    expect_type(resultado_causalidade, "list")

    # Teste 2: Verificar se cada elemento da lista é um data frame
    expect_s3_class(resultado_causalidade$causa_acidente, "data.frame")
    expect_s3_class(resultado_causalidade$tipo_acidente, "data.frame")
    expect_s3_class(resultado_causalidade$condicao_metereologica, "data.frame")

    # Teste 3: Verificar se os data frames contêm as colunas esperadas
    expect_true(all(c("causa_acidente", "frequencia_absoluta", "frequencia_relativa") %in% colnames(resultado_causalidade$causa_acidente)))
    expect_true(all(c("tipo_acidente", "frequencia_absoluta", "frequencia_relativa") %in% colnames(resultado_causalidade$tipo_acidente)))
    expect_true(all(c("condicao_metereologica", "frequencia_absoluta", "probabilidade") %in% colnames(resultado_causalidade$condicao_metereologica)))

    # Limpar o arquivo temporário após os testes
    unlink(temp_file)

})

# Ciclo de Testes Calculo da Moda - Verificar se a função identifica corretamente a moda da causa de acidente
test_that("analisar_casualidade identifica corretamente a moda da causa de acidente", {
    # Criar Mock de dados para teste
    csv_content <- c(
        "id;data_inversa;dia_semana;horario;fase_dia;condicao_metereologica;municipio;uf;veiculos;tipo_acidente;causa_acidente;feridos;feridos_leves;feridos_graves;classificacao_acidente",
        "1;2024-01-01;segunda-feira;12:00;Pleno dia;Ceu Claro;BELO HORIZONTE;MG;2;Colisao;Alcool;1;1;0;Com Vitimas",
        "2;2024-01-02;terça-feira;13:00;Pleno dia;Chuva Forte;BELO HORIZONTE;MG;3;Colisao;Alcool;2;2;0;Com Vitimas",
        "3;2024-01-03;quarta-feira;14:00;Pleno dia;Ceu Claro;BELO HORIZONTE;MG;1;Colisao;;0;0;0;"
    )

    temp_file <- tempfile(fileext = ".csv")
    writeLines(csv_content, temp_file)

    # Carregar o arquivo CSV em um data.frame
    df_teste <- read.csv(temp_file, sep = ";")

    # Executar a função de análise de causalidade
    resultado_causalidade <- analisar_casualidade(df_teste)

    # Teste 1: Verificar se a moda da causa de acidente é "Alcool"
    expect_equal(resultado_causalidade$causa_acidente$causa_acidente[1], "Alcool")

    # Limpar o arquivo temporário após os testes
    unlink(temp_file)

})

# Ciclo de Testes Calculo da Moda - Verificar se a função identifica corretamente a moda do tipo de acidente
test_that("analisar_casualidade identifica corretamente a moda do tipo de acidente", {
    # Criar Mock de dados para teste
    csv_content <- c(
        "id;data_inversa;dia_semana;horario;fase_dia;condicao_metereologica;municipio;uf;veiculos;tipo_acidente;causa_acidente;feridos;feridos_leves;feridos_graves;classificacao_acidente",
        "1;2024-01-01;segunda-feira;12:00;Pleno dia;Ceu Claro;BELO HORIZONTE;MG;2;Colisao;Alcool;1;1;0;Com Vitimas",
        "2;2024-01-02;terça-feira;13:00;Pleno dia;Chuva Forte;BELO HORIZONTE;MG;3;Colisao;;2;2;0;",
        "3;2024-01-03;quarta-feira;14:00;Pleno dia;Ceu Claro;BELO HORIZONTE;MG;1;;Alcool;;0;"
    )

    temp_file <- tempfile(fileext = ".csv")
    writeLines(csv_content, temp_file)

    # Carregar o arquivo CSV em um data.frame
    df_teste <- read.csv(temp_file, sep = ";")

    # Executar a função de análise de causalidade
    resultado_causalidade <- analisar_casualidade(df_teste)

    # Teste 1: Verificar se a moda do tipo de acidente é "Colisao"
    expect_equal(resultado_causalidade$tipo_acidente$tipo_acidente[1], "Colisao")

    # Limpar o arquivo temporário após os testes
    unlink(temp_file)

})

# Ciclo de Testes Calculo da Probabilidade - Verificar se a função calcula corretamente a probabilidade por condição meteorológica
test_that("analisar_casualidade calcula corretamente a probabilidade por condição meteorológica", {
    # Criar Mock de dados para teste
    csv_content <- c(
        "id;data_inversa;dia_semana;horario;fase_dia;condicao_metereologica;municipio;uf;veiculos;tipo_acidente;causa_acidente;feridos;feridos_leves;feridos_graves;classificacao_acidente",
        "1;2024-01-01;segunda-feira;12:00;Pleno dia;Ceu Claro;BELO HORIZONTE;MG;2;Colisao;Alcool;1;1;0;Com Vitimas",
        "2;2024-01-02;terça-feira;13:00;Pleno dia;Chuva Forte;BELO HORIZONTE;MG;3;;2;2;0;",
        "3;2024-01-03;quarta-feira;14:00;Pleno dia;;BELO HORIZONTE;MG;1;;0;"
    )

    temp_file <- tempfile(fileext = ".csv")
    writeLines(csv_content, temp_file)

    # Carregar o arquivo CSV em um data.frame
    df_teste <- read.csv(temp_file, sep = ";")

    # Executar a função de análise de causalidade
    resultado_causalidade <- analisar_casualidade(df_teste)

    # Teste 1: Verificar se a probabilidade para "Ceu Claro" é 33,33%
    expect_equal(round(resultado_causalidade$condicao_metereologica$probabilidade[resultado_causalidade$condicao_metereologica$condicao_metereologica == "Ceu Claro"], 2), 33.33)

    # Teste 2: Verificar se a probabilidade para "Chuva Forte" é 33,33%
    expect_equal(round(resultado_causalidade$condicao_metereologica$probabilidade[resultado_causalidade$condicao_metereologica$condicao_metereologica == "Chuva Forte"], 2), 33.33)

    # Limpar o arquivo temporário após os testes
    unlink(temp_file)
})

cat("TESTES UNITÁRIOS FINALIZADOS - Módulo de Casualidade\n")

# Fim do arquivo test_casualidade.R
