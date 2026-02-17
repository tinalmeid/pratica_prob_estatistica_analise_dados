#' @file data_preparation.R
#' @description Função para carregar e preparar os dados da PRF 2024 para análise exploratória e probabilidade
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

# Caminho para os dados
cat("************************************************************************\n")
if (!file.exists("data/datatran2024.csv")) {
  stop("ERRO: Arquivo 'datatran2024.csv' não encontrado no diretório 'data/'. Verifique o caminho e o nome do arquivo.")
} else {
  cat("Arquivo 'datatran2024.csv' encontrado. Prosseguindo com o carregamento dos dados...\n")
  cat("\n")
}
caminho_dados <- "data/datatran2024.csv"

#' @description Função para carregar os dados e limpar os dados
#' @param caminho_dados Caminho para o arquivo CSV contendo os dados da PRF 2024
#' @return Data frame limpo = df_temp
#' # function() = encapsula o código para carregar e limpar os dados
#' def_temp = variável temporária para armazenar dados brutos carregados do CSV

dados_carregados <- function(caminho_dados) {
  # Verificar se o arquivo existe antes de tentar carregá-lo
  if (!file.exists(caminho_dados)) {
    stop(paste("ERRO: O arquivo", caminho_dados, "não foi encontrado. Verifique o caminho e o nome do arquivo."))
  }

  # Carregar os dados, com separador ";"  que é padrão do arquivo
  df_temp <- read.csv(caminho_dados,
                      sep = ";",
                      dec = ",",
                      stringsAsFactors = FALSE,
                      encoding = "latin1")

  # Contagem de linhas para comparar com o total depois de limpar os dados
  total_linhas_brutas <- nrow(df_temp)

  # Exibir os nomes das colunas do arquivo carregado para depuração
  cat("Colunas encontradas no arquivo CSV:\n")
  print(colnames(df_temp))

  # Reorganizar as colunas que serão usadas na análise
  col_ordenadas <- c("data_inversa",
                     "dia_semana",
                     "horario",
                     "fase_dia",
                     "condicao_metereologica",
                     "municipio",
                     "uf",
                     "veiculos",
                     "tipo_acidente",
                     "causa_acidente",
                     "feridos",
                     "feridos_leves",
                     "feridos_graves")

  cat("========================================================================\n")
  cat("Ordenação das das colunas que serão utilizadas na análise:\n")
  print(col_ordenadas)
  cat("\n")

  # Verificar se todas as colunas ordenadas existem no data frame
  colunas_existentes <- col_ordenadas[col_ordenadas %in% colnames(df_temp)]

  # Verificar se todas as colunas necessárias estão presentes no arquivo CSV
  colunas_faltando <- setdiff(col_ordenadas, colnames(df_temp))
  if (length(colunas_faltando) > 0) {
    stop(paste("As seguintes colunas estão ausentes no arquivo CSV:", paste(colunas_faltando, collapse = ", ")))
  }

  # Selecionar e reorganizar as colunas na ordem definida
  df_temp <- df_temp |> select(all_of(colunas_existentes))

  # Limpar os dados - convertendo colunas para os tipos corretos
  # e lidando com valores ausentes
  # replace_na() = função do pacote tidyr para substituir valores NA por 0
  df_temp <- df_temp |> mutate(
    data_inversa = as.Date(.data$data_inversa, format = "%d/%m/%Y"),
    dia_semana = as.character(.data$dia_semana),
    horario = format(strptime(.data$horario, format = "%H:%M"), "%H:%M"),
    fase_dia = as.character(.data$fase_dia),
    condicao_metereologica = as.character(.data$condicao_metereologica),
    municipio = as.character(.data$municipio),
    uf = as.character(.data$uf),
    veiculos = as.character(.data$veiculos),
    tipo_acidente = as.character(.data$tipo_acidente),
    causa_acidente = as.character(.data$causa_acidente),
    feridos = replace_na(as.integer(.data$feridos), 0),
    feridos_leves = replace_na(as.integer(.data$feridos_leves), 0),
    feridos_graves = replace_na(as.integer(.data$feridos_graves), 0)
   )

  # Retornar o data frame limpo e o total de linhas brutas
  list(df = df_temp, total_linhas_brutas = total_linhas_brutas)
}

# Chamar a função para carregar e limpar os dados
dados_resultado <- dados_carregados(caminho_dados)

df_detran_prf2024 <- dados_resultado$df
total_linhas_brutas <- dados_resultado$total_linhas_brutas

# Contagem de linhas para verificar se os dados foram carregados corretamente
total_linhas <- nrow(df_detran_prf2024)
cat("========================================================================\n")
cat("Comparativo :\n")
if (total_linhas_brutas == total_linhas) {
  cat("Sucesso: A quantidade de linhas inicial (", total_linhas_brutas, ") é igual aos dados tratados (", total_linhas, ").\n")
} else {
  cat("AVISO: O número de linhas brutas (", total_linhas_brutas, ") não é igual ao número de linhas tratadas (", total_linhas, "). Verifique o processo de limpeza dos dados.\n")
}
cat("\n")

# Exibir com Glimpse para uma visão geral da estrutura dos dados
cat("========================================================================\n")
cat("Exibindo as primeiras linhas após tratamento dos dados:\n")
df_detran_prf2024 |> glimpse()
cat("\n")

# Fim do arquivo data_preparation.R
