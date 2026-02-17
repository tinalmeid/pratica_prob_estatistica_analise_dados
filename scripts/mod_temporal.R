#' @file mod_temporal.R
#' @description Módulo para análise temporal e probabilística dos acidentes para  escalas da PRF 2024
#' Categoriza horários e calcula a probabilidade de acidentes ocorrerem em determinados períodos do dia para
#' planejamento de escalas de patrulhamento e ações de segurança viária.
#'
#' @title Aula Prática Disciplina Probabilidade e Estatística
#' para Análise de Dados - Unidade I Aula 4
#' @name Manipulação e análise de dados com R
#'
#' @author Cristina de Almeida
#' @version 1.0
#' @date Fevereiro 2026

# --- CONFIGURAÇÃO INICIAL ---
options(radian.enabled = TRUE) # Configurar para usar R no VS Code
options(OutDec = ",", big.mark = ".") # Exibir "," como separador decimal e "." como separador de milhar
options("repos" = c(CRAN = "https://cloud.r-project.org/")) # Definir o repositório padrão do CRAN para instalação de pacotes

#' @description Função para análise temporal dos acidentes da PRF 2024
#' @param df_temporal Data frame contendo os dados tratados no 'data_preparation.R'
#' @return Data frame com as métricas temporais calculadas

analisar_temporal <- function(df_temporal) {
  cat("************************************************************************\n")
  cat("MÓDULO TEMPORAL - Iniciando análises temporais dos acidentes da PRF 2024...\n")
  cat("\n")

  # Validar se df possui as colunas necessárias para análise temporal
  colunas_necessarias <- c("horario", "data_inversa", "dia_semana")
  if (!all(colunas_necessarias %in% colnames(df_temporal))) {
    stop("Erro: Colunas temporais não encontradas no data frame.")
  }

  # --- 1. CATEGORIZAÇÃO DE HORÁRIOS ---
  df_temporal_categorias <- df_temporal |>
    mutate(
      # Extrair a hora do horário para categorizar os períodos do dia
      hora_numero = as.numeric(substr(horario, 1, 2)),
      periodo_dia = case_when(
        hora_numero >= 0 & hora_numero < 6 ~ "Madrugada",
        hora_numero >= 6 & hora_numero < 12 ~ "Manhã",
        hora_numero >= 12 & hora_numero < 18 ~ "Tarde",
        hora_numero >= 18 & hora_numero <= 23 ~ "Noite"
      ),
        # Identificar o tipo de dia da semana para análises adicionais
      eh_fim_semana = ifelse(dia_semana %in% c("sábado", "domingo"), "Fim de Semana", "Dia Útil"),
      # Categorizar em ciclos de 12 horas para análises de probabilidade
      ciclo_12h = ifelse(periodo_dia %in% c("Manhã", "Tarde"), "Dia", "Noite")
    )

  # --- 2. CALCULO DAS PROBABILIDADES ---
  # Definir o denominador total para o cálculo da probabilidade
  total_acidentes <- nrow(df_temporal)

  df_temporal_prob <- df_temporal_categorias |>
    group_by(eh_fim_semana, periodo_dia, ciclo_12h) |>
    summarise(
      frequencia = dplyr::n(),
      .groups = "drop"
    ) |>
    mutate(
      probabilidade = frequencia / total_acidentes * 100)

  # A - Probabilidade por tipo de dia/ciclo
  prob_fds <- sum (df_temporal_prob$frequencia[df_temporal_prob$eh_fim_semana
    == "Fim de Semana"]) / total_acidentes * 100

  prob_util <- sum (df_temporal_prob$frequencia[df_temporal_prob$eh_fim_semana
    == "Dia Útil"]) / total_acidentes * 100

  prob_ciclo_dia <- sum (df_temporal_prob$frequencia[df_temporal_prob$ciclo_12h
    == "Dia"]) / total_acidentes * 100

  prob_ciclo_noite <- sum (df_temporal_prob$frequencia[df_temporal_prob$ciclo_12h
    == "Noite"]) / total_acidentes * 100

  # B - Probabilidade Específicas Cruzadas
  # O pull(prob) extrai apenas o número do data frame resultante
  prob_madrugada_fds <- sum(df_temporal_prob$probabilidade[df_temporal_prob$eh_fim_semana
    == "Fim de Semana" & df_temporal_prob$periodo_dia == "Madrugada"])

  prob_manha_util     <- sum(df_temporal_prob$probabilidade[df_temporal_prob$eh_fim_semana
    == "Dia Útil" & df_temporal_prob$periodo_dia == "Manhã"])

  prob_tarde_util     <- sum(df_temporal_prob$probabilidade[df_temporal_prob$eh_fim_semana
    == "Dia Útil" & df_temporal_prob$periodo_dia == "Tarde"])

  prob_tarde_fds      <- sum(df_temporal_prob$probabilidade[df_temporal_prob$eh_fim_semana
    == "Fim de Semana" & df_temporal_prob$periodo_dia == "Tarde"])

  cat("========================================================================\n")
  cat("Probabilidades de acidentes por tipo de dia (Útil ou Fim de Semana):\n")
  cat("Probabilidade de acidentes por tipo de dia:", round(prob_fds, 2), "% (Fim de Semana)\n")
  cat("Probabilidade de acidentes por tipo de dia:", round(prob_util, 2), "% (Dia Útil)\n")
  cat("\n")


  cat("========================================================================\n")
  cat("Probabilidade de acidentes no ciclo diurno:", round(prob_ciclo_dia, 2), "%\n")
  cat("Probabilidade de acidentes no ciclo noturno:", round(prob_ciclo_noite, 2), "%\n")
  cat("\n")


  cat("====================== Outros exemplos: =================================\n")
  cat("Algumas visões de probabilidades de acidentes por tipo de + período de dia:\n")
  cat("Probabilidade de acidentes na madrugada de fim de semana:", round(prob_madrugada_fds, 2), "%\n")
  cat("Probabilidade de acidentes na manhã de dia útil:", round(prob_manha_util, 2), "%\n")
  cat("Probabilidade de acidentes na tarde de dia útil:", round(prob_tarde_util, 2), "%\n")
  cat("Probabilidade de acidentes na tarde de fim de semana:", round(prob_tarde_fds, 2), "%\n")
  cat("\n")

  # C - Cálculo Z para comparação de proporções entre dia e noite
  # Proporção de acidentes no dia e na noite
  freq_fds <- sum(df_temporal_prob$frequencia[df_temporal_prob$eh_fim_semana == "Fim de Semana"])
  freq_util <- sum(df_temporal_prob$frequencia[df_temporal_prob$eh_fim_semana == "Dia Útil"])

  # Proporção de acidentes no dia e na noite
  prop_fds <- freq_fds / total_acidentes
  prop_util <- freq_util / total_acidentes

  # Desvio padrão de cada proporção
  desvio_fds <- sqrt(prop_fds * (1 - prop_fds))
  desvio_util <- sqrt(prop_util * (1 - prop_util))

  # Proporção pooled: (x1 + x2) / (n1 + n2) : Um único resultado para ambas as proporções
  prop_pooled <- (freq_fds + freq_util) / (total_acidentes + total_acidentes)

  # Erro padrão da diferença entre as proporções usando a proporção pooled
  erro_padrao <- sqrt(prop_pooled * (1 - prop_pooled) * (1 / total_acidentes + 1 / total_acidentes))

  # Cálculo do Z-score
  z_score <- (prop_fds - prop_util) / erro_padrao

  # Calcular o valor-p para o teste de hipótese
  valor_p <- 2 * (1 - pnorm(abs(z_score)))

  # Formatar p-valor para exibição(5 casas decimais)
  formatar_pvalor <- function(p) {
    if (p < 0.0001) {
      return("< 0.0001 (altamente significativo)")
    } else if (p < 0.001) {
      return(paste0(formatC(p, format = "f", digits = 6), " (muito significativo)"))
    } else if (p < 0.01) {
      return(paste0(formatC(p, format = "f", digits = 4), " (significativo)"))
    } else if (p < 0.05) {
      return(paste0(formatC(p, format = "f", digits = 4), " (marginalmente significativo)"))
    } else {
      return(paste0(formatC(p, format = "f", digits = 4), " (não significativo)"))
    }
  }

  cat("========================================================================\n")
  cat("Teste Z para proporções : Dia Útil vs Fim de Semana\n")
  cat("  H0: P(FdS) = P(Útil) | H1: P(FdS) ≠ P(Útil)\n")
  cat("  Frequência de acidentes no Fim de Semana:", format(freq_fds, big.mark = "."), "\n") # formato para milhar
  cat("  Frequência de acidentes no Dia Útil:", format(freq_util, big.mark = "."), "\n")
  cat("  P(fds) =", round(prop_fds, 4), "| P(Útil) =", round(prop_util, 4), "\n")
  cat("  Desvio Padrão FdS:", round(desvio_fds, 4), "| Desvio Padrão Útil:", round(desvio_util, 4), "\n")
  cat("  Proporção pooled:", round(prop_pooled, 4), "\n")
  cat("  Erro padrão da diferença:", round(erro_padrao, 4), "\n")
  cat("  Z-score:", round(z_score, 4), "\n")
  cat("  Valor-p:", formatar_pvalor(valor_p), "\n")
  if (valor_p < 0.05) {
    cat("  Resultado: Rejeitamos H0 - Há uma diferença estatisticamente significativa entre as proporções de acidentes no Fim de Semana e Dia Útil.\n")
    cat("  Faz sentido executar ações de segurança viária diferenciadas para cada tipo de dia.\n")
  } else {
    cat("  Resultado: Não rejeitamos H0 - Não há evidências suficientes para afirmar que as proporções de acidentes no Fim de Semana e Dia Útil são diferentes.\n")
  }
  cat("\n")

  cat("Fim das análises temporais.\n")
  cat("\n")
  # Retornar o data frame para uso análises gráficas
  return(df_temporal_prob)
}

# log
message("Módulo 'mod_temporal.R' carregado com sucesso.")

# Fim do arquivo mod_temporal.R
