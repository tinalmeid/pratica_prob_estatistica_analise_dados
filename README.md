# 📊 Prática de Probabilidade e Estatística - Unidade I Aula 4

> **Status do Projeto**: Em Desenvolvimento
> ![Status da Análise](https://github.com/tinalmeid/pratica_unidade_I_prob/actions/workflows/r_analysis.yml/badge.svg)

---

## 🎯 Objetivo do Projeto

Este repositório contém a resolução da Aula Prática da Unidade I, focada na análise exploratória e cálculos probabilísticos sobre os dados de acidentes em rodovias federais (PRF) do ano de 2024.

### Perguntas do Roteiro

| # | Pergunta | Módulo Responsável |
| --- | --- | --- |
| 1 | Qual foi o estado com o maior número de acidentes? | `mod_geografia.R` |
| 2 | Qual a probabilidade de um acidente ocorrer em condições climáticas claras? | `mod_casualidade.R` |
| 3 | Como a fase do dia afeta a ocorrência de acidentes? | `mod_temporal.R` |
| 4 | Que insights podem ser gerados sobre os tipo de acidentes e suas causas? | `mod_casualidade.R` |

## 🛠️ Tecnologias Utilizadas

![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white) ![Git](https://img.shields.io/badge/git-%23F05033.svg?style=for-the-badge&logo=git&logoColor=white) ![GitHub Actions](https://img.shields.io/badge/github%20actions-%232088FF.svg?style=for-the-badge&logo=github-actions&logoColor=white) ![Visual Studio Code](https://img.shields.io/badge/Visual%20Studio%20Code-0078d7.svg?style=for-the-badge&logo=visual-studio-code&logoColor=white) ![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white) ![Testthat](https://img.shields.io/badge/testthat-%23191919.svg?style=for-the-badge&logo=r&logoColor=white) ![Markdown](https://img.shields.io/badge/markdown-%23000000.svg?style=for-the-badge&logo=markdown&logoColor=white)

![dplyr](https://img.shields.io/badge/dplyr-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white) ![ggplot2](https://img.shields.io/badge/ggplot2-%23191919.svg?style=for-the-badge&logo=r&logoColor=white) ![scales](https://img.shields.io/badge/scales-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white) ![Microsoft Excel](https://img.shields.io/badge/Microsoft_Excel-217346?style=for-the-badge&logo=microsoft-excel&logoColor=white) ![Microsoft Word](https://img.shields.io/badge/Microsoft_Word-2B579A?style=for-the-badge&logo=microsoft-word&logoColor=white)
![PDF](https://img.shields.io/badge/PDF-EC1C24?style=for-the-badge&logo=adobe-acrobat-reader&logoColor=white)

## 🧪 Qualidade e Testes

Diferente de projetos que usam SonarCloud, aqui optei por uma abordagem de **Engenharia de Software** aplicada à Ciência de Dados com:

* **Testes Unitários com `testthat`:** Garantia de integridade das colunas e validação das regras de probabilidade (P(S)) = 100%, eventos complementares) e conservação de registros em cada um dos módulos de análise.
* **Clean Code & SOLID:** Código modularizado para facilitar a manutenção e legibilidade, onde cada módulo possui sua responsabilidade única (análise, gráfico ou teste)

### Cobertura de Testes

| Módulo de Teste | Módulo Testado | Principais Asserts |
| --- | --- | --- |
| `test_data_preparation.R` | `mod_data_preparation.R` | Valida o ETL, a ingestão de CSV e a integridade do df. |
| `test_casualidade.R` | `mod_casualidade.R` | Valida a lógica de ranking e a agregação de acidentes por município. |
| `test_geografia.R` | `mod_geografia.R` | Valida os cálculos de média/mediana e os benchmarks nacionais. |
| `test_severidade.R` | `mod_severidade.R` | Valida os cálculos de probabilidade e a categorização temporal. |
| `test_temporal.R` | `mod_temporal.R` | Valida as métricas de causalidade e a análise de fatores. |

## 📊 Visualizações

Gráficos profissionais com tema escuro customizado (`mod_graficos_tema.R`), gerados `ggplot2`e salvos em PNG (300 DPI)

| Gráfico | Arquivo | Descrição |
| --- | --- | --- |
| WIP | WIP | WIP |

## 📂 Estrutura do Repositório

* `/.github`: Configuração do pipeline de automação
* `/data`: Contém o dataset `datatran2024.csv`.
* `/scripts`: Script principal `main.R` , módulos de análises (`mod_casualidade.R`, `mod_geografia.R`, `mod_severidade.R`, `mod_temporal.R`) e módulos de geração de gráficos (`mod_graficos_tema.R`, WIP, WIP, WIP, WIP)
* `/test`: Testes unitários das funções (`test_casualidade.R`, `test_geografia.R`, `test_severidade.R`, `test_temporal.R`)
* `/graficos`: PNGs gerados em tema escuro customizado e 300 DPI

---

### Fluxo de Execução

```text
main.R
    ├── 1. data_preparation.R            -> Carrega e limpa o csv
    ├── 2. Módulos de análises (4x)      -> Calcula probabilidades e métricas
    ├── 3. Módulos de teste (4x)         -> Valida as regras antes de prosseguir
    └── 4. Módulos de gráficos (4x)      -> Gera visualizações em PNG
```

## 🏃🏾‍♀️ Pace de Evolução

"A constância é o que leva à excelência, seja na corrida ou na análise de dados." 🏁
