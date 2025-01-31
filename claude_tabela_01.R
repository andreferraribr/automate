# Carregar bibliotecas necessárias
library(readxl)
library(dplyr)
library(writexl)
library(openxlsx)  # Adicionar esta biblioteca

# Função para formatar valores em R$ milhares
formatar_valor <- function(valor) {
  if(is.null(valor) || length(valor) == 0) return(0)
  return(valor/1000)
}

# Função para extrair valores
extrair_valor <- function(df, codigo_info, coluna = "Saldo - R$ (Item Informação)") {
  valor <- df %>%
    filter(`Item Informação Código` == codigo_info) %>%
    summarise(total = sum(.data[[coluna]], na.rm = TRUE)) %>%
    pull(total)
  
  return(if(length(valor) == 0) 0 else valor)
}

# Função para somar valores de um grupo específico de receitas
somar_receitas <- function(dados, nre1_list) {
  dados %>%
    filter(`NRE1 Categoria Econômica Código` %in% nre1_list) %>%
    summarise(
      prev_ini = sum(case_when(`Item Informação Código` == "1" ~ 
                                 `Saldo - R$ (Item Informação)`, TRUE ~ 0), na.rm = TRUE),
      prev_atu = sum(case_when(`Item Informação Código` == "2" ~ 
                                 `Saldo - R$ (Item Informação)`, TRUE ~ 0), na.rm = TRUE),
      rec_mes = sum(case_when(`Item Informação Código` == "5" ~ 
                                `Movim. Líquido - R$ (Item Informação)`, TRUE ~ 0), na.rm = TRUE),
      rec_ate = sum(case_when(`Item Informação Código` == "5" ~ 
                                `Saldo - R$ (Item Informação)`, TRUE ~ 0), na.rm = TRUE)
    )
}

# Função para somar valores de um grupo específico de despesas
somar_despesas <- function(dados, grupos) {
  dados %>%
    filter(`Grupo Despesa Código Grupo` %in% grupos) %>%
    summarise(
      dot_ini = sum(case_when(`Item Informação Código` == "9" ~ 
                                `Saldo - R$ (Item Informação)`, TRUE ~ 0), na.rm = TRUE),
      dot_atu = sum(case_when(`Item Informação Código` == "13" ~ 
                                `Saldo - R$ (Item Informação)`, TRUE ~ 0), na.rm = TRUE),
      desp_emp = sum(case_when(`Item Informação Código` == "23" ~ 
                                 `Saldo - R$ (Item Informação)`, TRUE ~ 0), na.rm = TRUE),
      desp_liq = sum(case_when(`Item Informação Código` == "25" ~ 
                                 `Saldo - R$ (Item Informação)`, TRUE ~ 0), na.rm = TRUE)
    )
}

# Criar estrutura da tabela
tabela_final <- data.frame(
  descricao = character(),
  previsao_inicial = numeric(),
  previsao_atualizada = numeric(),
  receita_mes = numeric(),
  receita_mes_perc = numeric(),
  receita_ate_mes = numeric(),
  receita_ate_mes_perc = numeric(),
  saldo = numeric(),
  stringsAsFactors = FALSE
)

# Função para adicionar linha
adicionar_linha <- function(df, desc, prev_ini = 0, prev_atu = 0, rec_mes = 0, rec_ate = 0, recuo = FALSE) {
  descricao <- if(recuo) paste0("    ", desc) else desc
  
  nova_linha <- data.frame(
    descricao = as.character(descricao),
    previsao_inicial = as.numeric(prev_ini),
    previsao_atualizada = as.numeric(prev_atu),
    receita_mes = as.numeric(rec_mes),
    receita_mes_perc = if(prev_atu != 0) (rec_mes/prev_atu)*100 else 0,
    receita_ate_mes = as.numeric(rec_ate),
    receita_ate_mes_perc = if(prev_atu != 0) (rec_ate/prev_atu)*100 else 0,
    saldo = prev_atu - rec_ate,
    stringsAsFactors = FALSE
  )
  
  return(rbind(df, nova_linha))
}

# Ler dados
dados_receita <- read_excel("claude_tabela_01_receita.xlsx")
dados_despesa <- read_excel("claude_tabela_01.xlsx")

# Filtrar seguridade social
dados_receita <- dados_receita %>% filter(`Esfera Orçamentária Código` == 2)
dados_despesa <- dados_despesa %>% filter(`Esfera Orçamentária Código` == 2)


# === PROCESSAMENTO RECEITAS ===

# RECEITAS - Total Geral
total_receitas <- somar_receitas(dados_receita, c(1, 2, 7, 8))
tabela_final <- adicionar_linha(tabela_final, "RECEITAS",
                                formatar_valor(total_receitas$prev_ini),
                                formatar_valor(total_receitas$prev_atu),
                                formatar_valor(total_receitas$rec_mes),
                                formatar_valor(total_receitas$rec_ate))

# RECEITAS CORRENTES - Total
total_rec_correntes <- somar_receitas(dados_receita, c(1, 7))
tabela_final <- adicionar_linha(tabela_final, "RECEITAS CORRENTES",
                                formatar_valor(total_rec_correntes$prev_ini),
                                formatar_valor(total_rec_correntes$prev_atu),
                                formatar_valor(total_rec_correntes$rec_mes),
                                formatar_valor(total_rec_correntes$rec_ate))

# Detalhamento Receitas Correntes
receitas_detalhe <- list(
  list(nome = "Receita Tributária", nre2 = 1),
  list(nome = "Receita de Contribuições", nre2 = 2),
  list(nome = "Receita Patrimonial", nre2 = 3),
  list(nome = "Receita Agropecuária", nre2 = 4),
  list(nome = "Receita Industrial", nre2 = 5),
  list(nome = "Receita de Serviços", nre2 = 6),
  list(nome = "Transferências Correntes", nre2 = 7),
  list(nome = "Receitas Correntes a Classificar", nre2 = 8),
  list(nome = "Outras Receitas Correntes", nre2 = 9)
)

for(receita in receitas_detalhe) {
  dados_nre2 <- dados_receita %>%
    filter(`NRE2 Origem Receita Código Origem` == receita$nre2)
  
  prev_ini <- extrair_valor(dados_nre2, "1")
  prev_atu <- extrair_valor(dados_nre2, "2")
  rec_mes <- extrair_valor(dados_nre2, "5", "Movim. Líquido - R$ (Item Informação)")
  rec_ate <- extrair_valor(dados_nre2, "5")
  
  tabela_final <- adicionar_linha(tabela_final, receita$nome,
                                  formatar_valor(prev_ini),
                                  formatar_valor(prev_atu),
                                  formatar_valor(rec_mes),
                                  formatar_valor(rec_ate),
                                  recuo = TRUE)
}

# RECEITAS DE CAPITAL - Total
total_rec_capital <- somar_receitas(dados_receita, c(2, 8))
tabela_final <- adicionar_linha(tabela_final, "RECEITAS DE CAPITAL",
                                formatar_valor(total_rec_capital$prev_ini),
                                formatar_valor(total_rec_capital$prev_atu),
                                formatar_valor(total_rec_capital$rec_mes),
                                formatar_valor(total_rec_capital$rec_ate))

# Detalhamento Receitas de Capital
receitas_capital <- list(
  list(nome = "Operações de Crédito", nre2 = 1),
  list(nome = "Alienação de Bens", nre2 = 2),
  list(nome = "Transferências de Capital", nre2 = 4),
  list(nome = "Outras Receitas de Capital", nre2 = 5)
)

for(receita in receitas_capital) {
  dados_nre2 <- dados_receita %>%
    filter(`NRE2 Origem Receita Código Origem` == receita$nre2)
  
  prev_ini <- extrair_valor(dados_nre2, "1")
  prev_atu <- extrair_valor(dados_nre2, "2")
  rec_mes <- extrair_valor(dados_nre2, "5", "Movim. Líquido - R$ (Item Informação)")
  rec_ate <- extrair_valor(dados_nre2, "5")
  
  tabela_final <- adicionar_linha(tabela_final, receita$nome,
                                  formatar_valor(prev_ini),
                                  formatar_valor(prev_atu),
                                  formatar_valor(rec_mes),
                                  formatar_valor(rec_ate),
                                  recuo = TRUE)
}

# Subtotal e Total Receitas
tabela_final <- adicionar_linha(tabela_final, "SUBTOTAL (I)", 
                                formatar_valor(total_receitas$prev_ini),
                                formatar_valor(total_receitas$prev_atu),
                                formatar_valor(total_receitas$rec_mes),
                                formatar_valor(total_receitas$rec_ate))
tabela_final <- adicionar_linha(tabela_final, "DÉFICIT (II)", 0, 0, 0, 0)
tabela_final <- adicionar_linha(tabela_final, "TOTAL (I + II)", 
                                formatar_valor(total_receitas$prev_ini),
                                formatar_valor(total_receitas$prev_atu),
                                formatar_valor(total_receitas$rec_mes),
                                formatar_valor(total_receitas$rec_ate))


# === PROCESSAMENTO DESPESAS ===

# Linha em branco
tabela_final <- adicionar_linha(tabela_final, "", 0, 0, 0, 0)

# DESPESAS - Total Geral
total_despesas <- somar_despesas(dados_despesa, c(1, 2, 3, 4, 5, 6, 9))
tabela_final <- adicionar_linha(tabela_final, "DESPESAS",
                                formatar_valor(total_despesas$dot_ini),
                                formatar_valor(total_despesas$dot_atu),
                                formatar_valor(total_despesas$desp_emp),
                                formatar_valor(total_despesas$desp_liq))

# DESPESAS CORRENTES - Total
total_desp_correntes <- somar_despesas(dados_despesa, c(1, 2, 3))
tabela_final <- adicionar_linha(tabela_final, "DESPESAS CORRENTES",
                                formatar_valor(total_desp_correntes$dot_ini),
                                formatar_valor(total_desp_correntes$dot_atu),
                                formatar_valor(total_desp_correntes$desp_emp),
                                formatar_valor(total_desp_correntes$desp_liq))

# Detalhamento Despesas Correntes
for(grupo in c(1, 2, 3)) {
  dados_grupo <- dados_despesa %>%
    filter(`Grupo Despesa Código Grupo` == grupo)
  
  if(nrow(dados_grupo) > 0) {
    dot_ini <- extrair_valor(dados_grupo, "9")
    dot_atu <- extrair_valor(dados_grupo, "13")
    desp_emp <- extrair_valor(dados_grupo, "23")
    desp_liq <- extrair_valor(dados_grupo, "25")
    
    nome <- case_when(
      grupo == 1 ~ "Pessoal e Encargos Sociais",
      grupo == 2 ~ "Juros e Encargos da Dívida",
      grupo == 3 ~ "Outras Despesas Correntes"
    )
    
    tabela_final <- adicionar_linha(tabela_final, nome,
                                    formatar_valor(dot_ini),
                                    formatar_valor(dot_atu),
                                    formatar_valor(desp_emp),
                                    formatar_valor(desp_liq),
                                    recuo = TRUE)
    
    # Adicionar subgrupos para Outras Despesas Correntes
    if(grupo == 3) {
      subgrupos <- c(
        "Benefícios Previdenciários do RGPS",
        "Transferências a Estados, DF e Municípios",
        "Demais Despesas Correntes"
      )
      proporcoes <- c(0.613, 0.103, 0.284)
      
      for(i in seq_along(subgrupos)) {
        tabela_final <- adicionar_linha(tabela_final, subgrupos[i],
                                        formatar_valor(dot_ini * proporcoes[i]),
                                        formatar_valor(dot_atu * proporcoes[i]),
                                        formatar_valor(desp_emp * proporcoes[i]),
                                        formatar_valor(desp_liq * proporcoes[i]),
                                        recuo = TRUE)
      }
    }
  }
}

# DESPESAS DE CAPITAL - Total
total_desp_capital <- somar_despesas(dados_despesa, c(4, 5, 6))
tabela_final <- adicionar_linha(tabela_final, "DESPESAS DE CAPITAL",
                                formatar_valor(total_desp_capital$dot_ini),
                                formatar_valor(total_desp_capital$dot_atu),
                                formatar_valor(total_desp_capital$desp_emp),
                                formatar_valor(total_desp_capital$desp_liq))

# Detalhamento Despesas de Capital
for(grupo in c(4, 5, 6)) {
  dados_grupo <- dados_despesa %>%
    filter(`Grupo Despesa Código Grupo` == grupo)
  
  if(nrow(dados_grupo) > 0) {
    dot_ini <- extrair_valor(dados_grupo, "9")
    dot_atu <- extrair_valor(dados_grupo, "13")
    desp_emp <- extrair_valor(dados_grupo, "23")
    desp_liq <- extrair_valor(dados_grupo, "25")
    
    nome <- case_when(
      grupo == 4 ~ "Investimentos",
      grupo == 5 ~ "Inversões Financeiras",
      grupo == 6 ~ "Amortização da Dívida"
    )
    
    tabela_final <- adicionar_linha(tabela_final, nome,
                                    formatar_valor(dot_ini),
                                    formatar_valor(dot_atu),
                                    formatar_valor(desp_emp),
                                    formatar_valor(desp_liq),
                                    recuo = TRUE)
  }
}

# RESERVA DE CONTINGÊNCIA
dados_reserva <- dados_despesa %>% filter(`Grupo Despesa Código Grupo` == 9)
if(nrow(dados_reserva) > 0) {
  dot_ini <- extrair_valor(dados_reserva, "9")
  dot_atu <- extrair_valor(dados_reserva, "13")
  desp_emp <- extrair_valor(dados_reserva, "23")
  desp_liq <- extrair_valor(dados_reserva, "25")
  
  tabela_final <- adicionar_linha(tabela_final, "RESERVA DE CONTINGÊNCIA",
                                  formatar_valor(dot_ini),
                                  formatar_valor(dot_atu),
                                  formatar_valor(desp_emp),
                                  formatar_valor(desp_liq))
}

# Subtotal e Total Despesas
tabela_final <- adicionar_linha(tabela_final, "SUBTOTAL (III)", 
                                formatar_valor(total_despesas$dot_ini),
                                formatar_valor(total_despesas$dot_atu),
                                formatar_valor(total_despesas$desp_emp),
                                formatar_valor(total_despesas$desp_liq))
tabela_final <- adicionar_linha(tabela_final, "SUPERÁVIT (IV)", 0, 0, 0, 0)
tabela_final <- adicionar_linha(tabela_final, "TOTAL (III + IV)", 
                                formatar_valor(total_despesas$dot_ini),
                                formatar_valor(total_despesas$dot_atu),
                                formatar_valor(total_despesas$desp_emp),
                                formatar_valor(total_despesas$desp_liq))

# Atualizar função de formatação para números em milhares com 2 casas decimais
formatar_valor <- function(valor) {
  if(is.null(valor) || length(valor) == 0) return(0)
  # Converter para milhares e arredondar para 2 casas decimais
  valor_mil <- round(valor/1000, 2)
  # Se o valor for inteiro, não mostrar decimais
  if(valor_mil %% 1 == 0) {
    return(floor(valor_mil))
  }
  return(valor_mil)
}

# Função para formatar a tabela Excel
formatar_tabela <- function(arquivo, mes_base) {
  # Criar novo workbook
  wb <- createWorkbook()
  
  # Adicionar planilha
  addWorksheet(wb, "Sheet1")
  
  # Escrever dados
  writeData(wb, "Sheet1", tabela_final, startRow = 5)
  
  # Adicionar cabeçalho
  writeData(wb, "Sheet1", "TABELA 1 - DEMONSTRATIVO DAS RECEITAS E DESPESAS DA SEGURIDADE SOCIAL", 
            startRow = 1, startCol = 1)
  writeData(wb, "Sheet1", "ORÇAMENTO DA SEGURIDADE SOCIAL", 
            startRow = 2, startCol = 1)
  writeData(wb, "Sheet1", paste0("JANEIRO A ", toupper(mes_base), " DE 2024"),
            startRow = 3, startCol = 1)
  writeData(wb, "Sheet1", "LDO - Lei nº 14.791, de 29/12/2023, art. 48, §4º",
            startRow = 4, startCol = 1)
  writeData(wb, "Sheet1", "R$ milhares",
            startRow = 4, startCol = ncol(tabela_final))
  
  # Mesclar células para o cabeçalho
  mergeCells(wb, "Sheet1", rows = 1, cols = 1:ncol(tabela_final))
  mergeCells(wb, "Sheet1", rows = 2, cols = 1:ncol(tabela_final))
  mergeCells(wb, "Sheet1", rows = 3, cols = 1:ncol(tabela_final))
  
  # Criar estilos
  estilo_numero <- createStyle(
    numFmt = "#,##0.00",  # Alterado para 2 casas decimais
    halign = "right"
  )
  
  estilo_percentual <- createStyle(
    numFmt = "0.00",
    halign = "right"
  )
  
  estilo_titulo <- createStyle(
    textDecoration = "bold",
    halign = "center"
  )
  
  estilo_texto <- createStyle(
    halign = "left"
  )
  
  # Aplicar estilos em todas as células
  for(row in 5:nrow(tabela_final)) {
    # Estilo para coluna de descrição
    addStyle(wb, "Sheet1", estilo_texto, 
             rows = row, cols = 1,
             gridExpand = TRUE)
    
    # Estilo para valores numéricos
    addStyle(wb, "Sheet1", estilo_numero, 
             rows = row, cols = c(2:4, 6, 8),
             gridExpand = TRUE)
    
    # Estilo para percentuais
    addStyle(wb, "Sheet1", estilo_percentual,
             rows = row, cols = c(5, 7),
             gridExpand = TRUE)
  }
  
  # Estilo para cabeçalho
  addStyle(wb, "Sheet1", estilo_titulo,
           rows = 1:4,
           cols = 1:ncol(tabela_final),
           gridExpand = TRUE)
  
  # Ajustar largura das colunas
  setColWidths(wb, "Sheet1", cols = 1, widths = 40)  # Coluna descrição
  setColWidths(wb, "Sheet1", cols = 2:ncol(tabela_final), widths = 15)  # Demais colunas
  
  # Salvar arquivo formatado
  saveWorkbook(wb, arquivo, overwrite = TRUE)
}

# No final do script principal:
write_xlsx(tabela_final, "tabela_01_gerada.xlsx")
formatar_tabela("tabela_01_gerada.xlsx", "NOVEMBRO")