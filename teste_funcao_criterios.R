aplicar_criterios <- function(df, criterios, agrupamento = NULL) {
  
  # Detectar automaticamente a métrica
  metrica <- names(df)[grepl("^(saldo|movim)", names(df), ignore.case = TRUE)][1]
  
  # Capturar o nome do dataframe passado como argumento
  df_completo <- paste(deparse(substitute(df)), collapse = " ")
  
  # Separar nome da df de eventuais filtros
  if (grepl("%>%", df_completo)) {
    # Se há pipe, extrair apenas o nome da df original
    df_nome <- trimws(strsplit(df_completo, "%>%")[[1]][1])
    df_filtros <- paste(strsplit(df_completo, "%>%")[[1]][-1], collapse = " %>% ")
    df_filtros <- trimws(df_filtros)
  } else {
    # Se não há pipe, usar o nome completo
    df_nome <- df_completo
    df_filtros <- NA
  }
  
  # Capturar o nome da variável criterios passada como argumento
  criterios_nome <- deparse(substitute(criterios))
  
  # Definir variáveis de agrupamento (padrão apenas mes_lancamento)
  group_vars <- "mes_lancamento"
  if (!is.null(agrupamento)) {
    group_vars <- c(group_vars, agrupamento)
  }
  
  # Desmembrar o nome da variável criterios usando "_" como separador
  partes <- strsplit(criterios_nome, "_")[[1]]
  
  # Remover "criterios" se estiver presente
  if (partes[1] == "criterios") {
    partes <- partes[-1]
  }
  
  relatorio <- if (length(partes) >= 1) partes[1] else NA
  anexo_codigo <- if (length(partes) >= 2) partes[2] else NA
  anexo_nome <- if (length(partes) >= 3) partes[3] else NA
  detalhe <- if (length(partes) >= 4) paste(partes[4:length(partes)], collapse = "_") else NA
  
  # Aplicar critérios - esta é a parte fundamental
  resultado <- map_df(names(criterios), function(categoria) {
    crit <- criterios[[categoria]]
    
    # Extrair informação de print do nome da categoria
    if (grepl("_s$", categoria)) {
      print_flag <- "s"
      categoria_limpa <- str_replace(categoria, "_s$", "")
    } else if (grepl("_n$", categoria)) {
      print_flag <- "n"
      categoria_limpa <- str_replace(categoria, "_n$", "")
    } else {
      print_flag <- "s"  # default
      categoria_limpa <- categoria
    }
    
    # Verificar se o critério é uma fórmula matemática (contém chaves de outros itens)
    if (grepl("\\{[^}]+\\}", crit$criterio)) {
      # É uma fórmula matemática - processar depois que todos os itens básicos forem calculados
      return(data.frame(
        categoria = categoria_limpa,
        metrica = metrica,
        dataframe_nome = df_nome,
        dataframe_filtros = df_filtros,
        relatorio = relatorio,
        anexo_codigo = anexo_codigo,
        anexo_nome = anexo_nome,
        detalhe = detalhe,
        print = print_flag,
        filtro = crit$criterio,
        valor = NA,  # Será calculado depois
        formula_matematica = TRUE,
        stringsAsFactors = FALSE
      ) %>%
        separate(categoria, into = c("ordem", "nome"), sep = 2, remove = FALSE) %>%
        group_by(across(all_of(group_vars))) %>%
        slice(1) %>%
        ungroup()
      )
    } else {
      # É um critério normal de filtro
      # Avaliar condição
      condicao_expr <- eval(parse(text = crit$criterio), envir = df)
      
      df %>%
        filter(condicao_expr) %>%
        group_by(across(all_of(group_vars))) %>%
        summarise(
          valor = sum(.data[[metrica]], na.rm = TRUE), 
          .groups = "drop"
        ) %>%
        mutate(
          categoria = categoria_limpa,
          metrica = metrica,
          dataframe_nome = df_nome,
          dataframe_filtros = df_filtros,
          relatorio = relatorio,
          anexo_codigo = anexo_codigo,
          anexo_nome = anexo_nome,
          detalhe = detalhe,
          print = print_flag,
          filtro = crit$criterio,
          formula_matematica = FALSE
        ) %>%
        separate(categoria, into = c("ordem", "nome"), sep = 2, remove = FALSE)
    }
  })
  
  # Processar fórmulas matemáticas após todos os itens básicos
  formulas <- resultado %>% filter(formula_matematica == TRUE, is.na(valor))
  
  if (nrow(formulas) > 0) {
    # Criar lookup dos valores calculados (por ordem)
    valores_calculados <- resultado %>% 
      filter(formula_matematica == FALSE) %>%
      select(ordem, valor) %>%
      distinct()
    
    lookup_valores <- setNames(valores_calculados$valor, valores_calculados$ordem)
    
    # Processar cada fórmula
    for (i in 1:nrow(formulas)) {
      formula <- formulas$filtro[i]
      
      # Substituir os placeholders {ordem} pelos valores
      placeholders <- str_extract_all(formula, "\\{[^}]+\\}")[[1]]
      formula_processada <- formula
      
      for (placeholder in placeholders) {
        ordem_ref <- str_remove_all(placeholder, "[{}]")
        if (ordem_ref %in% names(lookup_valores)) {
          valor_ref <- lookup_valores[[ordem_ref]]
          formula_processada <- str_replace(formula_processada, 
                                            fixed(placeholder), 
                                            as.character(valor_ref))
        } else {
          formula_processada <- str_replace(formula_processada, 
                                            fixed(placeholder), 
                                            "0")
        }
      }
      
      # Calcular o resultado da fórmula
      valor_calculado <- eval(parse(text = formula_processada))
      
      # Atualizar o resultado
      resultado[resultado$ordem == formulas$ordem[i] & 
                  resultado$formula_matematica == TRUE, "valor"] <- valor_calculado
    }
  }
  
  # Remover a coluna auxiliar
  resultado <- resultado %>% select(-formula_matematica)
  
  # Criar o nome da nova dataframe com prefixo df_
  novo_nome <- paste0("df_", criterios_nome)
  
  # Atribuir o resultado à nova dataframe no ambiente global
  assign(novo_nome, resultado, envir = .GlobalEnv)
  
  # Retornar o resultado também
  return(resultado)
}

# Função para consolidar todas as dataframes com prefixo "df_criterios"
consolidar_criterios <- function() {
  # Obter todos os objetos no ambiente global
  objetos <- ls(envir = .GlobalEnv)
  
  # Filtrar apenas as dataframes que começam com "df_criterios"
  df_criterios <- objetos[grepl("^df_criterios", objetos)]
  
  if (length(df_criterios) == 0) {
    stop("Nenhuma dataframe com prefixo 'df_criterios' foi encontrada")
  }
  
  # Obter todas as dataframes e combiná-las
  tabelas <- map(df_criterios, ~ get(.x, envir = .GlobalEnv))
  
  # Combinar todas as tabelas
  resultado_completo <- bind_rows(tabelas)
  
  # Reagrupar apenas por mes_lancamento, ignorando agrupamentos adicionais
  resultado <- resultado_completo %>%
    group_by(mes_lancamento, categoria, ordem, nome, metrica, dataframe_nome, dataframe_filtros,
             relatorio, anexo_codigo, anexo_nome, detalhe, print, filtro) %>%
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    arrange(mes_lancamento, relatorio, anexo_codigo, detalhe, ordem, detalhe) %>%
    select(-categoria) %>%
    mutate(
      # Converter mes_lancamento para data (último dia do mês)
      data = case_when(
        grepl("JAN", mes_lancamento) ~ as.Date(paste0(str_extract(mes_lancamento, "\\d{4}"), "-01-31")),
        grepl("FEV", mes_lancamento) ~ as.Date(paste0(str_extract(mes_lancamento, "\\d{4}"), "-02-28")),
        grepl("MAR", mes_lancamento) ~ as.Date(paste0(str_extract(mes_lancamento, "\\d{4}"), "-03-31")),
        grepl("ABR", mes_lancamento) ~ as.Date(paste0(str_extract(mes_lancamento, "\\d{4}"), "-04-30")),
        grepl("MAI", mes_lancamento) ~ as.Date(paste0(str_extract(mes_lancamento, "\\d{4}"), "-05-31")),
        grepl("JUN", mes_lancamento) ~ as.Date(paste0(str_extract(mes_lancamento, "\\d{4}"), "-06-30")),
        grepl("JUL", mes_lancamento) ~ as.Date(paste0(str_extract(mes_lancamento, "\\d{4}"), "-07-31")),
        grepl("AGO", mes_lancamento) ~ as.Date(paste0(str_extract(mes_lancamento, "\\d{4}"), "-08-31")),
        grepl("SET", mes_lancamento) ~ as.Date(paste0(str_extract(mes_lancamento, "\\d{4}"), "-09-30")),
        grepl("OUT", mes_lancamento) ~ as.Date(paste0(str_extract(mes_lancamento, "\\d{4}"), "-10-31")),
        grepl("NOV", mes_lancamento) ~ as.Date(paste0(str_extract(mes_lancamento, "\\d{4}"), "-11-30")),
        grepl("DEZ", mes_lancamento) ~ as.Date(paste0(str_extract(mes_lancamento, "\\d{4}"), "-12-31")),
        TRUE ~ as.Date(NA)
      ),
      # Criar chave única
      chave = paste0(str_extract(mes_lancamento, "\\d{4}"), 
                     case_when(
                       grepl("JAN", mes_lancamento) ~ "01",
                       grepl("FEV", mes_lancamento) ~ "02", 
                       grepl("MAR", mes_lancamento) ~ "03",
                       grepl("ABR", mes_lancamento) ~ "04",
                       grepl("MAI", mes_lancamento) ~ "05",
                       grepl("JUN", mes_lancamento) ~ "06",
                       grepl("JUL", mes_lancamento) ~ "07",
                       grepl("AGO", mes_lancamento) ~ "08",
                       grepl("SET", mes_lancamento) ~ "09",
                       grepl("OUT", mes_lancamento) ~ "10",
                       grepl("NOV", mes_lancamento) ~ "11",
                       grepl("DEZ", mes_lancamento) ~ "12",
                       TRUE ~ "00"
                     ),
                     "_", tolower(relatorio), "_", tolower(anexo_codigo), "_", tolower(detalhe), "_", ordem)
    ) %>%
    select(chave, mes_lancamento, data, dataframe_nome, dataframe_filtros, relatorio, anexo_codigo, 
           anexo_nome, detalhe, print, filtro, metrica, ordem, nome, valor)
  
  return(resultado)
}

# Função para realizar operações matemáticas usando as chaves únicas
calcular_operacoes <- function(dados_consolidados = NULL, ...) {
  
  # Se dados não fornecidos, usar consolidar_criterios()
  if (is.null(dados_consolidados)) {
    dados_consolidados <- consolidar_criterios()
  }
  
  # Capturar as operações passadas como argumentos
  operacoes <- list(...)
  
  if (length(operacoes) == 0) {
    stop("Nenhuma operação foi especificada")
  }
  
  # Criar um lookup table para facilitar a busca
  lookup_valores <- setNames(dados_consolidados$valor, dados_consolidados$chave)
  
  # Processar cada operação
  resultados <- map(names(operacoes), function(nome_operacao) {
    operacao <- operacoes[[nome_operacao]]
    
    if (is.character(operacao)) {
      # Operação simples: apenas a fórmula como string
      formula <- operacao
    } else if (is.list(operacao) && !is.null(operacao$formula)) {
      # Operação estruturada
      formula <- operacao$formula
    } else {
      stop(paste("Operação", nome_operacao, "deve ser uma string (fórmula) ou list com 'formula'"))
    }
    
    formula_processada <- formula
    
    # Encontrar todas as chaves na fórmula (padrão: YYYYMM_texto_texto_texto_NN)
    chaves <- str_extract_all(formula, "[0-9]{6}_[a-zA-Z0-9/_]+_[a-zA-Z0-9/_]+_[a-zA-Z0-9/_]+_[0-9]+")[[1]]
    
    for (chave in chaves) {
      valor <- ifelse(chave %in% names(lookup_valores), 
                      lookup_valores[[chave]], 
                      0)
      formula_processada <- str_replace(formula_processada, 
                                        fixed(chave), 
                                        as.character(valor))
    }
    
    # Avaliar a expressão matemática
    resultado_calculo <- eval(parse(text = formula_processada))
    
    return(list(
      operacao = nome_operacao,
      formula_original = formula,
      formula_processada = formula_processada,
      resultado = resultado_calculo
    ))
  })
  
  names(resultados) <- names(operacoes)
  return(resultados)
}
criterios_rreo_A03_rcl_receitas <- list(
  
  # === RECEITAS CORRENTES POR ORIGEM (01-09) ===
  
  # IMPOSTOS, TAXAS E CONTRIBUIÇÕES DE MELHORIA
  `01  Impostos, Taxas e Contribuições de Melhoria` = list(
    criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 1"
  ),
  
  # RECEITA DE CONTRIBUIÇÕES
  `02  Receita de Contribuições` = list(
    criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 2"
  ),
  
  # RECEITA PATRIMONIAL
  `03  Receita Patrimonial` = list(
    criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 3"
  ),
  
  # RECEITA AGROPECUÁRIA
  `04  Receita Agropecuária` = list(
    criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 4"
  ),
  
  # RECEITA INDUSTRIAL
  `05  Receita Industrial` = list(
    criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 5"
  ),
  
  # RECEITA DE SERVIÇOS
  `06  Receita de Serviços` = list(
    criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 6"
  ),
  
  # TRANSFERÊNCIAS CORRENTES
  `07  Transferências Correntes` = list(
    criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 7"
  ),
  
  # RECEITAS CORRENTES A CLASSIFICAR
  `08  Receitas Correntes a Classificar` = list(
    criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 8"
  ),
  
  # OUTRAS RECEITAS CORRENTES
  `09  Outras Receitas Correntes` = list(
    criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 9"
  ),
  
  # === ITENS ESPECÍFICOS PARA CÁLCULO DA RCL ===
  
  # TRANSFERÊNCIAS CONSTITUCIONAIS E LEGAIS
  `10  Transferências Constitucionais e Legais` = list(
    criterio = "
    nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 7 & (
      grepl('FPE|FPM|CONSTITUCIONAL|LEGAL', natureza_receita_nome, ignore.case = TRUE) |
      grepl('^172[0-9]', natureza_receita_codigo_completo) |
      grepl('^171[0-9]', natureza_receita_codigo_completo)
    )
    "
  ),
  
  # CONTRIB. EMPREGADORES E TRAB. PARA SEG. SOCIAL
  `11  Contrib. Empregadores e Trabalhadores para Seguridade Social` = list(
    criterio = "
    nre1_categoria_economica_codigo == 1 & fonte_recursos_codigo == '054' & 
    !natureza_receita_codigo_completo %in% c(
      '19900300', '19900310', '19900311', '19900312', '19900313', '19900314',
      '19990300', '19990301', '19990302', '19990303', '19990304'
    )
    "
  ),
  
  # CONTRIB. DO SERVIDOR PARA O PLANO DE PREVIDÊNCIA
  `12  Contrib. Plano Seguridade Social do Servidor` = list(
    criterio = "
    nre1_categoria_economica_codigo == 1 & (
      fonte_recursos_codigo %in% c('055', '056') |
      natureza_receita_codigo_completo == '12150116'
    )
    "
  ),
  
  # COMPENSAÇÃO FINANC. ENTRE REGIMES PREVIDÊNCIA
  `13  Compensação Financeira entre Regimes RPPS` = list(
    criterio = "
    nre1_categoria_economica_codigo == 1 & natureza_receita_codigo_completo %in% c(
      '19900300', '19900310', '19900311', '19900312', '19900313', '19900314',
      '19990300', '19990301', '19990302', '19990303', '19990304'
    )
    "
  ),
  
  # CONTRIB. DOS MILITARES PARA O CUSTEIO DAS PENSÕES
  `14  Contrib. para Custeio Pensões Militares` = list(
    criterio = "
    nre1_categoria_economica_codigo == 1 & (
      grepl('^1210051', natureza_receita_codigo_completo) |
      grepl('^1215041', natureza_receita_codigo_completo) |
      grepl('^121911', natureza_receita_codigo_completo)
    )
    "
  ),
  
  # CONTRIBUIÇÕES PARA PIS/PASEP
  `15  Contribuição para PIS/PASEP` = list(
    criterio = "
    nre1_categoria_economica_codigo == 1 & (
      (grepl('^1210091|^1212', natureza_receita_codigo_completo) & 
       !fonte_recursos_codigo %in% c('055', '056', '054')) |
      (!grepl('^1210091|^1212', natureza_receita_codigo_completo) & 
       fonte_recursos_codigo %in% c('040', '041'))
    )
    "
  ),
  
  # DEDUÇÕES DAS RECEITAS
  `16  Deduções das Receitas` = list(
    criterio = "
    (grepl('^1212', natureza_receita_codigo_completo) & fonte_recursos_codigo %in% c('031', '032', '040', '041')) |
    (grepl('^1214', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054') |
    (grepl('^1215', natureza_receita_codigo_completo) & fonte_recursos_codigo %in% c('023', '032', '055', '056')) |
    (grepl('^1219', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054') |
    (grepl('^1911', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054') |
    (grepl('^1922', natureza_receita_codigo_completo) & fonte_recursos_codigo %in% c('040', '054', '056')) |
    (grepl('^1923', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054') |
    (grepl('^1999', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054')
    "
  )
)






datatable(aplicar_criterios(dados_receita, criterios_rreo_A03_rcl_receitas,agrupamento = c("item_informacao_nome", "item_informacao_codigo") ))




criterios_dcl_1 <- list(
  
  `08 - PRECATÓRIOS POSTERIORES A 05/05/2000` = list(
    criterio = "
    conta_contabil_numero %in% c(631100000, 631200000, 631300000, 631510000, 631520000, 632100000, 
                                522110101, 522110201, 522110209, 522120101, 522120103, 522120201, 
                                522120202, 522120203, 522120301, 522120302, 522120303, 522190100, 
                                522190200, 522190300, 522190400, 522190101, 522190109, 522190201, 
                                522190209, 522190301, 522190309) &
    (acao_governo_codigo %in% c('0005', '00U9', '00UP', '0EC8', '0EC7', '00WU') |
     (unidade_orcamentaria_codigo == '71103' & acao_governo_codigo == '0Z01'))
    "
  ),
  
  `25 - RESTOS A PAGAR PROCESSADOS` = list(
    criterio = "
    acao_governo_codigo != '0005' &
    ((conta_contabil_numero %in% c(622920103, 622920107, 632100000, 631300000)) |
     (conta_contabil_numero %in% c(622920102, 622920106, 631200000) & 
      ug_executora_codigo != '170600'))
    "
  )
)

# ===============================================================================
# CRITÉRIOS PARA DCL_2A (Dados Patrimoniais - Base Principal)
# ===============================================================================
criterios_dcl_2a <- list(
  
  `02 - APLICAÇÃO EM TÍTULOS PÚBLICOS` = list(
    criterio = "
    str_starts(as.character(conta_contabil_numero), '1111150') &
    orgao_uge_tipo_administracao_codigo %in% c(3, 4, 5, 6, 8) &
    !conta_contabil_numero %in% c('111115005', '111115011', '111115012') &
    orgao_uge_codigo != '25901'
    "
  ),
  
  `05 - DÍVIDA MOBILIÁRIA EXTERNA` = list(
    criterio = "
    conta_contabil_numero %in% c(899913903, 899913904)
    "
  ),
  
  `06 - OPERAÇÕES DE EQUALIZAÇÃO CAMBIAL` = list(
    criterio = "
    conta_contabil_numero %in% c(218912902, 218942902, 218952902, 218912901)
    "
  ),
  
  `07 - DEMAIS DÍVIDAS CONTRATUAIS` = list(
    criterio = "
    isf_lancamento %in% c('P') &
    (conta_contabil_numero %in% c(222210200, 212210300, 222110200, 212110301, 212110303, 212510103,
                                  212140303, 212150303, 212540103, 212550103, 212140301, 212150301,
                                  217310301, 217310602, 217350402, 227310301, 212110700, 212210601,
                                  212310201, 212310202, 212410201, 222310101, 222310102, 222410101,
                                  217710101, 227710101) |
     (conta_contabil_numero %in% c(227310401) & ug_executora_codigo != '170512'))
    "
  ),
  
  `10 - PASSIVOS POR INSUFICIÊNCIA DE RECURSOS` = list(
    criterio = "
    conta_contabil_numero %in% c(211110101, 211419800, 223110100, 211210100, 213140400, 
                                213150400, 214119900, 213110400, 211449800, 211459800) &
    (is.na(isf_lancamento) | isf_lancamento %in% c('P'))
    "
  ),
  
  `11 - DEPÓSITOS DO TN (EM BCB) - DEDUÇÃO` = list(
    criterio = "
    str_starts(as.character(conta_contabil_numero), '1111102') |
    str_starts(as.character(conta_contabil_numero), '1111103') |
    str_starts(as.character(conta_contabil_numero), '1111104')
    "
  ),
  
  `12 - DEPÓSITOS À VISTA - DEDUÇÃO` = list(
    criterio = "
    (str_starts(as.character(conta_contabil_numero), '1111119') |
     str_starts(as.character(conta_contabil_numero), '1112102') |
     str_starts(as.character(conta_contabil_numero), '1112103') |
     str_starts(as.character(conta_contabil_numero), '1112150') |
     str_starts(as.character(conta_contabil_numero), '1112152')) &
    orgao_uge_codigo != '25901' &
    ug_executora_codigo != '380916'
    "
  ),
  
  `13 - DISPONIBILIDADE FAT - DEDUÇÃO` = list(
    criterio = "
    ug_executora_codigo == '380916' &
    (str_starts(as.character(conta_contabil_numero), '1111119') |
     str_starts(as.character(conta_contabil_numero), '1124103') |
     str_starts(as.character(conta_contabil_numero), '1135407') |
     str_starts(as.character(conta_contabil_numero), '1135113') |
     str_starts(as.character(conta_contabil_numero), '1135115') |
     str_starts(as.character(conta_contabil_numero), '1124101') |
     str_starts(as.character(conta_contabil_numero), '1135111') |
     str_starts(as.character(conta_contabil_numero), '1135107') |
     str_starts(as.character(conta_contabil_numero), '11121') |
     str_starts(as.character(conta_contabil_numero), '1135114') |
     str_starts(as.character(conta_contabil_numero), '1135112') |
     str_starts(as.character(conta_contabil_numero), '1135116') |
     str_starts(as.character(conta_contabil_numero), '1211503') |
     str_starts(as.character(conta_contabil_numero), '1211403') |
     str_starts(as.character(conta_contabil_numero), '1211103') |
     str_starts(as.character(conta_contabil_numero), '1135507') |
     str_starts(as.character(conta_contabil_numero), '1212105') |
     conta_contabil_numero %in% c(111115009, 111115011, 111115014, 111115015, 111115016, 111115006))
    "
  ),
  
  `14 - APLICAÇÕES EM FUNDOS DIVERSOS 1 - DEDUÇÃO` = list(
    criterio = "
    conta_contabil_numero == 111215100 &
    !orgao_uge_codigo %in% c('37904', '25915')
    "
  ),
  
  `15 - APLICAÇÕES EM FUNDOS DIVERSOS 2 - DEDUÇÃO` = list(
    criterio = "
    orgao_uge_tipo_administracao_codigo == 7 &
    !orgao_uge_codigo %in% c('37904', '25915') &
    str_starts(as.character(conta_contabil_numero), '23')
    "
  ),
  
  `16 - APLICAÇÕES EM FUNDOS DIVERSOS 3 - DEDUÇÃO` = list(
    criterio = "
    orgao_uge_tipo_administracao_codigo %in% c(7) &
    !orgao_uge_codigo %in% c('37904', '25915') &
    (str_starts(as.character(conta_contabil_numero), '1111102') |
     str_starts(as.character(conta_contabil_numero), '1111103') |
     str_starts(as.character(conta_contabil_numero), '1111104') |
     conta_contabil_numero %in% c(111210200, 111210300, 111215000, 111215200) |
     (str_starts(as.character(conta_contabil_numero), '1111119') & 
      ug_executora_codigo != '380916') |
     c_con_subgrupo_3_nome %in% c('INVESTIMENTOS', 'IMOBILIZADO', 'INTANGIVEL', 'DIFERIDO'))
    "
  ),
  
  `24 - RESULTADO POSITIVO TN/BCB - DEDUÇÃO` = list(
    criterio = "
    conta_contabil_numero %in% c(113813001, 113813002)
    "
  ),
  
  `26 - DÍVIDA MOBILIÁRIA TN INTERNA (INTRA) - DEDUÇÃO` = list(
    criterio = "
    conta_contabil_numero == 222120101
    "
  )
)

# ===============================================================================
# CRITÉRIOS PARA DCL_2B (Dados Patrimoniais - Base Específica)
# ===============================================================================
criterios_rgf_A02_dcl_df2b <- list(
  
  `01 - DÍVIDA MOBILIÁRIA TN INTERNA - PARTE 1` = list(
    criterio = "
    conta_contabil_numero %in% c('899913900', '899913901', '899913902', '899913903', 
                                '899913904', '899913905', '899913906') &
    entidade_c_cor_numero %in% c('DP1000001', 'DP1400001', 'DP1500001', 'DP1700001', 
                                'DP1800001', 'DP2000001', 'DP2300007', 'DP2400001', 
                                'DP2600001', 'DP2800001', 'DP3000001', 'DP3400001', 
                                'DP5000001', 'DP5500001', 'DP5800001', 'DP6100001', 
                                'DP6200001', 'DP6300001', 'DP6600001', 'DP7000001', 
                                'DP8000001', 'DP9000001', 'DP1200001')
    "
  ),
  
  `01 - DÍVIDA MOBILIÁRIA TN INTERNA - PARTE 2` = list(
    criterio = "
    conta_contabil_numero %in% c('899913900', '899913901', '899913902', '899913903', 
                                '899913904', '899913905', '899913906') &
    entidade_c_cor_numero == 'DP9102001'
    "
  ),
  
  `03 - DÍVIDA MOBILIÁRIA DO TN (EM BCB)` = list(
    criterio = "
    conta_contabil_numero %in% c('899913901', '899913902', '899913907', '899913908') &
    entidade_c_cor_numero %in% c('DP1500010', 'DP1700010', 'DP1800010', 'DP2300010', 
                                'DP5500010', 'DP7000010', 'DP9000010', 'DP3201450')
    "
  ),
  
  `04 - DÍVIDA SECURITIZADA` = list(
    criterio = "
    (conta_contabil_numero %in% c('899913900', '899913901', '899913902', '899913903', 
                                 '899913904', '899913905', '899913906') &
     entidade_c_cor_numero %in% c('DP3100001', 'DP3200001', 'DP3200002', 'DP3201031', 
                                 'DP3201032', 'DP3201059', 'DP3201077', 'DP3201078', 
                                 'DP3201080', 'DP3201081', 'DP3201145', 'DP3201202', 
                                 'DP3201222', 'DP3201228', 'DP3201233', 'DP3201250', 
                                 'DP3201256', 'DP3201257', 'DP3201258', 'DP3201259', 
                                 'DP3201260', 'DP3201262', 'DP3201271', 'DP3201272', 
                                 'DP3201275', 'DP3201276', 'DP3201277', 'DP3201280', 
                                 'DP3201281', 'DP3201296', 'DP3201299', 'DP3201362', 
                                 'DP3201368', 'DP3201378', 'DP3201390')) |
    conta_contabil_numero %in% c('212110202', '222110102')
    "
  ),
  
  `09 - DÍVIDA ASSUMIDA PELA UNIÃO` = list(
    criterio = "
    ug_executora_codigo == '170512' &
    conta_contabil_numero %in% c('218912600', '228911600', '227310401') &
    isf_lancamento == 'P' &
    conta_corrente != 'PF1705118'
    "
  ),
  
  `17 - DÍVIDAS RENEGOCIADAS - DEDUÇÃO` = list(
    criterio = "
    ug_executora_codigo == '170512' &
    conta_contabil_numero %in% c('121110301', '112410100', '121110318', '112410600', 
                                '121140301', '121150301', '112440100', '112450100', 
                                '121140318', '121150318', '112440600', '112450600', 
                                '112410401', '112450401', '112440401', '121249818', 
                                '113814200', '113844200', '113854200') &
    (entidade_c_cor_numero %in% c('PF1705320', 'PF1705524', 'PF1705528', 'PF1705546', 
                                 'PF1705547', 'PF1705548', 'PF1705406', 'PF1705525', 
                                 'PF1705529', 'PF1705544', 'PF1705545') |
     str_detect(entidade_c_cor_nome, '9.496/97') |
     str_detect(entidade_c_cor_nome, '2.185/2001'))
    "
  ),
  
  `18 - CRÉDITOS LEI 8.727/93 - DEDUÇÃO` = list(
    criterio = "
    ug_executora_codigo == '170512' &
    conta_contabil_numero %in% c('121110301', '112410100', '121110318', '112410600', 
                                '121140301', '121150301', '112440100', '112450100', 
                                '121140318', '121150318', '112440600', '112450600', 
                                '112410401', '121219818', '112450401', '112440401', 
                                '121249818', '113814200', '113844200', '113854200') &
    (entidade_c_cor_numero %in% c('PF1705109', 'PF1705536', 'TN0000016', 'TN0000017') |
     str_detect(entidade_c_cor_nome, '8.727/93'))
    "
  ),
  
  `19 - DÍVIDA EXTERNA RENEGOCIADA - DEDUÇÃO` = list(
    criterio = "
    ug_executora_codigo == '170512' &
    conta_contabil_numero %in% c('121110301', '112410100', '121110318', '112410600', 
                                '121140301', '121150301', '112440100', '112450100', 
                                '121140318', '121150318', '112440600', '112450600', 
                                '112410401', '121219818', '112450401', '112440401', 
                                '121249818', '113844200', '113814200', '113854200', 
                                '121259818') &
    (entidade_c_cor_numero %in% c('PF1705104', 'PF1705114', 'PF1705117', 'PF1705521', 
                                 'PF1705534', 'PF1705116', 'PF1705531', 'PF1705532', 
                                 'PF1705113', 'PF1701536', 'PF1705520', 'PF1705533', 
                                 'PF1705464', 'PF1705534', 'PF1705119', 'PF1705384') |
     str_detect(entidade_c_cor_nome, 'DMLP') |
     str_detect(entidade_c_cor_nome, 'FRANCA') |
     str_detect(entidade_c_cor_nome, 'EXTER') |
     str_detect(entidade_c_cor_nome, 'FRANÇA') |
     str_detect(entidade_c_cor_nome, 'MF 030') |
     str_detect(entidade_c_cor_nome, 'BIB'))
    "
  ),
  
  `20 - DEMAIS DÍVIDAS RENEGOCIADAS - DEDUÇÃO` = list(
    criterio = "
    ug_executora_codigo == '170512' &
    conta_contabil_numero %in% c('121110301', '112410100', '121110318', '112410600', 
                                '121140301', '121150301', '112440100', '112450100', 
                                '121140318', '121150318', '112440600', '112450600', 
                                '112410401', '112450401', '112440401', '121249818', 
                                '113844200', '113814200', '113854200', '121259818')
    "
  ),
  
  `21 - AJUSTES PARA PERDAS (POSITIVO)` = list(
    criterio = "
    ug_executora_codigo == '170512' &
    conta_contabil_numero %in% c('112940401', '112950401', '113940101', '112910401', 
                                '113950101', '121119902', '121149904', '121159904', 
                                '121119904', '121249903', '121259903')
    "
  ),
  
  `22 - OUTROS CRÉDITOS BANCÁRIOS - DEDUÇÃO` = list(
    criterio = "
    ug_executora_codigo %in% c('170705', '170526', '170700') &
    conta_contabil_numero %in% c('112410301', '112410303', '112440301', '112450301', 
                                '112440303', '112450303', '112410100', '121110301', 
                                '121110314', '121110308', '121140301', '121150301', 
                                '121140308', '121150308', '112411300', '121110316', 
                                '121110320', '112410302', '112410304', '112410201', 
                                '112410203', '112410403', '121110312')
    "
  ),
  
  `23 - OUTROS CRÉDITOS BANCÁRIOS - AJUSTES (POSITIVO)` = list(
    criterio = "
    ug_executora_codigo %in% c('170705', '170526', '170700') &
    conta_contabil_numero %in% c('112910401', '121119904', '121119907', '112910403')
    "
  )
)

datatable(aplicar_criterios(dcl_2b, criterios_rgf_A02_dcl_df2b))


criterios_rreo_T02_pessoal_liquidado <- list(
  
  # APLICAÇÃO DIRETA (será calculado por soma dos subitens)
  `01  Aplicação Direta` = list(
    criterio = "FALSE"  # Será calculado por soma posterior
  ),
  
  # A DETALHAR
  `02  A Detalhar` = list(
    criterio = "elemento_despesa_codigo %in% c('00')"
  ),
  
  # PESSOAL CIVIL (será calculado por soma dos subitens)
  `03  Pessoal Civil` = list(
    criterio = "FALSE"  # Será calculado por soma posterior
  ),
  
  # PESSOAL CIVIL - VENCIMENTOS E VANTAGENS FIXAS
  `04  Pessoal Civil - Vencimentos e Vantagens Fixas` = list(
    criterio = "elemento_despesa_codigo %in% c('11')"
  ),
  
  # PESSOAL CIVIL - OUTRAS DESPESAS VARIÁVEIS
  `05  Pessoal Civil - Outras Despesas Variáveis` = list(
    criterio = "elemento_despesa_codigo %in% c('16')"
  ),
  
  # PESSOAL CIVIL - APOSENTADORIA
  `06  Pessoal Civil - Aposentadoria` = list(
    criterio = "elemento_despesa_codigo %in% c('01') & (orgao_uge_orgao_maximo_codigo %notin% c('52000') | (orgao_uge_orgao_maximo_codigo %in% c('52000') & acao_governo_codigo %in% c('0181')))"
  ),
  
  # PESSOAL CIVIL - PENSÕES
  `07  Pessoal Civil - Pensões` = list(
    criterio = "elemento_despesa_codigo %in% c('03') & (orgao_uge_orgao_maximo_codigo %notin% c('52000') | (orgao_uge_orgao_maximo_codigo %in% c('52000') & acao_governo_codigo %in% c('0181')))"
  ),
  
  # PESSOAL CIVIL - CONTRIBUIÇÕES A ENTIDADES FECHADAS DE PREVIDÊNCIA
  `08  Pessoal Civil - Contribuições a Entidades Fechadas de Previdência` = list(
    criterio = "elemento_despesa_codigo %in% c('07') & orgao_uge_orgao_maximo_codigo %notin% c('52000')"
  ),
  
  # PESSOAL CIVIL - OBRIGAÇÕES PATRONAIS
  `09  Pessoal Civil - Obrigações Patronais` = list(
    criterio = "elemento_despesa_codigo %in% c('13') & orgao_uge_orgao_maximo_codigo %notin% c('52000')"
  ),
  
  # PESSOAL CIVIL - OUTRAS APLICAÇÕES
  `10  Pessoal Civil - Outras Aplicações` = list(
    criterio = "
    (elemento_despesa_codigo %notin% c('00', '01', '03') & 
     orgao_uge_orgao_maximo_codigo %in% c('52000') & 
     acao_governo_codigo %in% c('0181')) | 
    (elemento_despesa_codigo %notin% c('00', '01', '03', '07', '11', '12', '13', '16', '17', '41') & 
     orgao_uge_orgao_maximo_codigo %notin% c('52000'))
    "
  ),
  
  # PESSOAL MILITAR (será calculado por soma dos subitens)
  `11  Pessoal Militar` = list(
    criterio = "FALSE"  # Será calculado por soma posterior
  ),
  
  # PESSOAL MILITAR - VENCIMENTOS E VANTAGENS FIXAS
  `12  Pessoal Militar - Vencimentos e Vantagens Fixas` = list(
    criterio = "elemento_despesa_codigo %in% c('12')"
  ),
  
  # PESSOAL MILITAR - OUTRAS DESPESAS VARIÁVEIS
  `13  Pessoal Militar - Outras Despesas Variáveis` = list(
    criterio = "elemento_despesa_codigo %in% c('17')"
  ),
  
  # PESSOAL MILITAR - REFORMAS
  `14  Pessoal Militar - Reformas` = list(
    criterio = "elemento_despesa_codigo %in% c('01') & orgao_uge_orgao_maximo_codigo %in% c('52000') & acao_governo_codigo %notin% c('0181')"
  ),
  
  # PESSOAL MILITAR - PENSÕES
  `15  Pessoal Militar - Pensões` = list(
    criterio = "elemento_despesa_codigo %in% c('03') & orgao_uge_orgao_maximo_codigo %in% c('52000') & acao_governo_codigo %notin% c('0181')"
  ),
  
  # PESSOAL MILITAR - OBRIGAÇÕES PATRONAIS
  `16  Pessoal Militar - Obrigações Patronais` = list(
    criterio = "elemento_despesa_codigo %in% c('13') & orgao_uge_orgao_maximo_codigo %in% c('52000')"
  ),
  
  # PESSOAL MILITAR - OUTRAS APLICAÇÕES (+)
  `17  Pessoal Militar - Outras Aplicações (+)` = list(
    criterio = "elemento_despesa_codigo %notin% c('00', '01', '03', '11', '12', '13', '16', '17', '41') & orgao_uge_orgao_maximo_codigo %in% c('52000')"
  ),
  
  # PESSOAL MILITAR - OUTRAS APLICAÇÕES (-)
  `18  Pessoal Militar - Outras Aplicações (-)` = list(
    criterio = "elemento_despesa_codigo %notin% c('00', '01', '03', '11') & orgao_uge_orgao_maximo_codigo %in% c('52000') & acao_governo_codigo %in% c('0181')"
  ),
  
  # TRANSFERÊNCIAS INTERGOVERNAMENTAIS
  `19  Transferências Intergovernamentais` = list(
    criterio = "elemento_despesa_codigo %in% c('41')"
  ),
  
  # TRANSFERÊNCIAS A ESTADOS E AO DF
  `20  Transferências a Estados e ao DF` = list(
    criterio = "elemento_despesa_codigo %in% c('41')"
  )
)


datatable(aplicar_criterios(df = dados_despesa %>%  filter( grupo_despesa_codigo_grupo == 1,  orgao_uge_orcam_fiscal_s_n == "PERTENCE",   mes_lancamento == mes_filtro,   item_informacao_codigo == "25"
                              ), criterios_rreo_T02_pessoal_liquidado, agrupamento = "orgao_uge_tipo_administracao_nome"))
