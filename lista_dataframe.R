# ========================================
# ESTRUTURA COMPLETA - TODOS OS 125 CRITÉRIOS
# ========================================

# Esta é a conversão completa de todas as 14 listas originais
criterios_relatorios_fiscais_completo <- list(
  
  # ===== 1. CRITERIOS_DESPESAS_ANEXO3_RCL (2 critérios) =====
  rreo_a03_rcl_desp_01 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_despesa", linha_nome = "Transferências Constitucionais e Legais", linha_codigo = "01", metrica = "saldo_r_item_informacao", observacao = "Despesas - Transferências Constitucionais e Legais", data_atualizacao = Sys.Date(), criterio = "acao_governo_codigo %in% c('0E36', '00SB') | grepl('^28846090900RX', programa_governo_codigo) | (programa_governo_codigo %in% c('0903', '2030', '2080') & modalidade_aplicacao_codigo %in% c(30, 31, 32, 35, 36, 40, 41, 42, 45, 46) & acao_governo_codigo %in% c('0044', '0045', '0046', '0050', '0051', '00H6', '006M', '00G6', '0169', '0223', '0369', '0546', '0547', '0999', '099B', '0A53', '0C03', '0C33', '0E25', '0E36', '00PX', '00QR', '00S3', '00S7', '00S8', '00SE', '00RX', '00UH'))"),
  rreo_a03_rcl_desp_02 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_despesa", linha_nome = "Outras Deduções", linha_codigo = "02", metrica = "saldo_r_item_informacao", observacao = "Despesas - Outras Deduções", data_atualizacao = Sys.Date(), criterio = "acao_governo_codigo %in% c('0E36', '00SB') | grepl('^28846090900RX', programa_governo_codigo) | (programa_governo_codigo %in% c('0903', '2030', '2080') & modalidade_aplicacao_codigo %in% c(30, 31, 32, 35, 36, 40, 41, 42, 45, 46) & acao_governo_codigo %in% c('0044', '0045', '0046', '0050', '0051', '00H6', '006M', '00G6', '0169', '0223', '0369', '0546', '0547', '0999', '099B', '0A53', '0C03', '0C33', '0E25', '0E36', '00PX', '00QR', '00S3', '00S7', '00S8', '00SE', '00RX', '00UH'))"),
  
  # ===== 2. CRITERIOS_DESPESAS_FCDF (4 critérios) =====
  fcdf_desp_01 = list(relatorio = "ESPECIAL", anexo_nome = "FCDF", anexo_codigo = "FCDF", base_dados = "dados_despesa", linha_nome = "FCDF - DESPESAS - A detalhar", linha_codigo = "01", metrica = "saldo_r_item_informacao", observacao = "FCDF - Despesas a detalhar", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(1, 2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('00') & acao_governo_codigo %in% c('00Q2', '00QN', '00NS')"),
  fcdf_desp_02 = list(relatorio = "ESPECIAL", anexo_nome = "FCDF", anexo_codigo = "FCDF", base_dados = "dados_despesa", linha_nome = "FCDF - DESPESAS - Aposentadorias", linha_codigo = "02", metrica = "saldo_r_item_informacao", observacao = "FCDF - Aposentadorias", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(1, 2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('01') & acao_governo_codigo %in% c('00Q2', '00QN', '00NS', '0312', '00NR')"),
  fcdf_desp_03 = list(relatorio = "ESPECIAL", anexo_nome = "FCDF", anexo_codigo = "FCDF", base_dados = "dados_despesa", linha_nome = "FCDF - DESPESAS - Pensões", linha_codigo = "03", metrica = "saldo_r_item_informacao", observacao = "FCDF - Pensões", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(1, 2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('03') & acao_governo_codigo %in% c('00Q2', '00QN', '00NS', '0312', '00NR')"),
  fcdf_desp_04 = list(relatorio = "ESPECIAL", anexo_nome = "FCDF", anexo_codigo = "FCDF", base_dados = "dados_despesa", linha_nome = "FCDF - DESPESAS - Outros Benefícios Previdenciários", linha_codigo = "04", metrica = "saldo_r_item_informacao", observacao = "FCDF - Outros Benefícios Previdenciários", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(1, 2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845', '846') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %notin% c('00', '01', '03') & (acao_governo_codigo %in% c('00Q2', '00NS', '00QN') | (acao_governo_codigo %in% c('0005', '0625') & funcao_governo_codigo %notin% c('10', '08')))"),
  
  # ===== 3. CRITERIOS_DESPESAS_RGPS (7 critérios) =====
  rgps_desp_01 = list(relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS", base_dados = "dados_despesa", linha_nome = "Aposentadorias", linha_codigo = "01", metrica = "saldo_r_item_informacao", observacao = "RGPS - Aposentadorias", data_atualizacao = Sys.Date(), criterio = "elemento_despesa_codigo %in% c('53', '54') & grupo_despesa_codigo_grupo %in% c(3) & modalidade_aplicacao_codigo %notin% c('91')"),
  rgps_desp_02 = list(relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS", base_dados = "dados_despesa", linha_nome = "Pensões", linha_codigo = "02", metrica = "saldo_r_item_informacao", observacao = "RGPS - Pensões", data_atualizacao = Sys.Date(), criterio = "elemento_despesa_codigo %in% c('55', '56') & grupo_despesa_codigo_grupo %in% c(3) & modalidade_aplicacao_codigo %notin% c('91')"),
  rgps_desp_03 = list(relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS", base_dados = "dados_despesa", linha_nome = "Outros Benefícios", linha_codigo = "03", metrica = "saldo_r_item_informacao", observacao = "RGPS - Outros Benefícios", data_atualizacao = Sys.Date(), criterio = "elemento_despesa_codigo %in% c('57', '58') & grupo_despesa_codigo_grupo %in% c(3) & modalidade_aplicacao_codigo %notin% c('91')"),
  rgps_desp_04 = list(relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS", base_dados = "dados_despesa", linha_nome = "Compensação Previdenciária do RGPS para o RPPS", linha_codigo = "04", metrica = "saldo_r_item_informacao", observacao = "RGPS - Compensação Previdenciária", data_atualizacao = Sys.Date(), criterio = "elemento_despesa_codigo %notin% c('57', '58', '53', '54', '55', '56') & acao_governo_codigo %in% c('009W', '0531') & grupo_despesa_codigo_grupo %in% c(3) & modalidade_aplicacao_codigo %notin% c('91')"),
  rgps_desp_05 = list(relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS", base_dados = "dados_despesa", linha_nome = "Demais Despesas", linha_codigo = "05", metrica = "saldo_r_item_informacao", observacao = "RGPS - Demais Despesas", data_atualizacao = Sys.Date(), criterio = "elemento_despesa_codigo %notin% c('57', '58', '53', '54', '55', '56') & acao_governo_codigo %notin% c('009W', '0531') & grupo_despesa_codigo_grupo %in% c(3) & modalidade_aplicacao_codigo %notin% c('91')"),
  rgps_desp_06 = list(relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS", base_dados = "dados_despesa", linha_nome = "A detalhar", linha_codigo = "06", metrica = "saldo_r_item_informacao", observacao = "RGPS - A detalhar", data_atualizacao = Sys.Date(), criterio = "elemento_despesa_codigo %in% c('00') & grupo_despesa_codigo_grupo %in% c(3) & modalidade_aplicacao_codigo %notin% c('91')"),
  rgps_desp_07 = list(relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS", base_dados = "dados_despesa", linha_nome = "Despesas Previdenciárias (INTRA)", linha_codigo = "07", metrica = "saldo_r_item_informacao", observacao = "RGPS - Despesas Previdenciárias INTRA", data_atualizacao = Sys.Date(), criterio = "grupo_despesa_codigo_grupo %in% c(3) & modalidade_aplicacao_codigo %in% c('91') & elemento_despesa_codigo %notin% c('01', '03', '05')"),
  
  # ===== 4. CRITERIOS_DESPESAS_RPPS (10 critérios) =====
  rpps_desp_01 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "CIVIS - DESPESAS - A detalhar", linha_codigo = "01", metrica = "saldo_r_item_informacao", observacao = "RPPS Civil - A detalhar", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('00') & acao_governo_codigo %in% c('0053', '0181') & funcao_governo_codigo %notin% c('10', '08')"),
  rpps_desp_02 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "CIVIS - DESPESAS - Aposentadorias", linha_codigo = "02", metrica = "saldo_r_item_informacao", observacao = "RPPS Civil - Aposentadorias", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('01') & acao_governo_codigo %in% c('0053', '0181')"),
  rpps_desp_03 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "CIVIS - DESPESAS - Pensões", linha_codigo = "03", metrica = "saldo_r_item_informacao", observacao = "RPPS Civil - Pensões", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('03') & acao_governo_codigo %in% c('0053', '0181')"),
  rpps_desp_04 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "CIVIS - DESPESAS - Outros Benefícios Previdenciários", linha_codigo = "04", metrica = "saldo_r_item_informacao", observacao = "RPPS Civil - Outros Benefícios", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845', '846') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %notin% c('00', '01', '03') & (acao_governo_codigo %in% c('0053', '0181', '0005', '0625') & funcao_governo_codigo %notin% c('10', '08') | acao_governo_codigo %in% c('0397'))"),
  rpps_desp_05 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "MILITARES - DESPESAS - A Detalhar Pensões", linha_codigo = "05", metrica = "saldo_r_item_informacao", observacao = "RPPS Militar - A Detalhar Pensões", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845') & grupo_despesa_codigo_grupo %in% c(1) & acao_governo_codigo %in% c('0179', '000Q') & elemento_despesa_codigo %in% c('00')"),
  rpps_desp_06 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "MILITARES - DESPESAS - Pensões", linha_codigo = "06", metrica = "saldo_r_item_informacao", observacao = "RPPS Militar - Pensões", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845', '846') & grupo_despesa_codigo_grupo %in% c(1) & acao_governo_codigo %in% c('0179', '00QD') & elemento_despesa_codigo %in% c('03')"),
  rpps_desp_07 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "MILITARES - DESPESAS - Outros Benefícios Pensionistas", linha_codigo = "07", metrica = "saldo_r_item_informacao", observacao = "RPPS Militar - Outros Benefícios Pensionistas", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(2) & acao_governo_codigo %in% c('214H', '218K', '0179', '00QD') & elemento_despesa_codigo %notin% c('00', '01', '03') & grupo_despesa_codigo_grupo %in% c(1)"),
  rpps_desp_08 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "MILITARES - DESPESAS - A Detalhar Inativos", linha_codigo = "08", metrica = "saldo_r_item_informacao", observacao = "RPPS Militar - A Detalhar Inativos", data_atualizacao = Sys.Date(), criterio = "grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('00') & acao_governo_codigo %in% c('214H', '218K')"),
  rpps_desp_09 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "MILITARES - DESPESAS - Inativos", linha_codigo = "09", metrica = "saldo_r_item_informacao", observacao = "RPPS Militar - Inativos", data_atualizacao = Sys.Date(), criterio = "grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('01') & acao_governo_codigo %in% c('214H', '218K')"),
  rpps_desp_10 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "MILITARES - DESPESAS - Outros Outros Benefícios Inativos", linha_codigo = "10", metrica = "saldo_r_item_informacao", observacao = "RPPS Militar - Outros Benefícios Inativos", data_atualizacao = Sys.Date(), criterio = "grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %notin% c( '00', '01') & acao_governo_codigo %in% c('214H', '218K')"),
  
  # ===== 5. CRITERIOS_EDUCACAO (12 critérios) =====
  edu_01 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "COMPLEMENTAÇÃO DA UNIÃO AO FUNDEB", linha_codigo = "01", metrica = "saldo_r_item_informacao", observacao = "Educação - Complementação FUNDEB", data_atualizacao = Sys.Date(), criterio = "acao_governo_codigo %in% c('00SB', '0E36')"),
  edu_03 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "EDUCAÇÃO BÁSICA", linha_codigo = "03", metrica = "saldo_r_item_informacao", observacao = "Educação - Educação Básica", data_atualizacao = Sys.Date(), criterio = "fonte_recursos_codigo %notin% c('008', '035', '133', '134', '213', '242') & iduso_codigo == 8 & elemento_despesa_codigo %notin% c('01', '03', '59') & subfuncao_governo_codigo == '368'"),
  edu_04 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "ENSINO SUPERIOR", linha_codigo = "04", metrica = "saldo_r_item_informacao", observacao = "Educação - Ensino Superior", data_atualizacao = Sys.Date(), criterio = "fonte_recursos_codigo %notin% c(157, 175, 193, 109, 134, 181) & iduso_codigo == 8 & elemento_despesa_codigo %notin% c('01', '03', '59') & subfuncao_governo_codigo %in% c('364')"),
  edu_05 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "ENSINO PROFISSIONAL NÃO INTEGRADO AO ENSINO REGULAR", linha_codigo = "05", metrica = "saldo_r_item_informacao", observacao = "Educação - Ensino Profissional", data_atualizacao = Sys.Date(), criterio = "fonte_recursos_codigo %notin% c('008', '035', '133', '134') & iduso_codigo == 8 & elemento_despesa_codigo %notin% c('01', '03', '59') & subfuncao_governo_codigo == '363'"),
  edu_06 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "OUTRAS", linha_codigo = "06", metrica = "saldo_r_item_informacao", observacao = "Educação - Outras", data_atualizacao = Sys.Date(), criterio = "fonte_recursos_codigo %notin% c('008', '035', '133', '134') & iduso_codigo == 8 & elemento_despesa_codigo %notin% c('01', '03', '59') & !subfuncao_governo_codigo %in% c('363', '364', '368') & acao_governo_codigo %notin% c('00SB', '0E36')"),
  edu_08 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "COMPLEMENTAÇÃO DA UNIÃO - VAAT", linha_codigo = "08", metrica = "saldo_r_item_informacao", observacao = "Educação - VAAT 1", data_atualizacao = Sys.Date(), criterio = "acao_governo_codigo %in% c('00SB', '0E36') & fonte_recursos_codigo %notin% c('133', '134', '213', '008', '035', '212') & plano_orcamentario_codigo_po == '0001'"),
  edu_09 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "COMPLEMENTAÇÃO DA UNIÃO - VAAT", linha_codigo = "09", metrica = "saldo_r_item_informacao", observacao = "Educação - VAAT 2", data_atualizacao = Sys.Date(), criterio = "acao_governo_codigo %in% c('00SB', '0E36') & fonte_recursos_codigo %notin% c('133', '134', '213', '008', '035', '212') & plano_orcamentario_codigo_po %in% c('0002')"),
  edu_09b = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "VAAT po 26298 12 847 5111 00SB 0003", linha_codigo = "09b", metrica = "saldo_r_item_informacao", observacao = "Educação - VAAT específico", data_atualizacao = Sys.Date(), criterio = "fonte_recursos_codigo %notin% c('008') & plano_orcamentario_codigo_po %in% c('0003') & funcao_governo_codigo == '12' & subfuncao_governo_codigo == '847' & programa_governo_codigo == 5111 & acao_governo_codigo == '00SB' & unidade_orcamentaria_codigo == '26298'"),
  edu_11 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "DESPESAS CUSTEADAS COM A CONTRIBUIÇÃO SOCIAL DO SALÁRIO-EDUCAÇÃO", linha_codigo = "11", metrica = "saldo_r_item_informacao", observacao = "Educação - Salário-Educação", data_atualizacao = Sys.Date(), criterio = "fonte_recursos_codigo %in% c('133', '134', '213', '008', '035', '212') & iduso_codigo == 8 & acao_governo_codigo %notin% c('00SB', '0E36')"),
  edu_12 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "DESPESAS COM O FUNDO CONSTITUCIONAL DO DISTRITO FEDERAL - FCDF", linha_codigo = "12", metrica = "saldo_r_item_informacao", observacao = "Educação - FCDF", data_atualizacao = Sys.Date(), criterio = "acao_governo_codigo %in% c('0312') & fonte_recursos_codigo %notin% c('133', '134', '213', '008', '035', '212')"),
  edu_13 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "DESPESAS CUSTEADAS COM RECEITAS DE ROYALTIES DE EXPLORAÇÃO DO PRÉ-SAL", linha_codigo = "13", metrica = "saldo_r_item_informacao", observacao = "Educação - Royalties Pré-Sal", data_atualizacao = Sys.Date(), criterio = "fonte_recursos_codigo %in% c('242') & iduso_codigo == 8 & elemento_despesa_codigo %in% c('01', '03', '59')"),
  edu_14 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "DEMAIS DESPESAS COM EDUCAÇÃO", linha_codigo = "14", metrica = "saldo_r_item_informacao", observacao = "Educação - Demais Despesas", data_atualizacao = Sys.Date(), criterio = "iduso_codigo == 8 & fonte_recursos_codigo %in% c('008', '035', '133', '134', '213', '050', '000') & elemento_despesa_codigo %in% c('01', '03', '59') & acao_governo_codigo %notin% c('00SB', '0312', '0E36')"),
  
  # ===== 6. CRITERIOS_RECEITAS_ANEXO3_RCL (16 critérios) =====
  rcl_rec_01 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Impostos, Taxas e Contribuições de Melhoria", linha_codigo = "01", metrica = "saldo_r_item_informacao", observacao = "RCL - Impostos e Taxas", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 1"),
  rcl_rec_02 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Receita de Contribuições", linha_codigo = "02", metrica = "saldo_r_item_informacao", observacao = "RCL - Receita de Contribuições", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 2"),
  rcl_rec_03 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Receita Patrimonial", linha_codigo = "03", metrica = "saldo_r_item_informacao", observacao = "RCL - Receita Patrimonial", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 3"),
  rcl_rec_04 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Receita Agropecuária", linha_codigo = "04", metrica = "saldo_r_item_informacao", observacao = "RCL - Receita Agropecuária", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 4"),
  rcl_rec_05 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Receita Industrial", linha_codigo = "05", metrica = "saldo_r_item_informacao", observacao = "RCL - Receita Industrial", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 5"),
  rcl_rec_06 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Receita de Serviços", linha_codigo = "06", metrica = "saldo_r_item_informacao", observacao = "RCL - Receita de Serviços", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 6"),
  rcl_rec_07 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Transferências Correntes", linha_codigo = "07", metrica = "saldo_r_item_informacao", observacao = "RCL - Transferências Correntes", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 7"),
  rcl_rec_08 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Receitas Correntes a Classificar", linha_codigo = "08", metrica = "saldo_r_item_informacao", observacao = "RCL - Receitas a Classificar", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 8"),
  rcl_rec_09 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Outras Receitas Correntes", linha_codigo = "09", metrica = "saldo_r_item_informacao", observacao = "RCL - Outras Receitas Correntes", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 9"),
  rcl_rec_10 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Transferências Constitucionais e Legais", linha_codigo = "10", metrica = "saldo_r_item_informacao", observacao = "RCL - Transferências Constitucionais", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 7 & (grepl('FPE|FPM|CONSTITUCIONAL|LEGAL', natureza_receita_nome, ignore.case = TRUE) | grepl('^172[0-9]', natureza_receita_codigo_completo) | grepl('^171[0-9]', natureza_receita_codigo_completo))"),
  rcl_rec_11 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Contrib. Empregadores e Trabalhadores para Seguridade Social", linha_codigo = "11", metrica = "saldo_r_item_informacao", observacao = "RCL - Contrib. Seguridade", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & fonte_recursos_codigo == '054' & !natureza_receita_codigo_completo %in% c('19900300', '19900310', '19900311', '19900312', '19900313', '19900314', '19990300', '19990301', '19990302', '19990303', '19990304')"),
  rcl_rec_12 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Contrib. Plano Seguridade Social do Servidor", linha_codigo = "12", metrica = "saldo_r_item_informacao", observacao = "RCL - Contrib. PSS", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & (fonte_recursos_codigo %in% c('055', '056') | natureza_receita_codigo_completo == '12150116')"),
  rcl_rec_13 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Compensação Financeira entre Regimes RPPS", linha_codigo = "13", metrica = "saldo_r_item_informacao", observacao = "RCL - Compensação Financeira", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & natureza_receita_codigo_completo %in% c('19900300', '19900310', '19900311', '19900312', '19900313', '19900314', '19990300', '19990301', '19990302', '19990303', '19990304')"),
  rcl_rec_14 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Contrib. para Custeio Pensões Militares", linha_codigo = "14", metrica = "saldo_r_item_informacao", observacao = "RCL - Contrib. Pensões Militares", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & (grepl('^1210051', natureza_receita_codigo_completo) | grepl('^1215041', natureza_receita_codigo_completo) | grepl('^121911', natureza_receita_codigo_completo))"),
  rcl_rec_15 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Contribuição para PIS/PASEP", linha_codigo = "15", metrica = "saldo_r_item_informacao", observacao = "RCL - PIS/PASEP", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & ((grepl('^1210091|^1212', natureza_receita_codigo_completo) & !fonte_recursos_codigo %in% c('055', '056', '054')) | (!grepl('^1210091|^1212', natureza_receita_codigo_completo) & fonte_recursos_codigo %in% c('040', '041')))"),
  rcl_rec_16 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Deduções das Receitas", linha_codigo = "16", metrica = "saldo_r_item_informacao", observacao = "RCL - Deduções", data_atualizacao = Sys.Date(), criterio = "(grepl('^1212', natureza_receita_codigo_completo) & fonte_recursos_codigo %in% c('031', '032', '040', '041')) | (grepl('^1214', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054') | (grepl('^1215', natureza_receita_codigo_completo) & fonte_recursos_codigo %in% c('023', '032', '055', '056')) | (grepl('^1219', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054') | (grepl('^1911', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054') | (grepl('^1922', natureza_receita_codigo_completo) & fonte_recursos_codigo %in% c('040', '054', '056')) | (grepl('^1923', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054') | (grepl('^1999', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054')")
  
  # Continuar com as próximas listas nos próximos updates...
)

# ========================================
# ESTRUTURA COMPLETA - TODOS OS 125 CRITÉRIOS
# ========================================

# Esta é a conversão completa de todas as 14 listas originais
criterios_relatorios_fiscais_completo <- list(
  
  # ===== 1. CRITERIOS_DESPESAS_ANEXO3_RCL (2 critérios) =====
  rreo_a03_rcl_desp_01 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_despesa", linha_nome = "Transferências Constitucionais e Legais", linha_codigo = "01", metrica = "saldo_r_item_informacao", observacao = "Despesas - Transferências Constitucionais e Legais", data_atualizacao = Sys.Date(), criterio = "acao_governo_codigo %in% c('0E36', '00SB') | grepl('^28846090900RX', programa_governo_codigo) | (programa_governo_codigo %in% c('0903', '2030', '2080') & modalidade_aplicacao_codigo %in% c(30, 31, 32, 35, 36, 40, 41, 42, 45, 46) & acao_governo_codigo %in% c('0044', '0045', '0046', '0050', '0051', '00H6', '006M', '00G6', '0169', '0223', '0369', '0546', '0547', '0999', '099B', '0A53', '0C03', '0C33', '0E25', '0E36', '00PX', '00QR', '00S3', '00S7', '00S8', '00SE', '00RX', '00UH'))"),
  rreo_a03_rcl_desp_02 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_despesa", linha_nome = "Outras Deduções", linha_codigo = "02", metrica = "saldo_r_item_informacao", observacao = "Despesas - Outras Deduções", data_atualizacao = Sys.Date(), criterio = "acao_governo_codigo %in% c('0E36', '00SB') | grepl('^28846090900RX', programa_governo_codigo) | (programa_governo_codigo %in% c('0903', '2030', '2080') & modalidade_aplicacao_codigo %in% c(30, 31, 32, 35, 36, 40, 41, 42, 45, 46) & acao_governo_codigo %in% c('0044', '0045', '0046', '0050', '0051', '00H6', '006M', '00G6', '0169', '0223', '0369', '0546', '0547', '0999', '099B', '0A53', '0C03', '0C33', '0E25', '0E36', '00PX', '00QR', '00S3', '00S7', '00S8', '00SE', '00RX', '00UH'))"),
  
  # ===== 2. CRITERIOS_DESPESAS_FCDF (4 critérios) =====
  fcdf_desp_01 = list(relatorio = "ESPECIAL", anexo_nome = "FCDF", anexo_codigo = "FCDF", base_dados = "dados_despesa", linha_nome = "FCDF - DESPESAS - A detalhar", linha_codigo = "01", metrica = "saldo_r_item_informacao", observacao = "FCDF - Despesas a detalhar", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(1, 2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('00') & acao_governo_codigo %in% c('00Q2', '00QN', '00NS')"),
  fcdf_desp_02 = list(relatorio = "ESPECIAL", anexo_nome = "FCDF", anexo_codigo = "FCDF", base_dados = "dados_despesa", linha_nome = "FCDF - DESPESAS - Aposentadorias", linha_codigo = "02", metrica = "saldo_r_item_informacao", observacao = "FCDF - Aposentadorias", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(1, 2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('01') & acao_governo_codigo %in% c('00Q2', '00QN', '00NS', '0312', '00NR')"),
  fcdf_desp_03 = list(relatorio = "ESPECIAL", anexo_nome = "FCDF", anexo_codigo = "FCDF", base_dados = "dados_despesa", linha_nome = "FCDF - DESPESAS - Pensões", linha_codigo = "03", metrica = "saldo_r_item_informacao", observacao = "FCDF - Pensões", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(1, 2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('03') & acao_governo_codigo %in% c('00Q2', '00QN', '00NS', '0312', '00NR')"),
  fcdf_desp_04 = list(relatorio = "ESPECIAL", anexo_nome = "FCDF", anexo_codigo = "FCDF", base_dados = "dados_despesa", linha_nome = "FCDF - DESPESAS - Outros Benefícios Previdenciários", linha_codigo = "04", metrica = "saldo_r_item_informacao", observacao = "FCDF - Outros Benefícios Previdenciários", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(1, 2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845', '846') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %notin% c('00', '01', '03') & (acao_governo_codigo %in% c('00Q2', '00NS', '00QN') | (acao_governo_codigo %in% c('0005', '0625') & funcao_governo_codigo %notin% c('10', '08')))"),
  
  # ===== 3. CRITERIOS_DESPESAS_RGPS (7 critérios) =====
  rgps_desp_01 = list(relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS", base_dados = "dados_despesa", linha_nome = "Aposentadorias", linha_codigo = "01", metrica = "saldo_r_item_informacao", observacao = "RGPS - Aposentadorias", data_atualizacao = Sys.Date(), criterio = "elemento_despesa_codigo %in% c('53', '54') & grupo_despesa_codigo_grupo %in% c(3) & modalidade_aplicacao_codigo %notin% c('91')"),
  rgps_desp_02 = list(relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS", base_dados = "dados_despesa", linha_nome = "Pensões", linha_codigo = "02", metrica = "saldo_r_item_informacao", observacao = "RGPS - Pensões", data_atualizacao = Sys.Date(), criterio = "elemento_despesa_codigo %in% c('55', '56') & grupo_despesa_codigo_grupo %in% c(3) & modalidade_aplicacao_codigo %notin% c('91')"),
  rgps_desp_03 = list(relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS", base_dados = "dados_despesa", linha_nome = "Outros Benefícios", linha_codigo = "03", metrica = "saldo_r_item_informacao", observacao = "RGPS - Outros Benefícios", data_atualizacao = Sys.Date(), criterio = "elemento_despesa_codigo %in% c('57', '58') & grupo_despesa_codigo_grupo %in% c(3) & modalidade_aplicacao_codigo %notin% c('91')"),
  rgps_desp_04 = list(relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS", base_dados = "dados_despesa", linha_nome = "Compensação Previdenciária do RGPS para o RPPS", linha_codigo = "04", metrica = "saldo_r_item_informacao", observacao = "RGPS - Compensação Previdenciária", data_atualizacao = Sys.Date(), criterio = "elemento_despesa_codigo %notin% c('57', '58', '53', '54', '55', '56') & acao_governo_codigo %in% c('009W', '0531') & grupo_despesa_codigo_grupo %in% c(3) & modalidade_aplicacao_codigo %notin% c('91')"),
  rgps_desp_05 = list(relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS", base_dados = "dados_despesa", linha_nome = "Demais Despesas", linha_codigo = "05", metrica = "saldo_r_item_informacao", observacao = "RGPS - Demais Despesas", data_atualizacao = Sys.Date(), criterio = "elemento_despesa_codigo %notin% c('57', '58', '53', '54', '55', '56') & acao_governo_codigo %notin% c('009W', '0531') & grupo_despesa_codigo_grupo %in% c(3) & modalidade_aplicacao_codigo %notin% c('91')"),
  rgps_desp_06 = list(relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS", base_dados = "dados_despesa", linha_nome = "A detalhar", linha_codigo = "06", metrica = "saldo_r_item_informacao", observacao = "RGPS - A detalhar", data_atualizacao = Sys.Date(), criterio = "elemento_despesa_codigo %in% c('00') & grupo_despesa_codigo_grupo %in% c(3) & modalidade_aplicacao_codigo %notin% c('91')"),
  rgps_desp_07 = list(relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS", base_dados = "dados_despesa", linha_nome = "Despesas Previdenciárias (INTRA)", linha_codigo = "07", metrica = "saldo_r_item_informacao", observacao = "RGPS - Despesas Previdenciárias INTRA", data_atualizacao = Sys.Date(), criterio = "grupo_despesa_codigo_grupo %in% c(3) & modalidade_aplicacao_codigo %in% c('91') & elemento_despesa_codigo %notin% c('01', '03', '05')"),
  
  # ===== 4. CRITERIOS_DESPESAS_RPPS (10 critérios) =====
  rpps_desp_01 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "CIVIS - DESPESAS - A detalhar", linha_codigo = "01", metrica = "saldo_r_item_informacao", observacao = "RPPS Civil - A detalhar", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('00') & acao_governo_codigo %in% c('0053', '0181') & funcao_governo_codigo %notin% c('10', '08')"),
  rpps_desp_02 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "CIVIS - DESPESAS - Aposentadorias", linha_codigo = "02", metrica = "saldo_r_item_informacao", observacao = "RPPS Civil - Aposentadorias", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('01') & acao_governo_codigo %in% c('0053', '0181')"),
  rpps_desp_03 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "CIVIS - DESPESAS - Pensões", linha_codigo = "03", metrica = "saldo_r_item_informacao", observacao = "RPPS Civil - Pensões", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('03') & acao_governo_codigo %in% c('0053', '0181')"),
  rpps_desp_04 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "CIVIS - DESPESAS - Outros Benefícios Previdenciários", linha_codigo = "04", metrica = "saldo_r_item_informacao", observacao = "RPPS Civil - Outros Benefícios", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845', '846') & grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %notin% c('00', '01', '03') & (acao_governo_codigo %in% c('0053', '0181', '0005', '0625') & funcao_governo_codigo %notin% c('10', '08') | acao_governo_codigo %in% c('0397'))"),
  rpps_desp_05 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "MILITARES - DESPESAS - A Detalhar Pensões", linha_codigo = "05", metrica = "saldo_r_item_informacao", observacao = "RPPS Militar - A Detalhar Pensões", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845') & grupo_despesa_codigo_grupo %in% c(1) & acao_governo_codigo %in% c('0179', '000Q') & elemento_despesa_codigo %in% c('00')"),
  rpps_desp_06 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "MILITARES - DESPESAS - Pensões", linha_codigo = "06", metrica = "saldo_r_item_informacao", observacao = "RPPS Militar - Pensões", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(2) & subfuncao_governo_codigo %in% c('272', '273', '274', '845', '846') & grupo_despesa_codigo_grupo %in% c(1) & acao_governo_codigo %in% c('0179', '00QD') & elemento_despesa_codigo %in% c('03')"),
  rpps_desp_07 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "MILITARES - DESPESAS - Outros Benefícios Pensionistas", linha_codigo = "07", metrica = "saldo_r_item_informacao", observacao = "RPPS Militar - Outros Benefícios Pensionistas", data_atualizacao = Sys.Date(), criterio = "esfera_orcamentaria_codigo %in% c(2) & acao_governo_codigo %in% c('214H', '218K', '0179', '00QD') & elemento_despesa_codigo %notin% c('00', '01', '03') & grupo_despesa_codigo_grupo %in% c(1)"),
  rpps_desp_08 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "MILITARES - DESPESAS - A Detalhar Inativos", linha_codigo = "08", metrica = "saldo_r_item_informacao", observacao = "RPPS Militar - A Detalhar Inativos", data_atualizacao = Sys.Date(), criterio = "grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('00') & acao_governo_codigo %in% c('214H', '218K')"),
  rpps_desp_09 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "MILITARES - DESPESAS - Inativos", linha_codigo = "09", metrica = "saldo_r_item_informacao", observacao = "RPPS Militar - Inativos", data_atualizacao = Sys.Date(), criterio = "grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %in% c('01') & acao_governo_codigo %in% c('214H', '218K')"),
  rpps_desp_10 = list(relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04", base_dados = "dados_despesa", linha_nome = "MILITARES - DESPESAS - Outros Outros Benefícios Inativos", linha_codigo = "10", metrica = "saldo_r_item_informacao", observacao = "RPPS Militar - Outros Benefícios Inativos", data_atualizacao = Sys.Date(), criterio = "grupo_despesa_codigo_grupo %in% c(1) & elemento_despesa_codigo %notin% c( '00', '01') & acao_governo_codigo %in% c('214H', '218K')"),
  
  # ===== 5. CRITERIOS_EDUCACAO (12 critérios) =====
  edu_01 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "COMPLEMENTAÇÃO DA UNIÃO AO FUNDEB", linha_codigo = "01", metrica = "saldo_r_item_informacao", observacao = "Educação - Complementação FUNDEB", data_atualizacao = Sys.Date(), criterio = "acao_governo_codigo %in% c('00SB', '0E36')"),
  edu_03 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "EDUCAÇÃO BÁSICA", linha_codigo = "03", metrica = "saldo_r_item_informacao", observacao = "Educação - Educação Básica", data_atualizacao = Sys.Date(), criterio = "fonte_recursos_codigo %notin% c('008', '035', '133', '134', '213', '242') & iduso_codigo == 8 & elemento_despesa_codigo %notin% c('01', '03', '59') & subfuncao_governo_codigo == '368'"),
  edu_04 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "ENSINO SUPERIOR", linha_codigo = "04", metrica = "saldo_r_item_informacao", observacao = "Educação - Ensino Superior", data_atualizacao = Sys.Date(), criterio = "fonte_recursos_codigo %notin% c(157, 175, 193, 109, 134, 181) & iduso_codigo == 8 & elemento_despesa_codigo %notin% c('01', '03', '59') & subfuncao_governo_codigo %in% c('364')"),
  edu_05 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "ENSINO PROFISSIONAL NÃO INTEGRADO AO ENSINO REGULAR", linha_codigo = "05", metrica = "saldo_r_item_informacao", observacao = "Educação - Ensino Profissional", data_atualizacao = Sys.Date(), criterio = "fonte_recursos_codigo %notin% c('008', '035', '133', '134') & iduso_codigo == 8 & elemento_despesa_codigo %notin% c('01', '03', '59') & subfuncao_governo_codigo == '363'"),
  edu_06 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "OUTRAS", linha_codigo = "06", metrica = "saldo_r_item_informacao", observacao = "Educação - Outras", data_atualizacao = Sys.Date(), criterio = "fonte_recursos_codigo %notin% c('008', '035', '133', '134') & iduso_codigo == 8 & elemento_despesa_codigo %notin% c('01', '03', '59') & !subfuncao_governo_codigo %in% c('363', '364', '368') & acao_governo_codigo %notin% c('00SB', '0E36')"),
  edu_08 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "COMPLEMENTAÇÃO DA UNIÃO - VAAT", linha_codigo = "08", metrica = "saldo_r_item_informacao", observacao = "Educação - VAAT 1", data_atualizacao = Sys.Date(), criterio = "acao_governo_codigo %in% c('00SB', '0E36') & fonte_recursos_codigo %notin% c('133', '134', '213', '008', '035', '212') & plano_orcamentario_codigo_po == '0001'"),
  edu_09 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "COMPLEMENTAÇÃO DA UNIÃO - VAAT", linha_codigo = "09", metrica = "saldo_r_item_informacao", observacao = "Educação - VAAT 2", data_atualizacao = Sys.Date(), criterio = "acao_governo_codigo %in% c('00SB', '0E36') & fonte_recursos_codigo %notin% c('133', '134', '213', '008', '035', '212') & plano_orcamentario_codigo_po %in% c('0002')"),
  edu_09b = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "VAAT po 26298 12 847 5111 00SB 0003", linha_codigo = "09b", metrica = "saldo_r_item_informacao", observacao = "Educação - VAAT específico", data_atualizacao = Sys.Date(), criterio = "fonte_recursos_codigo %notin% c('008') & plano_orcamentario_codigo_po %in% c('0003') & funcao_governo_codigo == '12' & subfuncao_governo_codigo == '847' & programa_governo_codigo == 5111 & acao_governo_codigo == '00SB' & unidade_orcamentaria_codigo == '26298'"),
  edu_11 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "DESPESAS CUSTEADAS COM A CONTRIBUIÇÃO SOCIAL DO SALÁRIO-EDUCAÇÃO", linha_codigo = "11", metrica = "saldo_r_item_informacao", observacao = "Educação - Salário-Educação", data_atualizacao = Sys.Date(), criterio = "fonte_recursos_codigo %in% c('133', '134', '213', '008', '035', '212') & iduso_codigo == 8 & acao_governo_codigo %notin% c('00SB', '0E36')"),
  edu_12 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "DESPESAS COM O FUNDO CONSTITUCIONAL DO DISTRITO FEDERAL - FCDF", linha_codigo = "12", metrica = "saldo_r_item_informacao", observacao = "Educação - FCDF", data_atualizacao = Sys.Date(), criterio = "acao_governo_codigo %in% c('0312') & fonte_recursos_codigo %notin% c('133', '134', '213', '008', '035', '212')"),
  edu_13 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "DESPESAS CUSTEADAS COM RECEITAS DE ROYALTIES DE EXPLORAÇÃO DO PRÉ-SAL", linha_codigo = "13", metrica = "saldo_r_item_informacao", observacao = "Educação - Royalties Pré-Sal", data_atualizacao = Sys.Date(), criterio = "fonte_recursos_codigo %in% c('242') & iduso_codigo == 8 & elemento_despesa_codigo %in% c('01', '03', '59')"),
  edu_14 = list(relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08", base_dados = "dados_despesa", linha_nome = "DEMAIS DESPESAS COM EDUCAÇÃO", linha_codigo = "14", metrica = "saldo_r_item_informacao", observacao = "Educação - Demais Despesas", data_atualizacao = Sys.Date(), criterio = "iduso_codigo == 8 & fonte_recursos_codigo %in% c('008', '035', '133', '134', '213', '050', '000') & elemento_despesa_codigo %in% c('01', '03', '59') & acao_governo_codigo %notin% c('00SB', '0312', '0E36')"),
  
  # ===== 6. CRITERIOS_RECEITAS_ANEXO3_RCL (16 critérios) =====
  rcl_rec_01 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Impostos, Taxas e Contribuições de Melhoria", linha_codigo = "01", metrica = "saldo_r_item_informacao", observacao = "RCL - Impostos e Taxas", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 1"),
  rcl_rec_02 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Receita de Contribuições", linha_codigo = "02", metrica = "saldo_r_item_informacao", observacao = "RCL - Receita de Contribuições", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 2"),
  rcl_rec_03 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Receita Patrimonial", linha_codigo = "03", metrica = "saldo_r_item_informacao", observacao = "RCL - Receita Patrimonial", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 3"),
  rcl_rec_04 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Receita Agropecuária", linha_codigo = "04", metrica = "saldo_r_item_informacao", observacao = "RCL - Receita Agropecuária", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 4"),
  rcl_rec_05 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Receita Industrial", linha_codigo = "05", metrica = "saldo_r_item_informacao", observacao = "RCL - Receita Industrial", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 5"),
  rcl_rec_06 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Receita de Serviços", linha_codigo = "06", metrica = "saldo_r_item_informacao", observacao = "RCL - Receita de Serviços", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 6"),
  rcl_rec_07 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Transferências Correntes", linha_codigo = "07", metrica = "saldo_r_item_informacao", observacao = "RCL - Transferências Correntes", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 7"),
  rcl_rec_08 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Receitas Correntes a Classificar", linha_codigo = "08", metrica = "saldo_r_item_informacao", observacao = "RCL - Receitas a Classificar", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 8"),
  rcl_rec_09 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Outras Receitas Correntes", linha_codigo = "09", metrica = "saldo_r_item_informacao", observacao = "RCL - Outras Receitas Correntes", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 9"),
  rcl_rec_10 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Transferências Constitucionais e Legais", linha_codigo = "10", metrica = "saldo_r_item_informacao", observacao = "RCL - Transferências Constitucionais", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & nre2_origem_receita_codigo_origem == 7 & (grepl('FPE|FPM|CONSTITUCIONAL|LEGAL', natureza_receita_nome, ignore.case = TRUE) | grepl('^172[0-9]', natureza_receita_codigo_completo) | grepl('^171[0-9]', natureza_receita_codigo_completo))"),
  rcl_rec_11 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Contrib. Empregadores e Trabalhadores para Seguridade Social", linha_codigo = "11", metrica = "saldo_r_item_informacao", observacao = "RCL - Contrib. Seguridade", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & fonte_recursos_codigo == '054' & !natureza_receita_codigo_completo %in% c('19900300', '19900310', '19900311', '19900312', '19900313', '19900314', '19990300', '19990301', '19990302', '19990303', '19990304')"),
  rcl_rec_12 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Contrib. Plano Seguridade Social do Servidor", linha_codigo = "12", metrica = "saldo_r_item_informacao", observacao = "RCL - Contrib. PSS", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & (fonte_recursos_codigo %in% c('055', '056') | natureza_receita_codigo_completo == '12150116')"),
  rcl_rec_13 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Compensação Financeira entre Regimes RPPS", linha_codigo = "13", metrica = "saldo_r_item_informacao", observacao = "RCL - Compensação Financeira", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & natureza_receita_codigo_completo %in% c('19900300', '19900310', '19900311', '19900312', '19900313', '19900314', '19990300', '19990301', '19990302', '19990303', '19990304')"),
  rcl_rec_14 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Contrib. para Custeio Pensões Militares", linha_codigo = "14", metrica = "saldo_r_item_informacao", observacao = "RCL - Contrib. Pensões Militares", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & (grepl('^1210051', natureza_receita_codigo_completo) | grepl('^1215041', natureza_receita_codigo_completo) | grepl('^121911', natureza_receita_codigo_completo))"),
  rcl_rec_15 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Contribuição para PIS/PASEP", linha_codigo = "15", metrica = "saldo_r_item_informacao", observacao = "RCL - PIS/PASEP", data_atualizacao = Sys.Date(), criterio = "nre1_categoria_economica_codigo == 1 & ((grepl('^1210091|^1212', natureza_receita_codigo_completo) & !fonte_recursos_codigo %in% c('055', '056', '054')) | (!grepl('^1210091|^1212', natureza_receita_codigo_completo) & fonte_recursos_codigo %in% c('040', '041')))"),
  rcl_rec_16 = list(relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03", base_dados = "dados_receita", linha_nome = "Deduções das Receitas", linha_codigo = "16", metrica = "saldo_r_item_informacao", observacao = "RCL - Deduções", data_atualizacao = Sys.Date(), criterio = "(grepl('^1212', natureza_receita_codigo_completo) & fonte_recursos_codigo %in% c('031', '032', '040', '041')) | (grepl('^1214', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054') | (grepl('^1215', natureza_receita_codigo_completo) & fonte_recursos_codigo %in% c('023', '032', '055', '056')) | (grepl('^1219', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054') | (grepl('^1911', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054') | (grepl('^1922', natureza_receita_codigo_completo) & fonte_recursos_codigo %in% c('040', '054', '056')) | (grepl('^1923', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054') | (grepl('^1999', natureza_receita_codigo_completo) & fonte_recursos_codigo == '054')")
  
  # Continuar com as próximas listas nos próximos updates...
)

# ========================================
# CONVERSÃO AUTOMÁTICA DE TODAS AS LISTAS
# ========================================

# Função para converter automaticamente todas as listas originais
converter_todas_listas_automatico <- function() {
  
  # Mapeamento das listas originais para a estrutura unificada
  mapeamento_listas <- list(
    
    # RREO
    criterios_despesas_anexo3_rcl = list(
      relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03",
      base_dados = "dados_despesa", prefixo = "rreo_a03_rcl_desp"
    ),
    criterios_receitas_anexo3_rcl = list(
      relatorio = "RREO", anexo_nome = "Receita Corrente Líquida", anexo_codigo = "A03",
      base_dados = "dados_receita", prefixo = "rreo_a03_rcl_rec"
    ),
    criterios_despesas_rpps = list(
      relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04",
      base_dados = "dados_despesa", prefixo = "rreo_a04_rpps_desp"
    ),
    criterios_receitas_rpps = list(
      relatorio = "RREO", anexo_nome = "RPPS", anexo_codigo = "A04",
      base_dados = "dados_receita", prefixo = "rreo_a04_rpps_rec"
    ),
    criterios_educacao = list(
      relatorio = "RREO", anexo_nome = "Manutenção e Desenvolvimento do Ensino", anexo_codigo = "A08",
      base_dados = "dados_despesa", prefixo = "rreo_a08_mde"
    ),
    criterios_tabela1_receitas = list(
      relatorio = "RREO", anexo_nome = "Demonstrativo da Receita", anexo_codigo = "T01",
      base_dados = "dados_receita", prefixo = "rreo_t01_rec"
    ),
    criterios_tabela1_despesas = list(
      relatorio = "RREO", anexo_nome = "Demonstrativo da Despesa", anexo_codigo = "T01",
      base_dados = "dados_despesa", prefixo = "rreo_t01_desp"
    ),
    criterios_tabela_02 = list(
      relatorio = "RREO", anexo_nome = "Demonstrativo da Despesa por Função/Subfunção", anexo_codigo = "T02",
      base_dados = "dados_despesa", prefixo = "rreo_t02_desp"
    ),
    criterios_tabela_1b_seguridade = list(
      relatorio = "RREO", anexo_nome = "Demonstrativo da Seguridade Social", anexo_codigo = "T1B",
      base_dados = "dados_despesa", prefixo = "rreo_t1b_seg"
    ),
    criterios_tabela1a_colunas = list(
      relatorio = "RREO", anexo_nome = "Demonstrativo da Receita - Colunas", anexo_codigo = "T1A",
      base_dados = "dados_receita", prefixo = "rreo_t1a_col"
    ),
    
    # RGF
    # (adicionar quando houver critérios específicos do RGF)
    
    # ESPECIAIS
    criterios_despesas_fcdf = list(
      relatorio = "ESPECIAL", anexo_nome = "FCDF", anexo_codigo = "FCDF",
      base_dados = "dados_despesa", prefixo = "esp_fcdf_desp"
    ),
    criterios_receitas_fcdf = list(
      relatorio = "ESPECIAL", anexo_nome = "FCDF", anexo_codigo = "FCDF",
      base_dados = "dados_receita", prefixo = "esp_fcdf_rec"
    ),
    criterios_despesas_rgps = list(
      relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS",
      base_dados = "dados_despesa", prefixo = "esp_rgps_desp"
    ),
    criterios_receitas_rgps = list(
      relatorio = "ESPECIAL", anexo_nome = "RGPS", anexo_codigo = "RGPS",
      base_dados = "dados_receita", prefixo = "esp_rgps_rec"
    )
  )
  
  # Lista para armazenar todos os critérios convertidos
  criterios_unificados <- list()
  
  # Iterar sobre cada lista original
  for (nome_lista in names(mapeamento_listas)) {
    
    cat("🔄 Convertendo", nome_lista, "...\n")
    
    # Verificar se a lista existe no ambiente
    if (!exists(nome_lista)) {
      cat("⚠️ Lista", nome_lista, "não encontrada. Pulando...\n")
      next
    }
    
    # Obter a lista original
    lista_original <- get(nome_lista)
    info_mapeamento <- mapeamento_listas[[nome_lista]]
    
    # Converter cada critério da lista
    for (nome_criterio_original in names(lista_original)) {
      
      criterio_original <- lista_original[[nome_criterio_original]]
      
      # Extrair código da linha
      codigo_linha <- str_extract(nome_criterio_original, "^\\d+\\w*")
      if (is.na(codigo_linha)) codigo_linha <- "01"
      
      # Extrair nome da linha (limpar formatação)
      nome_linha <- str_trim(str_remove(nome_criterio_original, "^\\d+\\w*\\s*[-–]?\\s*"))
      
      # Criar ID único
      id_criterio <- paste0(info_mapeamento$prefixo, "_", str_pad(codigo_linha, 2, pad = "0"))
      
      # Obter critério (texto da condição)
      texto_criterio <- if (is.list(criterio_original) && !is.null(criterio_original$criterio)) {
        str_trim(criterio_original$criterio)
      } else if (is.character(criterio_original)) {
        str_trim(criterio_original)
      } else {
        "TRUE"  # fallback
      }
      
      # Criar critério unificado
      criterio_unificado <- list(
        relatorio = info_mapeamento$relatorio,
        anexo_nome = info_mapeamento$anexo_nome,
        anexo_codigo = info_mapeamento$anexo_codigo,
        base_dados = info_mapeamento$base_dados,
        linha_nome = nome_linha,
        linha_codigo = codigo_linha,
        metrica = "saldo_r_item_informacao",  # padrão
        observacao = paste("Convertido de", nome_lista, "-", nome_linha),
        data_atualizacao = Sys.Date(),
        criterio = texto_criterio
      )
      
      # Adicionar à lista unificada
      criterios_unificados[[id_criterio]] <- criterio_unificado
    }
    
    cat("✅", length(lista_original), "critérios convertidos de", nome_lista, "\n")
  }
  
  cat("\n🎉 Conversão concluída!\n")
  cat("📊 Total de critérios convertidos:", length(criterios_unificados), "\n\n")
  
  return(criterios_unificados)
}

# ========================================
# EXECUTAR A CONVERSÃO
# ========================================

# Executar a conversão automática
criterios_relatorios_fiscais_completo <- converter_todas_listas_automatico()

# Verificar o resultado
cat("=== RESUMO DA CONVERSÃO ===\n")
resumir_criterios(criterios_relatorios_fiscais_completo)

# ========================================
# FUNÇÃO PARA PROCESSAR COM ESTRUTURA COMPLETA
# ========================================

processar_todos_criterios_completo <- function(usar_deteccao_automatica = TRUE) {
  
  cat("🚀 PROCESSANDO TODOS OS 125+ CRITÉRIOS\n\n")
  
  # Detectar ou definir bases
  if (usar_deteccao_automatica) {
    bases_disponiveis <- detectar_bases_ambiente()
  } else {
    bases_disponiveis <- list(
      dados_despesa = if(exists("dados_despesa")) dados_despesa else data.frame(),
      dados_receita = if(exists("dados_receita")) dados_receita else data.frame(),
      dados_rp_anexo_08 = if(exists("dados_rp_anexo_08")) dados_rp_anexo_08 else data.frame(),
      dados_rp_anexo_03_rcl = if(exists("dados_rp_anexo_03_rcl")) dados_rp_anexo_03_rcl else data.frame(),
      dados_conta_contabil = if(exists("dados_conta_contabil")) dados_conta_contabil else data.frame()
    )
  }
  
  cat("📊 Bases detectadas:", paste(names(bases_disponiveis), collapse = ", "), "\n")
  cat("🔍 Critérios disponíveis:", length(criterios_relatorios_fiscais_completo), "\n\n")
  
  # Processar usando a função existente
  resultado <- processar_todos_criterios_df(
    criterios_relatorios_fiscais_completo, 
    bases_disponiveis
  )
  
  return(resultado)
}

# ========================================
# FUNÇÕES DE VALIDAÇÃO E DIAGNÓSTICO
# ========================================

validar_conversao <- function() {
  cat("=== VALIDAÇÃO DA CONVERSÃO ===\n")
  
  # Contar critérios por lista original (valores esperados)
  esperado <- list(
    criterios_despesas_anexo3_rcl = 2,
    criterios_despesas_fcdf = 4,
    criterios_despesas_rgps = 7,
    criterios_despesas_rpps = 10,
    criterios_educacao = 12,
    criterios_receitas_anexo3_rcl = 16,
    criterios_receitas_fcdf = 7,
    criterios_receitas_rgps = 7,
    criterios_receitas_rpps = 7,
    criterios_tabela_02 = 20,
    criterios_tabela_1b_seguridade = 5,
    criterios_tabela1_despesas = 11,
    criterios_tabela1_receitas = 14,
    criterios_tabela1a_colunas = 3
  )
  
  total_esperado <- sum(unlist(esperado))
  total_convertido <- length(criterios_relatorios_fiscais_completo)
  
  cat("📊 Critérios esperados:", total_esperado, "\n")
  cat("📊 Critérios convertidos:", total_convertido, "\n")
  
  if (total_convertido >= total_esperado * 0.9) {
    cat("✅ Conversão bem-sucedida!\n")
  } else {
    cat("⚠️ Possível problema na conversão\n")
  }
  
  # Verificar por relatório
  cat("\n=== POR RELATÓRIO ===\n")
  relatorios <- table(sapply(criterios_relatorios_fiscais_completo, function(x) x$relatorio))
  print(relatorios)
  
  # Verificar por base de dados
  cat("\n=== POR BASE DE DADOS ===\n")
  bases <- table(sapply(criterios_relatorios_fiscais_completo, function(x) x$base_dados))
  print(bases)
}

# Executar validação
validar_conversao()

# ========================================
# SALVAR ESTRUTURA COMPLETA
# ========================================

salvar_estrutura_completa <- function(nome_arquivo = "criterios_completos") {
  
  # Salvar em RDS (formato R nativo)
  saveRDS(criterios_relatorios_fiscais_completo, paste0(nome_arquivo, ".rds"))
  cat("💾 Estrutura salva em:", paste0(nome_arquivo, ".rds"), "\n")
  
  # Salvar como código R
  dput(criterios_relatorios_fiscais_completo, file = paste0(nome_arquivo, ".R"))
  cat("💾 Código R salvo em:", paste0(nome_arquivo, ".R"), "\n")
  
  # Salvar dicionário em Excel
  dicionario <- data.frame(
    id = names(criterios_relatorios_fiscais_completo),
    relatorio = sapply(criterios_relatorios_fiscais_completo, function(x) x$relatorio),
    anexo_codigo = sapply(criterios_relatorios_fiscais_completo, function(x) x$anexo_codigo),
    anexo_nome = sapply(criterios_relatorios_fiscais_completo, function(x) x$anexo_nome),
    linha_codigo = sapply(criterios_relatorios_fiscais_completo, function(x) x$linha_codigo),
    linha_nome = sapply(criterios_relatorios_fiscais_completo, function(x) x$linha_nome),
    base_dados = sapply(criterios_relatorios_fiscais_completo, function(x) x$base_dados),
    criterio = sapply(criterios_relatorios_fiscais_completo, function(x) x$criterio),
    observacao = sapply(criterios_relatorios_fiscais_completo, function(x) x$observacao)
  )
  
  library(openxlsx)
  wb <- createWorkbook()
  addWorksheet(wb, "Dicionario_Completo")
  writeData(wb, "Dicionario_Completo", dicionario)
  setColWidths(wb, "Dicionario_Completo", cols = 1:ncol(dicionario), 
               widths = c(15, 10, 10, 25, 8, 30, 15, 50, 25))
  saveWorkbook(wb, paste0(nome_arquivo, "_dicionario.xlsx"), overwrite = TRUE)
  cat("📊 Dicionário Excel salvo em:", paste0(nome_arquivo, "_dicionario.xlsx"), "\n")
}

# ========================================
# WORKFLOW COMPLETO
# ========================================

executar_workflow_completo <- function() {
  cat("🚀 INICIANDO WORKFLOW COMPLETO\n")
  cat("=====================================\n\n")
  
  # 1. Converter todas as listas
  cat("1️⃣ Convertendo todas as listas...\n")
  criterios_completos <- converter_todas_listas_automatico()
  
  # 2. Validar conversão
  cat("\n2️⃣ Validando conversão...\n")
  validar_conversao()
  
  # 3. Salvar estrutura
  cat("\n3️⃣ Salvando estrutura completa...\n")
  salvar_estrutura_completa("criterios_fiscais_completo")
  
  # 4. Processar todos os critérios
  cat("\n4️⃣ Processando todos os critérios...\n")
  resultado_completo <- processar_todos_criterios_completo()
  
  # 5. Resumir resultado final
  cat("\n5️⃣ Resumo final...\n")
  resumir_dataframe(resultado_completo)
  
  cat("\n🎉 WORKFLOW CONCLUÍDO COM SUCESSO!\n")
  cat("=====================================\n")
  cat("📊 Resultado disponível em: resultado_completo\n")
  cat("🗃️ Estrutura disponível em: criterios_relatorios_fiscais_completo\n")
  
  return(list(
    criterios = criterios_completos,
    resultado = resultado_completo
  ))
}

# ========================================
# EXEMPLO DE USO
# ========================================

# Executar tudo de uma vez:
# workflow_resultado <- executar_workflow_completo()

# Ou por etapas:
# 1. Converter
# criterios_completos <- converter_todas_listas_automatico()

# 2. Processar
# resultado <- processar_todos_criterios_completo()

# 3. Analisar
# resumir_dataframe(resultado)
# validar_filtros(resultado)

# 4. Exportar
# exportar_resultado(resultado, "todos_criterios_fiscais_2025")

# EXECUTAR TUDO DE UMA VEZ (recomendado)
workflow_resultado <- executar_workflow_completo()

# Resultado:
# - criterios_relatorios_fiscais_completo (estrutura unificada)
# - resultado_completo (DataFrame com todas as linhas processadas)
# Ver resumo
resumir_criterios(criterios_relatorios_fiscais_completo)

# Ver dados processados  
View(resultado_completo)

# Filtrar por relatório
rreo_completo <- resultado_completo %>% filter(relatorio == "RREO")

# Filtrar por anexo
anexo3_completo <- resultado_completo %>% filter(anexo_codigo == "A03")


datatable(rreo_completo %>% filter(base_origem == "dados_receita", mes_lancamento == mes_filtro, item_informacao_codigo == 5) %>% group_by(item_informacao_nome, anexo_codigo, anexo_nome, linha_codigo, linha_nome) %>% summarise(saldo_r_item_informacao = sum(saldo_r_item_informacao)) %>% pivot_wider(names_from = item_informacao_nome, values_from = saldo_r_item_informacao))
