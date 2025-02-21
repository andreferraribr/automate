library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(bsicons)
library(readxl)
library(janitor)
library(writexl)
library(scales)
library(tidyr)

# Definir operador personalizado
`%notin%` <- Negate(`%in%`)

# Configurações globais do Shiny
options(shiny.maxRequestSize = 100*1024^2)  # Tamanho máximo do arquivo de upload

# Interface do Usuário (UI)
ui <- page_sidebar(
  theme = bs_theme(
    preset = "minty",
    base_font_size = "0.9rem"
  ),
  title = "Gerador de Subsets",
  
  sidebar = sidebar(
    title = "Parâmetros do Subset",
    width = 400,
    
    fileInput("arquivo", "Carregar arquivo (CSV ou XLSX):",
              accept = c(".csv", ".xlsx", ".xls")),
    
    accordion(
      accordion_panel(
        "Seleção de Variáveis",
        selectInput("vars_group", "Agrupar por:", 
                    choices = NULL, multiple = TRUE),
        
        selectInput("vars_sum", "Somar:", 
                    choices = NULL, multiple = TRUE),
        
        checkboxInput("pivot_wider", "Pivotar dados (formato largo)", FALSE),
        
        conditionalPanel(
          condition = "input.pivot_wider == true",
          selectInput("pivot_cols", "Coluna para pivotar:", 
                      choices = NULL, multiple = FALSE),
          selectInput("pivot_values", "Valores para as colunas:", 
                      choices = NULL, multiple = FALSE)
        )
      ),
      
      accordion_panel(
        "Filtros",
        card(
          selectInput("var_filtro", "Variável:", choices = NULL),
          selectInput("operador", "Operador:",
                      choices = c("==", "!=", ">", "<", ">=", "<=", "%in%")),
          selectizeInput("valor_filtro", "Valor:", choices = NULL, multiple = TRUE),
          
          selectInput("operador_logico", "Operador Lógico:",
                      choices = c("and", "or")),
          
          actionButton("adicionar_filtro", "Adicionar Filtro", 
                       class = "btn-primary w-100 mb-2"),
          actionButton("limpar_filtros", "Limpar Filtros", 
                       class = "btn-warning w-100"),
          
          hr(),
          textAreaInput("filtro_manual", "Editar Filtro Manualmente:",
                        rows = 4,
                        placeholder = "Ex: grupo_despesa_codigo_grupo == 6"),
          
          # Botão para negar expressão completa
          actionButton("negar_expressao", "Negar Expressão Completa", 
                       class = "btn-info w-100 mb-2"),
          
          actionButton("aplicar_filtro_manual", "Aplicar Filtro Manual",
                       class = "btn-info w-100 mb-2")
        )
      )
    ),
    actionButton("mostrar_preview", "Atualizar Preview", 
                 class = "btn-success w-100 mt-3")
  ),
  
  card(
    full_screen = TRUE,
    min_height = "350px",
    card_header(
      class = "d-flex justify-content-between align-items-center",
      "Preview dos Dados",
      downloadButton("download_data", "Baixar dados (XLSX)")
    ),
    card_body(
      class = "p-0",
      uiOutput("preview_container")
    )
  ),
  
  card(
    full_screen = TRUE,
    card_header(
      class = "d-flex justify-content-between align-items-center",
      "Código Gerado",
      downloadButton("download_code", "Baixar código (TXT)")
    ),
    verbatimTextOutput("codigo_gerado")
  ),
  
  div(
    class = "d-flex justify-content-end align-items-center mt-2 mb-2",
    "Desenvolvido com assistência do ",
    a(
      href = "https://claude.ai",
      target = "_blank",
      class = "text-decoration-none ms-1",
      tags$i(class = "bi bi-robot"),
      " Claude AI"
    )
  )
)

server <- function(input, output, session) {
  # Variáveis reativas
  dados <- reactiveVal(NULL)
  filtros <- reactiveVal(list())
  mostrar_preview <- reactiveVal(FALSE)
  
  # Função para criar condições de filtro
  create_advanced_filter_condition <- function(var, operator, values) {
    # Formatar valores
    formatted_values <- if(is.numeric(values)) {
      paste(values, collapse = ", ")
    } else {
      paste(sprintf("'%s'", values), collapse = ", ")
    }
    
    # Construir a condição base
    base_condition <- if(operator == "==") {
      if(length(values) > 1) {
        sprintf("%s %%in%% c(%s)", var, formatted_values)
      } else {
        sprintf("%s == %s", var, formatted_values)
      }
    } else if(operator == "!=") {
      if(length(values) > 1) {
        sprintf("!(%s %%in%% c(%s))", var, formatted_values)
      } else {
        sprintf("%s != %s", var, formatted_values)
      }
    } else if(operator == "%in%") {
      sprintf("%s %%in%% c(%s)", var, formatted_values)
    } else {
      # Para outros operadores (>, <, >=, <=)
      if(length(values) > 1) {
        conditions <- sapply(values, function(val) {
          sprintf("%s %s %s", var, operator, val)
        })
        sprintf("(%s)", paste(conditions, collapse = " | "))
      } else {
        sprintf("%s %s %s", var, operator, formatted_values)
      }
    }
    
    # Retornar com parênteses para garantir precedência
    sprintf("(%s)", base_condition)
  }
  
  # Carregar arquivo
  observeEvent(input$arquivo, {
    mostrar_preview(FALSE)
    
    req(input$arquivo)
    
    showNotification("Carregando arquivo...", type = "message", duration = NULL, id = "loading")
    
    tryCatch({
      ext <- tools::file_ext(input$arquivo$name)
      
      df <- if(tolower(ext) == "csv") {
        read.csv(input$arquivo$datapath, check.names = FALSE)
      } else if(tolower(ext) %in% c("xlsx", "xls")) {
        read_excel(input$arquivo$datapath)
      }
      
      df <- clean_names(df)
      df <- as.data.frame(df)
      
      colunas_numericas <- names(df)[sapply(df, is.numeric)]
      
      dados(df)
      
      updateSelectInput(session, "var_filtro", choices = names(df))
      updateSelectInput(session, "vars_group", choices = names(df))
      updateSelectInput(session, "vars_sum", 
                        choices = colunas_numericas,
                        selected = NULL)
      
      removeNotification(id = "loading")
      showNotification("Arquivo carregado com sucesso!", type = "success")
    }, error = function(e) {
      removeNotification(id = "loading")
      showNotification(
        sprintf("Erro ao carregar arquivo: %s", e$message),
        type = "error"
      )
    })
  })
  
  # Atualizar opções de seleção
  observeEvent(input$var_filtro, {
    req(dados(), input$var_filtro)
    valores_unicos <- unique(dados()[[input$var_filtro]])
    valores_unicos <- sort(valores_unicos)
    updateSelectizeInput(session, "valor_filtro", 
                         choices = valores_unicos,
                         selected = NULL)
  })
  
  observeEvent(input$vars_group, {
    updateSelectInput(session, "pivot_cols", 
                      choices = input$vars_group,
                      selected = NULL)
  })
  
  observeEvent(input$vars_sum, {
    updateSelectInput(session, "pivot_values", 
                      choices = input$vars_sum,
                      selected = NULL)
  })
  
  # Limpar filtros
  observeEvent(input$limpar_filtros, {
    filtros(list())
    updateTextAreaInput(session, "filtro_manual", value = "")
    mostrar_preview(FALSE)
    showNotification("Filtros removidos", type = "message")
  })
  
  # Mostrar preview
  observeEvent(input$mostrar_preview, {
    mostrar_preview(TRUE)
  })
  
  # Negar expressão completa
  observeEvent(input$negar_expressao, {
    req(length(filtros()) > 0)
    
    # Pegar filtro atual
    filtro_atual <- if(length(filtros()) > 1) {
      filtros_texto <- sapply(seq_along(filtros()), function(i) {
        filtro_item <- filtros()[[i]]
        if(i == 1) return(filtro_item$filtro)
        
        op <- switch(filtro_item$operador,
                     "and" = " & ",
                     "or" = " | ")
        
        paste0(op, "(", filtro_item$filtro, ")")
      })
      paste(filtros_texto, collapse = "")
    } else {
      filtros()[[1]]$filtro
    }
    
    # Criar novo filtro negado
    novo_filtro <- sprintf("!(%s)", filtro_atual)
    
    # Atualizar filtros
    filtros(list(list(
      filtro = novo_filtro,
      operador = "and"
    )))
    
    # Atualizar texto
    updateTextAreaInput(session, "filtro_manual", value = novo_filtro)
    
    showNotification("Expressão completa negada", type = "message")
  })
  
  # Atualizar textAreaInput quando filtros são adicionados
  observe({
    if(length(filtros()) > 0) {
      filtros_texto <- sapply(seq_along(filtros()), function(i) {
        filtro_item <- filtros()[[i]]
        if(i == 1) return(filtro_item$filtro)
        
        op <- switch(filtro_item$operador,
                     "and" = " & ",
                     "or" = " | ")
        
        paste0(op, "(", filtro_item$filtro, ")")
      })
      
      filtro_completo <- paste(filtros_texto, collapse = "")
      updateTextAreaInput(session, "filtro_manual", value = filtro_completo)
    }
  })
  
  # Adicionar filtro
  observeEvent(input$adicionar_filtro, {
    req(input$var_filtro, input$operador, input$valor_filtro)
    
    novo_filtro <- create_advanced_filter_condition(
      input$var_filtro, 
      input$operador, 
      input$valor_filtro
    )
    
    filtros_atuais <- filtros()
    novo_item <- list(
      filtro = novo_filtro,
      operador = input$operador_logico
    )
    filtros(c(filtros_atuais, list(novo_item)))
    mostrar_preview(FALSE)
    
    showNotification("Filtro adicionado", type = "message")
  })
  
  # Processamento de dados filtrados
  dados_filtrados <- reactive({
    req(dados())
    df <- dados()
    
    if(length(filtros()) > 0) {
      filtros_texto <- sapply(seq_along(filtros()), function(i) {
        filtro_item <- filtros()[[i]]
        if(i == 1) return(filtro_item$filtro)
        
        op <- switch(filtro_item$operador,
                     "and" = " & ",
                     "or" = " | ")
        
        paste0(op, "(", filtro_item$filtro, ")")
      })
      
      filtro_completo <- paste(filtros_texto, collapse = "")
      
      tryCatch({
        print(paste("Aplicando filtro:", filtro_completo))  # Debug
        df <- df %>% filter(eval(parse(text = filtro_completo)))
      }, error = function(e) {
        print(paste("Erro no filtro:", e$message))  # Debug
        showNotification(
          sprintf("Erro ao aplicar filtro: %s", e$message),
          type = "error",
          duration = 10
        )
      })
    }
    
    if(length(input$vars_group) > 0 && length(input$vars_sum) > 0) {
      df <- df %>%
        group_by(across(all_of(input$vars_group))) %>%
        summarise(across(all_of(input$vars_sum), \(x) sum(x, na.rm = TRUE))) %>%
        ungroup()
    }
    
    if(input$pivot_wider && !is.null(input$pivot_cols) && !is.null(input$pivot_values)) {
      df <- df %>%
        pivot_wider(
          names_from = input$pivot_cols,
          values_from = input$pivot_values,
          values_fill = 0
        )
    }
    
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    if(length(numeric_cols) > 0) {
      df <- df %>%
        bind_rows(
          df %>%
            summarise(across(all_of(numeric_cols), sum, na.rm = TRUE)) %>%
            mutate(across(where(is.character), ~"Total"))
        )
      
      if(length(numeric_cols) > 1) {
        df <- df %>%
          mutate(
            Total = rowSums(select(., all_of(numeric_cols)), na.rm = TRUE)
          )
      }
    }
    
    return(df)
  })
  
  # Preview container
  output$preview_container <- renderUI({
    if (mostrar_preview()) {
      div(
        style = "height: 100%;",
        DTOutput("preview_dados")
      )
    } else {
      div(
        class = "text-center p-4",
        style = "height: 100%;",
        "Clique em 'Atualizar Preview' para visualizar os dados"
      )
    }
  })
  
  # Renderizar tabela de dados
  output$preview_dados <- renderDT({
    req(dados_filtrados())
    df <- dados_filtrados()
    
    cols_numericas <- sapply(df, is.numeric)
    
    datatable(
      df,
      options = list(
        pageLength = 5,
        scrollY = "400px",
        scrollCollapse = TRUE,
        language = list(
          decimal = ",",
          thousands = "."
        )
      )
    ) %>%
      formatCurrency(
        columns = which(cols_numericas),
        currency = "",
        mark = ".",
        dec.mark = ",",
        digits = 2
      )
  })
  
  # Download de dados
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("dados_filtrados_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      write_xlsx(dados_filtrados(), file)
    }
  )
  
  # Download de código
  output$download_code <- downloadHandler(
    filename = function() {
      paste0("codigo_r_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      codigo <- "dados %>%\n  clean_names()"
      
      if(length(filtros()) > 0) {
        filtros_texto <- sapply(seq_along(filtros()), function(i) {
          filtro_item <- filtros()[[i]]
          if(i == 1) return(filtro_item$filtro)
          
          op <- switch(filtro_item$operador,
                       "and" = " & ",
                       "or" = " | ")
          
          paste0(op, "(", filtro_item$filtro, ")")
        })
        
        filtro_completo <- paste(filtros_texto, collapse = "")
        codigo <- paste0(codigo, " %>%\n  filter(", filtro_completo, ")")
      }
      
      if(length(input$vars_group) > 0 && length(input$vars_sum) > 0) {
        vars_group <- paste(input$vars_group, collapse = ", ")
        vars_sum <- paste(input$vars_sum, collapse = ", ")
        
        codigo <- paste0(codigo, " %>%\n",
                         "  group_by(", vars_group, ") %>%\n",
                         "  summarise(across(c(", vars_sum, "), \\(x) sum(x, na.rm = TRUE)))")
        
        if(input$pivot_wider && !is.null(input$pivot_cols) && !is.null(input$pivot_values)) {
          codigo <- paste0(codigo, " %>%\n",
                           "  pivot_wider(\n",
                           "    names_from = ", input$pivot_cols, ",\n",
                           "    values_from = ", input$pivot_values, ",\n",
                           "    values_fill = 0\n",
                           "  )")
        }
      }
      
      writeLines(codigo, file)
    },
    contentType = "text/plain"
  )
  
  # Código gerado
  output$codigo_gerado <- renderText({
    req(dados())
    
    codigo <- "dados %>%\n  clean_names()"
    
    if(length(filtros()) > 0) {
      filtros_texto <- sapply(seq_along(filtros()), function(i) {
        filtro_item <- filtros()[[i]]
        if(i == 1) return(filtro_item$filtro)
        
        op <- switch(filtro_item$operador,
                     "and" = " & ",
                     "or" = " | ")
        
        paste0(op, "(", filtro_item$filtro, ")")
      })
      
      filtro_completo <- paste(filtros_texto, collapse = "")
      codigo <- paste0(codigo, " %>%\n  filter(", filtro_completo, ")")
    }
    
    if(length(input$vars_group) > 0 && length(input$vars_sum) > 0) {
      vars_group <- paste(input$vars_group, collapse = ", ")
      vars_sum <- paste(input$vars_sum, collapse = ", ")
      
      codigo <- paste0(codigo, " %>%\n",
                       "  group_by(", vars_group, ") %>%\n",
                       "  summarise(across(c(", vars_sum, "), \\(x) sum(x, na.rm = TRUE)))")
      
      if(input$pivot_wider && !is.null(input$pivot_cols) && !is.null(input$pivot_values)) {
        codigo <- paste0(codigo, " %>%\n",
                         "  pivot_wider(\n",
                         "    names_from = ", input$pivot_cols, ",\n",
                         "    values_from = ", input$pivot_values, ",\n",
                         "    values_fill = 0\n",
                         "  )")
      }
    }
    
    return(codigo)
  })
  
  # Aplicar filtro manual
  observeEvent(input$aplicar_filtro_manual, {
    req(input$filtro_manual)
    
    tryCatch({
      parse(text = input$filtro_manual)
      
      filtros(list(list(
        filtro = input$filtro_manual,
        operador = "and"
      )))
      
      mostrar_preview(FALSE)
      showNotification("Filtro manual aplicado", type = "message")
    }, error = function(e) {
      showNotification(
        sprintf("Erro na sintaxe do filtro: %s", e$message),
        type = "error",
        duration = 10
      )
    })
  })
}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)