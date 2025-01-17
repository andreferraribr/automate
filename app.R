library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(bsicons)
library(readxl)
library(janitor)
library(writexl)
library(scales)

ui <- page_sidebar(
  title = "Gerador de Subsets",
  sidebar = sidebar(
    title = "Parâmetros do Subset",
    
    fileInput("arquivo", "Carregar arquivo (CSV ou XLSX):",
              accept = c(".csv", ".xlsx", ".xls")),
    
    selectInput("vars_numericas", "Selecionar Colunas Numéricas:", 
                choices = NULL, multiple = TRUE),
    
    selectInput("variaveis", "Selecionar Variáveis:", 
                choices = NULL, multiple = TRUE),
    
    selectInput("vars_group", "Agrupar por:", 
                choices = NULL, multiple = TRUE),
    
    selectInput("vars_sum", "Somar:", 
                choices = NULL, multiple = TRUE),
    
    card(
      card_header("Adicionar Filtro"),
      selectInput("var_filtro", "Variável:", choices = NULL),
      selectInput("operador", "Operador:",
                  choices = c("==", "!=", ">", "<", ">=", "<=")),
      selectizeInput("valor_filtro", "Valor:", choices = NULL, multiple = TRUE),
      
      selectInput("operador_logico", "Operador Lógico para Próximo Filtro:",
                  choices = c("and", "or", "and not", "or not")),
      
      actionButton("adicionar_filtro", "Adicionar Filtro", class = "btn-primary")
    ),
    
    actionButton("limpar_filtros", "Limpar Filtros", class = "btn-warning"),
    actionButton("validar", "Validar Subset", class = "btn-success")
  ),
  
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Preview dos Dados"),
      downloadButton("download_data", "Baixar dados (XLSX)"),
      tags$br(), tags$br(),
      DTOutput("preview_dados")
    ),
    card(
      full_screen = TRUE,
      card_header("Código Gerado"),
      verbatimTextOutput("codigo_gerado")
    )
  ),
  
  card(
    card_header("Filtros Ativos"),
    textOutput("filtros_ativos")
  ),
  
  card(
    card_header("Estatísticas do Subset"),
    layout_columns(
      value_box(
        "Linhas no Subset",
        textOutput("n_linhas"),
        showcase = bs_icon("table")
      ),
      value_box(
        "Colunas Selecionadas",
        textOutput("n_colunas"),
        showcase = bs_icon("columns-gap")
      ),
      value_box(
        "% dos Dados Originais",
        textOutput("proporcao"),
        showcase = bs_icon("percent")
      )
    )
  )
)

server <- function(input, output, session) {
  dados <- reactiveVal(NULL)
  filtros <- reactiveVal(list())
  
  observeEvent(input$arquivo, {
    req(input$arquivo)
    
    ext <- tools::file_ext(input$arquivo$name)
    
    df <- if(tolower(ext) == "csv") {
      read.csv(input$arquivo$datapath)
    } else if(tolower(ext) %in% c("xlsx", "xls")) {
      read_excel(input$arquivo$datapath)
    }
    
    df <- clean_names(df)
    df <- as.data.frame(df)
    
    dados(df)
    
    updateSelectInput(session, "vars_numericas", choices = names(df))
    updateSelectInput(session, "variaveis", 
                      choices = names(df),
                      selected = names(df)[1:min(3, ncol(df))])
    updateSelectInput(session, "var_filtro", choices = names(df))
    updateSelectInput(session, "vars_group", choices = names(df))
    updateSelectInput(session, "vars_sum", choices = names(df))
  })
  
  observeEvent(input$var_filtro, {
    req(dados(), input$var_filtro)
    valores_unicos <- unique(dados()[[input$var_filtro]])
    valores_unicos <- sort(valores_unicos)
    updateSelectizeInput(session, "valor_filtro", 
                         choices = valores_unicos,
                         selected = NULL)
  })
  
  observe({
    req(dados(), input$vars_numericas)
    df <- dados()
    
    for(col in names(df)) {
      if(col %in% input$vars_numericas) {
        df[[col]] <- as.numeric(df[[col]])
      } else {
        df[[col]] <- as.character(df[[col]])
      }
    }
    
    dados(df)
  })
  
  observeEvent(input$adicionar_filtro, {
    req(input$var_filtro, input$operador, input$valor_filtro)
    
    if(length(input$valor_filtro) > 1) {
      valores <- if(input$var_filtro %in% input$vars_numericas) {
        paste(input$valor_filtro, collapse = ", ")
      } else {
        paste(sprintf("'%s'", input$valor_filtro), collapse = ", ")
      }
      
      if(input$operador %in% c("==", "!=")) {
        novo_filtro <- sprintf("%s %s c(%s)", 
                               input$var_filtro,
                               ifelse(input$operador == "==", "%in%", "!%in%"),
                               valores)
      } else {
        condicoes <- sapply(input$valor_filtro, function(valor) {
          sprintf("%s %s %s", 
                  input$var_filtro,
                  input$operador,
                  if(input$var_filtro %in% input$vars_numericas) valor 
                  else sprintf("'%s'", valor))
        })
        novo_filtro <- paste(condicoes, collapse = " | ")
      }
    } else {
      novo_filtro <- sprintf("%s %s %s", 
                             input$var_filtro,
                             input$operador,
                             if(input$var_filtro %in% input$vars_numericas) input$valor_filtro 
                             else sprintf("'%s'", input$valor_filtro))
    }
    
    filtros_atuais <- filtros()
    novo_item <- list(
      filtro = novo_filtro,
      operador = input$operador_logico
    )
    filtros(c(filtros_atuais, list(novo_item)))
  })
  
  observeEvent(input$limpar_filtros, {
    filtros(list())
  })
  
  dados_filtrados <- reactive({
    req(dados())
    df <- dados()
    
    vars_disponiveis <- names(df)
    
    vars_select <- input$variaveis[input$variaveis %in% vars_disponiveis]
    vars_group <- input$vars_group[input$vars_group %in% vars_disponiveis]
    vars_sum <- input$vars_sum[input$vars_sum %in% vars_disponiveis]
    
    if(length(filtros()) > 0) {
      filtros_texto <- sapply(seq_along(filtros()), function(i) {
        filtro_item <- filtros()[[i]]
        if(i == 1) return(filtro_item$filtro)
        
        op <- switch(filtro_item$operador,
                     "and" = " & ",
                     "or" = " | ",
                     "and not" = " & !(",
                     "or not" = " | !(")
        
        paste0(op, filtro_item$filtro, 
               if(filtro_item$operador %in% c("and not", "or not")) ")" else "")
      })
      
      filtro_completo <- paste(filtros_texto, collapse = "")
      df <- df %>% filter(eval(parse(text = filtro_completo)))
    }
    
    if(length(vars_select) > 0) {
      vars_select <- unique(c(vars_select, vars_group, vars_sum))
      df <- df %>% select(all_of(vars_select))
    }
    
    if(length(vars_group) > 0 && length(vars_sum) > 0) {
      vars_sum_existentes <- vars_sum[vars_sum %in% names(df)]
      
      if(length(vars_sum_existentes) > 0) {
        df <- df %>%
          group_by(across(all_of(vars_group))) %>%
          summarise(across(all_of(vars_sum_existentes), sum, na.rm = TRUE))
      }
    }
    
    return(df)
  })
  
  output$preview_dados <- renderDT({
    req(dados_filtrados())
    df <- dados_filtrados()
    
    # Identifica colunas numéricas
    cols_numericas <- sapply(df, is.numeric)
    
    datatable(
      df,
      options = list(
        pageLength = 5,
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
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("dados_filtrados_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      write_xlsx(dados_filtrados(), file)
    }
  )
  
  output$codigo_gerado <- renderText({
    req(dados())
    
    codigo <- "dados %>%\n  clean_names()"
    
    if(length(filtros()) > 0) {
      filtros_texto <- sapply(seq_along(filtros()), function(i) {
        filtro_item <- filtros()[[i]]
        if(i == 1) return(filtro_item$filtro)
        
        op <- switch(filtro_item$operador,
                     "and" = " & ",
                     "or" = " | ",
                     "and not" = " & !(",
                     "or not" = " | !(")
        
        paste0(op, filtro_item$filtro, 
               if(filtro_item$operador %in% c("and not", "or not")) ")" else "")
      })
      
      filtro_completo <- paste(filtros_texto, collapse = "")
      codigo <- paste0(codigo, " %>%\n  filter(", filtro_completo, ")")
    }
    
    if(length(input$vars_group) > 0 && length(input$vars_sum) > 0) {
      vars_group <- paste(input$vars_group, collapse = ", ")
      vars_sum <- paste(input$vars_sum, collapse = ", ")
      
      codigo <- paste0(codigo, " %>%\n",
                       "  group_by(", vars_group, ") %>%\n",
                       "  summarise(across(c(", vars_sum, "), sum, na.rm = TRUE))")
    }
    
    return(codigo)
  })
  
  observeEvent(input$limpar_filtros, {
    filtros(list())
  })
  
  dados_filtrados <- reactive({
    req(dados())
    df <- dados()
    
    vars_disponiveis <- names(df)
    
    vars_select <- input$variaveis[input$variaveis %in% vars_disponiveis]
    vars_group <- input$vars_group[input$vars_group %in% vars_disponiveis]
    vars_sum <- input$vars_sum[input$vars_sum %in% vars_disponiveis]
    
    if(length(filtros()) > 0) {
      filtros_texto <- sapply(seq_along(filtros()), function(i) {
        filtro_item <- filtros()[[i]]
        if(i == 1) return(filtro_item$filtro)
        
        op <- switch(filtro_item$operador,
                     "and" = " & ",
                     "or" = " | ",
                     "and not" = " & !(",
                     "or not" = " | !(")
        
        paste0(op, filtro_item$filtro, 
               if(filtro_item$operador %in% c("and not", "or not")) ")" else "")
      })
      
      filtro_completo <- paste(filtros_texto, collapse = "")
      df <- df %>% filter(eval(parse(text = filtro_completo)))
    }
    
    if(length(vars_select) > 0) {
      vars_select <- unique(c(vars_select, vars_group, vars_sum))
      df <- df %>% select(all_of(vars_select))
    }
    
    if(length(vars_group) > 0 && length(vars_sum) > 0) {
      vars_sum_existentes <- vars_sum[vars_sum %in% names(df)]
      
      if(length(vars_sum_existentes) > 0) {
        df <- df %>%
          group_by(across(all_of(vars_group))) %>%
          summarise(across(all_of(vars_sum_existentes), sum, na.rm = TRUE))
      }
    }
    
    return(df)
  })
  
  output$filtros_ativos <- renderText({
    if(length(filtros()) == 0) return("Nenhum filtro aplicado")
    
    filtros_texto <- sapply(seq_along(filtros()), function(i) {
      filtro_item <- filtros()[[i]]
      if(i == 1) return(filtro_item$filtro)
      paste(filtro_item$operador, filtro_item$filtro)
    })
    
    paste("Filtros:", paste(filtros_texto, collapse = " "))
  })
  
  output$n_linhas <- renderText({
    req(dados_filtrados())
    nrow(dados_filtrados())
  })
  
  output$n_colunas <- renderText({
    req(dados_filtrados())
    ncol(dados_filtrados())
  })
  
  output$proporcao <- renderText({
    req(dados(), dados_filtrados())
    sprintf("%.1f%%", 100 * nrow(dados_filtrados()) / nrow(dados()))
  })
}

shinyApp(ui = ui, server = server)