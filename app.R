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

`%notin%` <- Negate(`%in%`)

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
                      choices = c("==", "!=", ">", "<", ">=", "<=")),
          selectizeInput("valor_filtro", "Valor:", choices = NULL, multiple = TRUE),
          
          selectInput("operador_logico", "Operador Lógico para Próximo Filtro:",
                      choices = c("and", "or", "and not", "or not")),
          
          actionButton("adicionar_filtro", "Adicionar Filtro", 
                       class = "btn-primary w-100 mb-2"),
          actionButton("limpar_filtros", "Limpar Filtros", 
                       class = "btn-warning w-100")
        )
      )
    ),
    actionButton("mostrar_preview", "Atualizar Preview", 
                 class = "btn-success w-100 mt-3")
  ),
  
  card(
    full_screen = TRUE,
    min_height = "350px",  # Add minimum height
    card_header(
      class = "d-flex justify-content-between align-items-center",
      "Preview dos Dados",
      downloadButton("download_data", "Baixar dados (XLSX)")
    ),
    card_body(
      class = "p-0",  # Remove padding to maximize space
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
  
  # layout_columns(
  #   col_widths = c(12),
  #   card(
  #     card_header("Filtros Ativos"),
  #     textOutput("filtros_ativos")
  #   )
  # ),
  
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
  
  ),
  
  # Add credits card here
  # card(
  #   class = "mt-3",
  #   card_header(
  #     class = "d-flex align-items-center gap-2",
  #     bs_icon("info-circle"),
  #     "Créditos"
  #   ),
  #   div(
  #     class = "text-center p-2",
  #     HTML(paste(
  #       "App desenvolvido com assistência do Claude AI (Anthropic)",
  #       "<br>",
  #       "<a href='https://claude.ai' target='_blank' class='text-decoration-none'>",
  #       "<i class='bi bi-robot'></i> claude.ai",
  #       "</a>"
  #     ))
  #   )
  # )
)

server <- function(input, output, session) {
  dados <- reactiveVal(NULL)
  filtros <- reactiveVal(list())
  mostrar_preview <- reactiveVal(FALSE)
  
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
  
  observeEvent(input$limpar_filtros, {
    filtros(list())
    mostrar_preview(FALSE)
    showNotification("Filtros removidos", type = "message")
  })
  
  observeEvent(input$mostrar_preview, {
    mostrar_preview(TRUE)
  })
  
  observeEvent(input$adicionar_filtro, {
    req(input$var_filtro, input$operador, input$valor_filtro)
    
    if(length(input$valor_filtro) > 1) {
      valores <- if(is.numeric(dados()[[input$var_filtro]])) {
        paste(input$valor_filtro, collapse = ", ")
      } else {
        paste(sprintf("'%s'", input$valor_filtro), collapse = ", ")
      }
      
      if(input$operador %in% c("==", "!=")) {
        novo_filtro <- sprintf("%s %s c(%s)", 
                               input$var_filtro,
                               ifelse(input$operador == "==", "%in%", "%notin%"),
                               valores)
      } else {
        condicoes <- sapply(input$valor_filtro, function(valor) {
          sprintf("%s %s %s", 
                  input$var_filtro,
                  input$operador,
                  if(is.numeric(dados()[[input$var_filtro]])) valor 
                  else sprintf("'%s'", valor))
        })
        novo_filtro <- paste(condicoes, collapse = " | ")
      }
    } else {
      novo_filtro <- sprintf("%s %s %s", 
                             input$var_filtro,
                             input$operador,
                             if(is.numeric(dados()[[input$var_filtro]])) input$valor_filtro 
                             else sprintf("'%s'", input$valor_filtro))
    }
    
    filtros_atuais <- filtros()
    novo_item <- list(
      filtro = novo_filtro,
      operador = input$operador_logico
    )
    filtros(c(filtros_atuais, list(novo_item)))
    mostrar_preview(FALSE)
  })
  
  
 
  dados_filtrados <- reactive({
    req(dados())
    df <- dados()
    
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
    
    if(length(input$vars_group) > 0 && length(input$vars_sum) > 0) {
      # Convert group_by variables to character before grouping
      df <- df %>%
        mutate(across(all_of(input$vars_group), as.character)) %>%
        group_by(across(all_of(input$vars_group))) %>%
        summarise(across(all_of(input$vars_sum), \(x) sum(x, na.rm = TRUE))) %>%
        ungroup()  # Ungroup to avoid issues with further operations
    }
    
    if(input$pivot_wider && !is.null(input$pivot_cols) && !is.null(input$pivot_values)) {
      df <- df %>%
        pivot_wider(
          names_from = input$pivot_cols,
          values_from = input$pivot_values,
          values_fill = 0
        )
    }
    
    # Add totals only if we have numeric columns
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    if(length(numeric_cols) > 0) {
      # Add total row
      df <- df %>%
        bind_rows(
          df %>%
            summarise(across(all_of(numeric_cols), sum, na.rm = TRUE)) %>%
            mutate(across(where(is.character), ~"Total"))
        )
      
      # Add total column only if we have more than one numeric column
      if(length(numeric_cols) > 1) {
        df <- df %>%
          mutate(
            Total = rowSums(select(., all_of(numeric_cols)), na.rm = TRUE)
          )
      }
    }
    
    return(df)
  })
  
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
  
  output$preview_dados <- renderDT({
    req(dados_filtrados())
    df <- dados_filtrados()
    
    cols_numericas <- sapply(df, is.numeric)
    
    datatable(
      df,
      options = list(
        pageLength = 5,  # Increased from 5 to 10
        scrollY = "400px",  # Add vertical scrolling
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
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("dados_filtrados_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      write_xlsx(dados_filtrados(), file)
    }
  )
  
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

options(shiny.maxRequestSize = 30*1024^2) 
shinyApp(ui = ui, server = server)