if(!require(shiny)){install.packages("shiny")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(DT)){install.packages("DT")}
if(!require(plotly)){install.packages("plotly")}
if(!require(readr)){install.packages("readr")}
if(!require(openxlsx)){install.packages("openxlsx")}
if(!require(scales)){install.packages("scales")}
if(!require(stringr)){install.packages("stringr")}
if(!require(janitor)){install.packages("janitor")}


library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(readr)
library(openxlsx)
library(scales)
library(stringr)
library(janitor)

# Function to scale numbers
scale_number <- function(x, scale = "unit") {
  if (is.numeric(x)) {
    scaled_value <- if (scale == "thousand") {
      x / 1000
    } else if (scale == "million") {
      x / 1000000
    } else if (scale == "billion") {
      x / 1000000000
    } else {
      x
    }
    
    format(round(scaled_value, 2), nsmall = 2, big.mark = ".", decimal.mark = ",")
  } else {
    x
  }
}

ui <- fluidPage(
  titlePanel("Análise de Dados Personalizável"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file_input", "Carregar Dados (CSV ou XLSX)",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".xlsx")),
      
      uiOutput("attribute_selector"),
      # Grouping UI
      uiOutput("group_selector"),
      
      # Filtering UI
      uiOutput("filter_selectors"),
      
      # Summarization UI
      uiOutput("summarize_selector"),
      
      # Scale Selection
      selectInput("scale_selector", "Escala dos Resultados",
                  choices = c("unit", "thousand", "million", "billion"), selected = "unit"),
      
      actionButton("save_filter", "Salvar Filtro"),
      downloadButton("download_filters", "Download Filtros")
      
      
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tabela de Dados", DT::dataTableOutput("custom_table")),
        tabPanel("Filtro", verbatimTextOutput("filter_text"))
      )
    )
  )
)
options(shiny.maxRequestSize=30*1024^2) 
server <- function(input, output) {
  
  uploaded_data <- reactive({
    file <- input$file_input
    if (is.null(file)) {
      return(NULL)
    }
    
    ext <- tools::file_ext(file$datapath)
    
    tryCatch({
      if (ext == "csv") {
        df <- read_csv(file$datapath)
      } else if (ext == "xlsx") {
        sheets <- openxlsx::getSheetNames(file$datapath)
        if(length(sheets) == 0){
          showNotification("O arquivo excel não possui abas!", type = "error")
          return(NULL)
        }else{
          df <- openxlsx::read.xlsx(file$datapath, sheet = sheets[1])
        }
      } else {
        showNotification("Formato de arquivo inválido", type = "error")
        return(NULL)
      }
      
      df <- janitor::clean_names(df)
      return(df)
      
    }, error = function(e){
      showNotification(paste("Erro ao ler o arquivo:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$attribute_selector <- renderUI({
    df <- uploaded_data()
    if (is.null(df)) {
      return(NULL)
    }
    
    attribute_cols <- names(df)[!sapply(df, is.numeric)]
    
    
    return(NULL)
  })
  
  output$group_selector <- renderUI({
    df <- uploaded_data()
    if (is.null(df)) {
      return(NULL)
    }
    attribute_cols <- names(df)[!sapply(df, is.numeric)]
    
    selectInput("group_col", "Selecionar Variáveis de Agrupamento (Opcional)",
                choices =  attribute_cols, multiple = TRUE, selected = NULL)
  })
  
  output$filter_selectors <- renderUI({
    df <- uploaded_data()
    if(is.null(df)){
      return(NULL)
    }
    attribute_cols <- names(df)[!sapply(df, is.numeric)]
    
    filter_ui_elements <- lapply(attribute_cols, function(col) {
      
      choices <- unique(df[[col]])
      tagList(
        selectInput(paste0("filter_type_", col),
                    paste("Tipo de filtro", col),
                    choices = c("equal", "not equal", "greater", "less", "greater or equal", "less or equal",  "in", "not in", "starts with", "ends with", "contains")),
        
        selectInput(paste0("filter_val_", col),
                    paste("Valor do filtro", col),
                    choices = choices, multiple = TRUE)
      )
    })
    
    filter_ui_elements
  })
  
  output$summarize_selector <- renderUI({
    df <- uploaded_data()
    if(is.null(df)){
      return(NULL)
    }
    
    metric_cols <- names(df)[sapply(df, is.numeric)]
    
    tagList(
      selectInput("summarize_col", "Selecionar Coluna para Sumarizar (Opcional)",
                  choices = c("Nenhum", metric_cols), selected = "Nenhum"),
      
      conditionalPanel(
        condition = "input$summarize_col != 'Nenhum'",
        selectInput("summarize_func", "Selecionar Função de Sumarização",
                    choices = c("sum", "mean", "median", "min", "max"), selected = "sum")
      )
    )
  })
  
  
  filter_settings <- reactiveVal(list())
  
  observeEvent(input$save_filter, {
    
    df <- uploaded_data()
    if (is.null(df)) {
      showNotification("Nenhum dado carregado.", type = "error")
      return(NULL)
    }
    
    attribute_cols <- names(df)[!sapply(df, is.numeric)]
    current_filters <- data.frame(attribute = character(),
                                  type = character(),
                                  value = character(),
                                  stringsAsFactors = FALSE)
    
    for(col in attribute_cols){
      filter_type <- input[[paste0("filter_type_", col)]]
      filter_value <- input[[paste0("filter_val_", col)]]
      
      if (!is.null(filter_type) && filter_type != "" && !is.null(filter_value) && length(filter_value) > 0) {
        current_filters <- rbind(current_filters,
                                 data.frame(attribute = col,
                                            type = filter_type,
                                            value = paste(filter_value, collapse = ", "),
                                            stringsAsFactors = FALSE)
        )
      }
    }
    
    if (nrow(current_filters) == 0) {
      showNotification("Nenhum filtro selecionado.", type = "error")
      return(NULL)
    }
    
    filter_list <- filter_settings()
    filter_list <- append(filter_list, list(current_filters))
    filter_settings(filter_list)
    
    showModal(modalDialog(
      title = "Filtro salvo",
      "Filtro salvo com sucesso!",
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })
  
  
  
  output$filter_text <- renderText({
    filters <- filter_settings()
    df <- uploaded_data()
    if(length(filters) > 0 && !is.null(df)){
      code <- "library(dplyr)\n\nfilter_data <- function(df){\n"
      for(filter_df in filters){
        attribute_cols <- names(df)[!sapply(df, is.numeric)]
        for(i in 1:nrow(filter_df)){
          col <- filter_df$attribute[i]
          filter_type <- filter_df$type[i]
          filter_value <- filter_df$value[i]
          
          code <- paste0(code, "  df <- df %>% filter(")
          
          if (filter_type == "equal") {
            code <- paste0(code, col, " == ", deparse(filter_value),")\n")
          } else if(filter_type == "not equal") {
            code <- paste0(code, col, " != ", deparse(filter_value),")\n")
          } else if (filter_type == "greater") {
            code <- paste0(code, col, " > ", deparse(filter_value),")\n")
          } else if (filter_type == "less") {
            code <- paste0(code, col, " < ", deparse(filter_value),")\n")
          }  else if (filter_type == "greater or equal") {
            code <- paste0(code, col, " >= ", deparse(filter_value),")\n")
          }  else if (filter_type == "less or equal") {
            code <- paste0(code, col, " <= ", deparse(filter_value),")\n")
          }  else if(filter_type == "in"){
            code <- paste0(code, col, " %in%  ", deparse(filter_value),")\n")
          } else if(filter_type == "not in"){
            code <- paste0(code, "! (", col, " %in% ", deparse(filter_value),") | is.na(", col, "))\n")
          }else if(filter_type == "starts with"){
            code <- paste0(code, "startsWith(", col, ", ", deparse(filter_value),")\n")
          }else if(filter_type == "ends with"){
            code <- paste0(code, "endsWith(", col, ", ", deparse(filter_value),")\n")
          }else if(filter_type == "contains"){
            code <- paste0(code, "grepl(", deparse(filter_value),", ", col, ", ignore.case = TRUE))\n")
          }
        }
      }
      if(!is.null(input$group_col) && length(input$group_col) > 0){
        code <- paste0(code, "  df <- df %>% group_by(",paste(input$group_col, collapse = ", "), ")\n")
      }
      if(input$summarize_col != "Nenhum" && input$summarize_func != ""){
        code <- paste0(code, "   df <- df %>% summarise(", paste0(input$summarize_func, "_", input$summarize_col), " = ", input$summarize_func, "(", input$summarize_col, ", na.rm = TRUE))\n")
      }
      code <- paste0(code, "  return(df)\n}")
      code <- paste0(code, "\n\n df <- filter_data(df)\n\n")
      
      if(input$summarize_col != "Nenhum"){
        code <- paste0(code, "numeric_cols <- sapply(df, is.numeric)\n df <- df %>%
                         mutate(across(where(is.numeric), ~ scale_number(., scale = \"", input$scale_selector ,"\")))\n")
      }
      code <- paste0(code, "\n\ndf")
      code
    } else {
      "Nenhum filtro salvo"
    }
  })
  
  output$download_filters <- downloadHandler(
    filename = function() {
      paste("filters-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      filters <- filter_settings()
      df <- uploaded_data()
      if(length(filters) > 0 && !is.null(df)){
        
        code <- "library(dplyr)\n\nfilter_data <- function(df){\n"
        
        for(filter_df in filters){
          
          attribute_cols <- names(df)[!sapply(df, is.numeric)]
          for(i in 1:nrow(filter_df)){
            col <- filter_df$attribute[i]
            filter_type <- filter_df$type[i]
            filter_value <- filter_df$value[i]
            
            code <- paste0(code, "  df <- df %>% filter(")
            
            if (filter_type == "equal") {
              code <- paste0(code, col, " == ", deparse(filter_value),")\n")
            } else if(filter_type == "not equal") {
              code <- paste0(code, col, " != ", deparse(filter_value),")\n")
            } else if (filter_type == "greater") {
              code <- paste0(code, col, " > ", deparse(filter_value),")\n")
            } else if (filter_type == "less") {
              code <- paste0(code, col, " < ", deparse(filter_value),")\n")
            }  else if (filter_type == "greater or equal") {
              code <- paste0(code, col, " >= ", deparse(filter_value),")\n")
            }  else if (filter_type == "less or equal") {
              code <- paste0(code, col, " <= ", deparse(filter_value),")\n")
            }  else if(filter_type == "in"){
              code <- paste0(code, col, " %in%  ", deparse(filter_value),")\n")
            } else if(filter_type == "not in"){
              code <- paste0(code, "! (", col, " %in% ", deparse(filter_value),") | is.na(", col, "))\n")
            }else if(filter_type == "starts with"){
              code <- paste0(code, "startsWith(", col, ", ", deparse(filter_value),")\n")
            }else if(filter_type == "ends with"){
              code <- paste0(code, "endsWith(", col, ", ", deparse(filter_value),")\n")
            }else if(filter_type == "contains"){
              code <- paste0(code, "grepl(", deparse(filter_value),", ", col, ", ignore.case = TRUE))\n")
            }
          }
        }
        
        if(!is.null(input$group_col) && length(input$group_col) > 0){
          code <- paste0(code, "  df <- df %>% group_by(",paste(input$group_col, collapse = ", "), ")\n")
        }
        if(input$summarize_col != "Nenhum" && input$summarize_func != ""){
          code <- paste0(code, "   df <- df %>% summarise(", paste0(input$summarize_func, "_", input$summarize_col), " = ", input$summarize_func, "(", input$summarize_col, ", na.rm = TRUE))\n")
        }
        code <- paste0(code, "  return(df)\n}")
        
        code <- paste0(code, "\n\n df <- filter_data(df)\n\n")
        if(input$summarize_col != "Nenhum"){
          code <- paste0(code, "numeric_cols <- sapply(df, is.numeric)\n df <- df %>%
                        mutate(across(where(is.numeric), ~ scale_number(., scale = \"", input$scale_selector ,"\")))\n")
        }
        code <- paste0(code, "\n\ndf")
        write(code, file)
      }
    }
  )
  
  filtered_data <- reactive({
    df <- uploaded_data()
    if (is.null(df)) {
      return(NULL)
    }
    
    attribute_cols <- names(df)[!sapply(df, is.numeric)]
    
    for(col in attribute_cols){
      filter_type <- input[[paste0("filter_type_", col)]]
      filter_value <- input[[paste0("filter_val_", col)]]
      
      if (!is.null(filter_type) && filter_type != "" && !is.null(filter_value) && length(filter_value) > 0) {
        
        filter_value <- tryCatch(as.numeric(filter_value), warning = function(w) filter_value)
        
        if (filter_type == "equal") {
          df <- df %>% filter(!!sym(col) == filter_value)
        } else if(filter_type == "not equal") {
          df <- df %>% filter(!!sym(col) != filter_value)
        } else if (filter_type == "greater") {
          df <- df %>% filter(!!sym(col) > filter_value)
        } else if (filter_type == "less") {
          df <- df %>% filter(!!sym(col) < filter_value)
        }  else if (filter_type == "greater or equal") {
          df <- df %>% filter(!!sym(col) >= filter_value)
        }  else if (filter_type == "less or equal") {
          df <- df %>% filter(!!sym(col) <= filter_value)
        }  else if(filter_type == "in"){
          
          df <- df %>% filter(!!sym(col) %in% filter_value)
        } else if(filter_type == "not in"){
          
          df <- df %>% filter(! (!!sym(col) %in% filter_value) | is.na(!!sym(col)))
        }else if(filter_type == "starts with"){
          df <- df %>% filter(startsWith(!!sym(col), filter_value))
        }else if(filter_type == "ends with"){
          df <- df %>% filter(endsWith(!!sym(col), filter_value))
        }else if(filter_type == "contains"){
          df <- df %>% filter(grepl(filter_value, !!sym(col), ignore.case = TRUE))
        }
      }
    }
    
    return(df)
  })
  
  summarized_data <- reactive({
    df <- filtered_data()
    if (is.null(df) || input$summarize_col == "Nenhum") {
      return(df)
    }
    group_cols <- input$group_col
    
    if(is.null(group_cols) || length(group_cols) == 0){
      if(input$summarize_func == "sum"){
        df <- df %>% summarise(!!paste0("sum_", input$summarize_col) := sum(!!sym(input$summarize_col), na.rm=TRUE))
      } else if (input$summarize_func == "mean"){
        df <- df %>% summarise(!!paste0("mean_", input$summarize_col) := mean(!!sym(input$summarize_col), na.rm=TRUE))
      } else if (input$summarize_func == "median"){
        df <- df %>% summarise(!!paste0("median_", input$summarize_col) := median(!!sym(input$summarize_col), na.rm=TRUE))
      } else if (input$summarize_func == "min"){
        df <- df %>% summarise(!!paste0("min_", input$summarize_col) := min(!!sym(input$summarize_col), na.rm=TRUE))
      } else if (input$summarize_func == "max"){
        df <- df %>% summarise(!!paste0("max_", input$summarize_col) := max(!!sym(input$summarize_col), na.rm=TRUE))
      }
    }
    else{
      group_syms <- syms(group_cols)
      if(input$summarize_func == "sum"){
        df <- df %>% group_by(!!!group_syms) %>% summarise(!!paste0("sum_", input$summarize_col) := sum(!!sym(input$summarize_col), na.rm=TRUE))
        
      } else if (input$summarize_func == "mean"){
        df <- df %>% group_by(!!!group_syms) %>% summarise(!!paste0("mean_", input$summarize_col) := mean(!!sym(input$summarize_col), na.rm=TRUE))
      } else if (input$summarize_func == "median"){
        df <- df %>% group_by(!!!group_syms) %>% summarise(!!paste0("median_", input$summarize_col) := median(!!sym(input$summarize_col), na.rm=TRUE))
      } else if (input$summarize_func == "min"){
        df <- df %>% group_by(!!!group_syms) %>% summarise(!!paste0("min_", input$summarize_col) := min(!!sym(input$summarize_col), na.rm=TRUE))
      } else if (input$summarize_func == "max"){
        df <- df %>% group_by(!!!group_syms) %>% summarise(!!paste0("max_", input$summarize_col) := max(!!sym(input$summarize_col), na.rm=TRUE))
      }
    }
    
    return(df)
  })
  
  output$custom_table <- DT::renderDataTable({
    df <- summarized_data()
    req(df)
    
    scale <- input$scale_selector
    
    if(input$summarize_col != "Nenhum"){
      if(!is.null(input$group_col) && length(input$group_col) > 0){
        
        original_data <- filtered_data()
        if(!is.null(original_data)){
          
          group_syms <- syms(input$group_col)
          
          sum_data <- original_data %>%
            group_by(!!!group_syms) %>%
            summarise(total = sum(!!sym(input$summarize_col), na.rm = TRUE)) %>%
            mutate(total = scale_number(total, scale = scale))
          
        } else{
          sum_data <- data.frame()
        }
        
        combined_data <- left_join(df, sum_data, by=input$group_col)
        
        # Scale the numeric columns
        numeric_cols <- sapply(combined_data, is.numeric)
        combined_data <- combined_data %>%
          mutate(across(where(is.numeric), ~ scale_number(., scale = scale)))
        
        
        table_title <- paste("Dados em", switch(scale,
                                                unit = "Unidades",
                                                thousand = "Milhares",
                                                million = "Milhões",
                                                billion = "Bilhões"))
        
        # Get the name of the summarized column
        summarized_col <- names(combined_data)
        summarized_col <- summarized_col[grepl("sum_|mean_|median_|min_|max_", summarized_col)][1]
        
        return(DT::datatable(combined_data,
                             filter = 'top',
                             options = list(
                               dom = 't',
                               order = list(list(which(names(combined_data) == summarized_col) -1 , 'desc')),
                               columnDefs = list(list(className = 'dt-right', targets = which(sapply(combined_data, is.numeric)) - 1))
                             ),
                             caption = htmltools::tags$caption(
                               style = 'caption-side: top; text-align: center;',
                               table_title
                             )))
      } else{
        original_data <- filtered_data()
        if(!is.null(original_data)){
          sum_data <- original_data %>%
            summarise(total = sum(!!sym(input$summarize_col), na.rm = TRUE)) %>%
            mutate(total = scale_number(total, scale = scale))
        }else{
          sum_data <- data.frame()
        }
        combined_data <- cbind(df,sum_data)
        
        # Scale the numeric columns
        numeric_cols <- sapply(combined_data, is.numeric)
        combined_data <- combined_data %>%
          mutate(across(where(is.numeric), ~ scale_number(., scale = scale)))
        
        table_title <- paste("Dados em", switch(scale,
                                                unit = "Unidades",
                                                thousand = "Milhares",
                                                million = "Milhões",
                                                billion = "Bilhões"))
        
        # Get the name of the summarized column
        summarized_col <- names(combined_data)
        summarized_col <- summarized_col[grepl("sum_|mean_|median_|min_|max_", summarized_col)][1]
        
        return(DT::datatable(combined_data,
                             filter = 'top',
                             options = list(
                               dom = 't',
                               order = list(list(which(names(combined_data) == summarized_col) -1 , 'desc')),
                               columnDefs = list(list(className = 'dt-right', targets = which(sapply(combined_data, is.numeric)) - 1))
                             ),
                             caption = htmltools::tags$caption(
                               style = 'caption-side: top; text-align: center;',
                               table_title
                             )))
        
      }
      
    } else {
      # Scale the numeric columns
      numeric_cols <- sapply(df, is.numeric)
      df <- df %>%
        mutate(across(where(is.numeric), ~ scale_number(., scale = scale)))
      
      table_title <- paste("Dados em", switch(scale,
                                              unit = "Unidades",
                                              thousand = "Milhares",
                                              million = "Milhões",
                                              billion = "Bilhões"))
      return(DT::datatable(df,
                           filter = 'top',
                           options = list(
                             dom = 't',
                             columnDefs = list(list(className = 'dt-right', targets = which(sapply(df, is.numeric)) - 1))
                           ),
                           caption = htmltools::tags$caption(
                             style = 'caption-side: top; text-align: center;',
                             table_title
                           )))
    }
  })
}

shinyApp(ui = ui, server = server)