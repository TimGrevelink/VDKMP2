function(input, output, session) {
  
  # UI------
  output$ui_sidebar <- renderMenu({
    tabs.out <- list() 
    tabs.out[[length(tabs.out)+1]] <- fileInput("file1", "Kies CSV File", placeholder = "Geen CSV geselecteerd")
    tabs.out[[length(tabs.out)+1]] <- menuItem("Ingelezen tabel", tabName = "dataTable", 
                                               icon = shiny::icon("fas fa-table", lib = "font-awesome"))
    tabs.out[[length(tabs.out)+1]] <- menuItem("Uitkomsten", tabName = "uitkomsten", 
                                               icon = shiny::icon("fas fa-chart-line", lib = "font-awesome"))
    
    return(sidebarMenu(
      .list = tabs.out,
      id = "sidebarmenu")
    )
  })
  
  output$test_ui <- renderUI({
    items <- c(
      list(tabItem(tabName = "dataTable",
                   fluidPage(
                     fluidRow(
                       div(class = "col-md-12 col-lg-10", 
                           box(title = "Antwoorden tabel", status = "primary", solidHeader = TRUE, width = "100%",
                               DT::dataTableOutput("csvData")
                           )
                           
                           
                       )
                     )
                   )
      )
      ),
      list(tabItem(tabName = "uitkomsten",
                   fluidPage(
                     fluidRow(
                       div(class = "col-md-12 col-lg-10", 
                           box(title = "Selectie en filters", status = "primary", solidHeader = TRUE, width = "100%",
                               selectizeInput("prefix", label = "Type vragen", choices = NULL)
                           )
                       )
                     ),
                     fluidRow(
                       div(class = "col-md-12 col-lg-10", 
                           box(title = "Barchart gemiddelde uitkomst per domein", status = "primary", solidHeader = TRUE, width = "100%",
                               plotlyOutput("barchartPlot", height = 600)
                           )
                       )
                     ),
                     fluidRow(
                       div(class = "col-md-12 col-lg-10", 
                           box(title = "Tabel gemiddelde uitkomst per domein", status = "primary", solidHeader = TRUE, width = "100%",
                               dataTableOutput("avgOutcomes")
                           )
                       )
                     )
                     
                   )
      )
      )
    )
    do.call(tabItems, items)
  })
  
  # Read data ----
  
  loadData <- reactive({
    if (is.null(input$file1)){
      return(NULL)
    } else{
      file <- input$file1$datapath[1]
      dt <- tryCatch(
        {
          dt <- as.data.table(read.csv(file, header = T, sep = ",", check.names = FALSE, colClasses = "character") )
        }, error = function(cond){
          stop("The file can not be read.")
        }, warning = function(cond){
          if (file.exists(file)){
            cat("\n", file = file, append = TRUE)
            dt <- as.data.table(read.csv(file, header = T, sep = ",", check.names = FALSE, colClasses = "character") )
          } else{
            stop("The file is not available.")
          }
        }
      )
      
      return(dt)
    }
  })
  
  output$csvData <- DT::renderDataTable({
    dt <- loadData()
    dt.new = datatable(dt, options = list(scrollX = T)) 
    if (!is.null(dt)){
      dt.new = dt.new %>%
        formatStyle(
          columns = colnames(dt),
          color = styleInterval(1, c('red', 'black')),
          backgroundColor = styleEqual("", 'red')
        )
    }
    return(dt.new)
  })
  
  # Preprocess data ----
  
  preprocessData <- reactive({
    data <- loadData()
    
    if (!is.null(data)){
      data = data[, `:=` (Tijdstempel = NULL,
                          Gebruikersnaam = NULL)]
      # rename the columns that are in a specific order. Names can occur multiple times
      column.names <- unique(colnames(data))
      tmp <- sapply(column.names, function(x){
        col.nrs <- which(colnames(data) == x)
        numberof.cols <- length(col.nrs)
        if ( numberof.cols > 1){
          if (numberof.cols < length(domains)){
            warning(paste0("Kolom ", x, " komt slechts ", numberof.cols, " keer voor."))
          } else if(numberof.cols > length(domains)){
            warning(paste0("Kolom ", x, " komt te vaak voor. Namelijk ", numberof.cols, " keer."))
          }
          setnames(data, old = c(col.nrs), new = paste0(letters[1:length(col.nrs)], x))
        }
      })
      data = data[, lapply(.SD, as.numeric)]
      
      data = data[, lapply(.SD, mean, na.rm = T)]
      
    }
    
    return(data)
  })
  
  rankingOutcomesTable <- reactive({
    data <- preprocessData()
    if (!is.null(data)){
      prefixes <- letters[1:length(domains)]
      column.names = colnames(data)[startsWith(colnames(data), prefixes[1])] 
      column.names.trim = substr(column.names, 2, nchar(column.names))
      
      dt.new <- setNames(data.table(matrix(nrow = length(column.names.trim), ncol = 3)),  domains)[, lapply(.SD, as.numeric), .SDcols = domains]
      dt.new = dt.new[, Domeinen := column.names.trim ]
      setcolorder(dt.new, c("Domeinen", domains))
      for (i in 1: length(domains)){
        column.names = colnames(data)[startsWith(colnames(data), prefixes[i])]
        set(dt.new, j = domains[i], value = t(data[,.SD,.SDcols = column.names]))
      }
      dt.new = dt.new[, prefix := substr(Domeinen,1,1)]
    } else{
      dt.new = data
    }
    
    
    return (dt.new)
    
  })
  
  observeEvent(rankingOutcomesTable(),{
    dt <- rankingOutcomesTable()
    if (!is.null(dt)){
      updateSelectInput(session, "prefix", choices = unique(rankingOutcomesTable()$prefix))
    }
    
  })
  
 
  output$avgOutcomes <- renderDataTable({
    dt <- rankingOutcomesTable()
    if (!is.null(dt)){
      dt = dt[prefix == input$prefix][, prefix := NULL]
    }
    datatable(dt, options = list(scrollX = T, scrollY = "300px", paging = FALSE))
  })
  
  
  # Make plot --------
  
  output$barchartPlot <- renderPlotly({
    
    dt <- rankingOutcomesTable()
    
    if (!is.null(dt)) {
      dt = dt[prefix == input$prefix][, prefix := NULL]
      p <- plot_ly(data = dt)
      
      for (i in 1:length(domains)){
        p = p %>% add_trace( x = dt[[domains[i]]], y = ~Domeinen, type = 'bar', orientation = 'h', name = domains[i],
                          marker = list(color = colors[i],
                                        line = list(color = colors[i])))
      }
      p = p %>%
        layout(barmode = 'stack',
               xaxis = list(title = "", domain = c(0.4, 1), range = c(0, 15)),
               yaxis = list(title =""))
      
    } else {
      p <- plot_ly()
    }
    
    return(p)
    
  })
  
}