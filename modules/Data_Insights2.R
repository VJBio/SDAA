
Data_Insights_UI_2 <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("SD Listing with Abnormal Value Highlighting"),
    
    # fluidRow(
    #   column(
    #     width = 3,  # Sidebar panel for filters
    #     div(
    #       id = "filter-panel",
    #       style = "background-color: #f9f9f9; padding: 10px; border-radius: 5px; max-height: 90vh; overflow-y: auto;",
    #       
    #       h4("Filters", style = "text-align:center; margin-top: 15px;"),
    #       
    #       # Visit Filter
    #       tags$div(
    #         h5("Select VISIT:"),
    #         tags$div(
    #           style = "height: 150px; overflow-y: scroll; border: 1px solid #ccc; padding: 5px;",
    #           checkboxGroupInput(ns("visit_filter"), label = NULL, choices = NULL, selected = NULL)
    #         )
    #       ),
    #       
    #       # PCTPT Filter
    #       tags$div(
    #         h5("Select PCTPT:"),
    #         tags$div(
    #           style = "height: 150px; overflow-y: scroll; border: 1px solid #ccc; padding: 5px;",
    #           checkboxGroupInput(ns("pctpt_filter"), label = NULL, choices = NULL, selected = NULL)
    #         )
    #       ),
    #       
    #       # Subject ID Filter
    #       tags$div(
    #         h5("Select SUBJECT ID:"),
    #         tags$div(
    #           style = "height: 150px; overflow-y: scroll; border: 1px solid #ccc; padding: 5px;",
    #           checkboxGroupInput(ns("subjid_filter"), label = NULL, choices = NULL, selected = NULL)
    #         )
    #       ),
    #       
    #       # PKACOM Filter
    #       tags$div(
    #         h5("Select PKACOM:"),
    #         tags$div(
    #           style = "height: 150px; overflow-y: scroll; border: 1px solid #ccc; padding: 5px;",
    #           checkboxGroupInput(ns("pkacom_filter"), label = NULL, choices = NULL, selected = NULL)
    #         )
    #       ),
    #       
    #       # PKCOML Filter
    #       tags$div(
    #         h5("Select PKCOML:"),
    #         tags$div(
    #           style = "height: 150px; overflow-y: scroll; border: 1px solid #ccc; padding: 5px;",
    #           checkboxGroupInput(ns("pkcoml_filter"), label = NULL, choices = NULL, selected = NULL)
    #         )
    #       )
    #     )
    #   ),
      column(
        width = 12,  # Main content panel for data table
        div(
          id = "data-table-container",
          #style = "padding: 10px;",
          DTOutput(ns("filtered_data_table"))
        )
      )
    #)
  )
}


Data_Insights_server_2 <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive dataset to load  and data1
    data <- reactive({
      req(uploadedData$THdata(), uploadedData$data1())  
      
      
      df <- uploadedData$THdata()
      normal_values <- uploadedData$data1()
      ##########################################################################
      # Debugging: Print summaries of both datasets to ensure they are loaded
      #cat("THdata Summary:\n")
      #print(head(df))
      #cat("Normal Values Summary:\n")
      #print(head(normal_values))
      ######################################################################
      return(list(df = df, normal_values = normal_values))
    })
    
    # Updating filter options dynamically
    observe({
      req(data())  # Ensure data is available
      df <- data()$df
      
      updateCheckboxGroupInput(session, "visit_filter", choices = unique(df$VISIT), selected = unique(df$VISIT))
      updateCheckboxGroupInput(session, "pctpt_filter", choices = unique(df$PCTPT), selected = unique(df$PCTPT))
      updateCheckboxGroupInput(session, "subjid_filter", choices = unique(df$SUBJID), selected = unique(df$SUBJID))
      updateCheckboxGroupInput(session, "pkacom_filter", choices = unique(df$PKACOM), selected = unique(df$PKACOM))
      updateCheckboxGroupInput(session, "pkcoml_filter", choices = unique(df$PKCOML), selected = unique(df$PKCOML))
    })
    
    # Reactive dataset to add abnormality status
    processed_data <- reactive({
      req(data())  # Ensure data is available
      df <- data()$df
      normal_values <- data()$normal_values
      
    
      
      data.num <- as.numeric(df$PCORRES)
      data.num[is.na(data.num)] <- 0
      df$PCORRES_numeric <- data.num
      df$PCORRES<- data.num
      # Convert ULOQ and LLOQ to numeric (removing units)
      
      normal_values$Lower_Limit <- as.numeric(normal_values$Lower_Limit)
      normal_values$Upper_Limit <- as.numeric(normal_values$Upper_Limit)
      
      
      
      # Joining with normal_values to bring LLOQ and ULOQ based on VISIT
      # df <- df %>%
      #   left_join(normal_values %>% select(VISIT, Upper_Limit, Lower_Limit), by = "VISIT")
      # df$Upper_Limit <-as.numeric(df$Upper_Limit)
      # df$Lower_Limit <-as.numeric(df$Lower_Limit)
      df <- merge(df ,normal_values, by=c("STUDYID" ,"TREATXT" , "VISIT", "PCTPT") )
      
      # Add a status column to determine if values are abnormal
      df <- df %>%
        mutate(
          Status = ifelse(!is.na(Lower_Limit) & !is.na(Upper_Limit) & (PCORRES_numeric < Lower_Limit | PCORRES_numeric > Upper_Limit), "Abnormal", "Normal")
        )
      
      # Debugging: Check the status column and LLOQ/ULOQ values
      #cat("Data with Status Column Summary:\n")
      #print(head(df))
      
      return(df)
    })
    
    # Rendering the filtered data table with highlighting for abnormal values
    output$filtered_data_table <- renderDT({
      req(processed_data())  # Ensuring processed data is available
      
      df <- processed_data()

      # Rendering the data table with highlighting for abnormalities
      datatable(df, options = list( scrollX = TRUE), rownames = FALSE) %>%
        formatStyle(
          columns = names(df),
          target = 'row',
          backgroundColor = styleEqual("Abnormal", "pink")  # Highlight rows with "Abnormal" status
        )
    })
  })
}


Data_Insights_module_2 <- function(id, uploadedData) {
  Data_Insights_UI_2(id)
  Data_Insights_server_2(id, uploadedData)
}

