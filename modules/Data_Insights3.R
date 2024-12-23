
# Updated UI Module
Data_Insights_UI_3 <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Interactive Graph with Data Table"),
    fluidRow(
      column(
        width = 2,  # Sidebar occupies 1/4th of the screen
        div(
          id = "filter-panel",
          style = "background-color: #f9f9f9; padding: 10px; border-radius: 5px; max-height: 90vh; overflow-y: auto;",
          
          # Reset and Update Buttons
          actionButton(ns("reset_filters"), "RESET ALL", class = "btn btn-primary", style = "width: 100%; margin-bottom: 20px;"),
          actionButton(ns("update_plots"), "Update Visuals", class = "btn btn-success", style = "width: 100%; margin-bottom: 20px;"),
          
          # Filters Section
          h4("Filters", style = "text-align:center; margin-top: 15px;"),
          
          # Search Subject ID
          textInput(ns("subjid_search"), "Search SUBJECT ID:", value = ""),
          
          # Subject ID Filter
          tags$div(
            h5("Select SUBJECT ID:"),
            tags$div(
              style = "height: 150px; overflow-y: scroll; border: 1px solid #ccc; padding: 5px;",
              checkboxGroupInput(ns("subjid_filter"), label = NULL, choices = NULL, selected = NULL)
            )
          ),
          
          # Visit Filter
          tags$div(
            h5("Select VISIT:"),
            tags$div(
              style = "height: 150px; overflow-y: scroll; border: 1px solid #ccc; padding: 5px;",
              checkboxGroupInput(ns("visit_filter"), label = NULL, choices = NULL, selected = NULL)
            )
          ),
          
          # PCTPT Filter
          tags$div(
            h5("Select PCTPT:"),
            tags$div(
              style = "height: 150px; overflow-y: scroll; border: 1px solid #ccc; padding: 5px;",
              checkboxGroupInput(ns("pctpt_filter"), label = NULL, choices = NULL, selected = NULL)
            )
          ),
          
          # Graph Theme Selector
          # selectInput(ns("theme_choice"), "Choose Graph Theme:",
          #             choices = c("Minimal" = "theme_minimal",
          #                         "Classic" = "theme_classic",
          #                         "Black & White" = "theme_bw",
          #                         "Light" = "theme_light",
          #                         "Dark" = "theme_dark",
          #                         "Void" = "theme_void",
          #                         "Gray" = "theme_gray"))
        )
      ),
      column(
        width = 10,  # Main content occupies 3/4th of the screen
        fluidRow(
          column(
            width = 12,
            div(
              id = "plot-container",
              style = "padding: 10px;",
              plotlyOutput(ns("interactive_plot"), height = "500px")
            )
          )
        )#,
        # fluidRow(
        #   column(
        #     width = 12,
        #     div(
        #       id = "data-table-container",
        #       style = "padding: 10px;",
        #       DTOutput(ns("data_table"))
        #     )
        #   )
        # )
      )
    )
  )
}


Data_Insights_server_3 <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #print(ns)
    # Reactive dataset using the uploadedData$THdata()
    data <- reactive({
      req(uploadedData$THdata())  # Ensure THdata is available
      df <- uploadedData$THdata()
      

      #####################
      #df <-  readxl::read_excel("../VJ/c1071003_pf-06863135_serum_mock_dft_v2.xlsx")
      #######################
      data.num <- as.numeric(df$PCORRES)
      data.num[is.na(data.num)] <- 0
      df$PCORRES_numeric <- data.num
      
      
      # Converting timepoint to numeric for sorting purposes
      df$time_numeric <- as.numeric(sub(" H", "", df$PCTPT))
      
      # Order timepoint dynamically and create a factor with ordered levels
      df$PCTPT_factor <- factor(df$PCTPT, levels = unique(df$PCTPT[order(df$time_numeric)]))
      
      # Creating a unique identifier for each VISIT and SUBJID combination
      df$visit_id <- paste(df$SUBJID, df$VISIT, sep = "_")
      
      return(df)
    })
    
    # Reactive expression to filter SUBJID based on the search input
    filtered_subjid <- reactive({
      req(data())
      if (input$subjid_search == "") {
        unique(data()$SUBJID)
      } else {
        unique(data()$SUBJID[grepl(input$subjid_search, data()$SUBJID, ignore.case = TRUE)])
      }
    })
    
    # Updating checkboxGroupInput for SUBJID based on the search input
    observe({
      req(filtered_subjid())
      updateCheckboxGroupInput(
        session,
        "subjid_filter",
        choices = filtered_subjid(),
        selected = filtered_subjid()  
      )
    })
    
    # Updating filter options dynamically 
    observe({
      req(data())
      df <- data()
      
      # Initialize filters with all values selected by default
      updateCheckboxGroupInput(session, "subjid_filter",
                               choices = unique(df$SUBJID),
                               selected = unique(df$SUBJID))
      
      updateCheckboxGroupInput(session, "visit_filter",
                               choices = unique(df$VISIT),
                               selected = unique(df$VISIT))
      
      updateCheckboxGroupInput(session, "pctpt_filter",
                               choices = unique(df$PCTPT),
                               selected = unique(df$PCTPT))
    })
    
    
    observeEvent(input$reset_filters, {
      req(data())
      df <- data()
      
      # Reset search and all filters to initial values
      updateTextInput(session, "subjid_search", value = "")
      updateCheckboxGroupInput(session, "subjid_filter", choices = unique(df$SUBJID), selected = unique(df$SUBJID))
      updateCheckboxGroupInput(session, "visit_filter", choices = unique(df$VISIT), selected = unique(df$VISIT))
      updateCheckboxGroupInput(session, "pctpt_filter", choices = unique(df$PCTPT), selected = unique(df$PCTPT))
    })
    
    # Filtered dataset based on checkbox input
    filtered_data <- reactive({
      req(data())
      
      # If no specific filters have been applied, should return the entire dataset
      subjid_filter <- input$subjid_filter
      visit_filter <- input$visit_filter
      pctpt_filter <- input$pctpt_filter
      
      if (is.null(subjid_filter)) {
        subjid_filter <- unique(data()$SUBJID)
      }
      if (is.null(visit_filter)) {
        visit_filter <- unique(data()$VISIT)
      }
      if (is.null(pctpt_filter)) {
        pctpt_filter <- unique(data()$PCTPT)
      }
      
      # Filtering the data based on the inputs
      filtered <- data() %>%
        filter(
          SUBJID %in% subjid_filter,
          VISIT %in% visit_filter,
          PCTPT %in% pctpt_filter
        )
      
      return(filtered)
    })
    
    # Creating the ggplot object based on filtered data
    output$interactive_plot <- renderPlotly({
      req(filtered_data())
      filtered <- filtered_data()
      
      if (nrow(filtered) == 0) {
        return(NULL)
      }
      
      # Creating ggplot object with dynamic theme selection
      #ggplot_obj <- ggplot(filtered, aes(x = PCTPT_factor, y = PCORRES_numeric, color = as.factor(SUBJID), group = visit_id, linetype = VISIT)) +
      #  geom_line(linewidth = 1) +  # Updated for compatibility with ggplot2 v3.4.0
      #  geom_point(size = 3) +
      #  scale_y_log10() +  # Log scale for y-axis
      #  scale_color_viridis(discrete = TRUE) +  # Use a different color palette for SUBJID (discrete scale)
      #  labs(
      #    x = "Time (Hours)",
      #    y = "Concentration (ng/mL)",
      #    title = "Concentration vs Time by Subject and Visit",
      #   color = "Subject ID",
      #    linetype = "Visit"
      #  )
      
      data.num <- as.numeric(filtered$PCORRES)
      data.num[is.na(data.num)] <- 0
      filtered$PCORRES <- data.num
      #filtered <-  na.omit(filtered)
      ggplot_obj <- ggplot(filtered, aes(x = VISIT, y = PCORRES, color=PCTPT)) +
        geom_boxplot() +
        geom_point(position = position_jitter(width = 0.2)) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
              legend.position = "top",
              text = element_text(size=8)) +
        facet_wrap(~PCTPT , scales = "free_x" , nrow=1) +
        labs(
          x = "Time (Hours)",
          y = "Concentration (ng/mL)",
          title = "Concentration vs Time by Subject and Visit",
          
        ) 
     ggplotly(ggplot_obj)
     
     
  
     
     
      # Applying the chosen theme based on user input
      # ggplot_obj <- switch(input$theme_choice,
      #                      theme_minimal = ggplot_obj + theme_minimal(),
      #                      theme_classic = ggplot_obj + theme_classic(),
      #                      theme_bw = ggplot_obj + theme_bw(),
      #                      theme_light = ggplot_obj + theme_light(),
      #                      theme_dark = ggplot_obj + theme_dark(),
      #                      theme_void = ggplot_obj + theme_void(),
      #                      theme_gray = ggplot_obj + theme_gray())
      
      # Converting ggplot object to plotly and register the click event
     #  p <- ggplotly(ggplot_obj, source = "select") %>%
     #    layout(dragmode = "select")
     #  #event_register(p, "plotly_click")  # Register plotly click event
     #  p
     #  #th<- uploadedData$data1()
     #  #print(th)
     #  #normal_values <- subset(th , th$STUDYID ==unique(filtered$STUDYID) , )
     #  normal_values <- uploadedData$data1()
     #  
     # # print(normal_values)
     # 
     #  library(plotly)
     #  fig <- plot_ly( filtered ,x=~VISIT , y = ~PCORRES_numeric, color = ~PCTPT_factor , 
     #                  type = "box" , boxpoints = "all", jitter = 0.3 )  %>%
     #    layout(
     #      shapes = list(
     #        list(
     #          type = "line",
     #          x0 = 0,
     #          x1 = 1,
     #          y0 = 900,
     #          y1 = 900,
     #          xref = "paper",
     #          line = list(color = "red", width = 2, dash = "dash")
     #        ),
     #        list(
     #          type = "line",
     #          x0 = 0,
     #          x1 = 1,
     #          y0 = 5,
     #          y1 = 5,
     #          xref = "paper",
     #          line = list(color = "blue", width = 2, dash = "dash")
     #        )
     #      )
     #    )
     #  fig <- fig %>%   
     #    subplot(nrows = 1, shareX = TRUE, shareY = TRUE)
     # 
     #  
     #  fig <- fig  %>% layout(
     #        title = "Concentration vs Time by Subject and Visit")
     #  fig
     #  
      #p
    })
    
    # Initially rendering an empty data table
    output$data_table <- renderDT({
      datatable(
        data.frame(Message = "Please select a point on the plot to see details."),
        options = list(pageLength = 5, scrollX = TRUE, scrollY = "300px"),
        style = "bootstrap"
      )
    })
    
    
    observeEvent(event_data("plotly_click", source = "select"), {
      selected_data <- event_data("plotly_click", source = "select")
      
      if (!is.null(selected_data)) {
        levels_list <- levels(data()$PCTPT_factor)
        selected_x_value <- levels_list[selected_data$x]
        selected_y_value <- 10^as.numeric(selected_data$y)
        
        # Use a tolerance for floating-point comparison
        tolerance <- 1e-2
        selected_rows <- filtered_data() %>%
          filter(
            abs(PCORRES_numeric - selected_y_value) < tolerance &
              as.character(PCTPT_factor) == selected_x_value
          )
        
        selected_rows_to_display <- selected_rows %>%
          select(-PCORRES_numeric, -time_numeric, -PCTPT_factor, -visit_id)
        
        if (nrow(selected_rows_to_display) > 0) {
          output$data_table <- renderDT({
            datatable(
              selected_rows_to_display,
              options = list(pageLength = 5, scrollX = TRUE, scrollY = "300px"),
              style = "bootstrap"
            )
          })
        } else {
          output$data_table <- renderDT({
            datatable(
              data.frame(Message = "No matching record found."),
              options = list(pageLength = 5, scrollX = TRUE, scrollY = "300px"),
              style = "bootstrap"
            )
          })
        }
      }
    })
  })
}

# Module for Data Insights 3
Data_Insights_module_3 <- function(id, uploadedData) {
  Data_Insights_UI_3(id)
  Data_Insights_server_3(id, uploadedData)
}
