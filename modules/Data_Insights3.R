
# Updated UI Module
Data_Insights_UI_3 <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Interactive Graph with Data Table"),
    fluidRow(
      column(
        width = 3,  # Sidebar occupies 1/4th of the screen
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
          selectInput(ns("theme_choice"), "Choose Graph Theme:",
                      choices = c("Minimal" = "theme_minimal",
                                  "Classic" = "theme_classic",
                                  "Black & White" = "theme_bw",
                                  "Light" = "theme_light",
                                  "Dark" = "theme_dark",
                                  "Void" = "theme_void",
                                  "Gray" = "theme_gray"))
        )
      ),
      column(
        width = 9,  # Main content occupies 3/4th of the screen
        fluidRow(
          column(
            width = 12,
            div(
              id = "plot-container",
              style = "padding: 10px;",
              plotlyOutput(ns("interactive_plot"), height = "500px")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              id = "data-table-container",
              style = "padding: 10px;",
              DTOutput(ns("data_table"))
            )
          )
        )
      )
    )
  )
}


Data_Insights_server_3 <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive dataset using the uploadedData$THdata()
    data <- reactive({
      req(uploadedData$THdata())  # Ensure THdata is available
      df <- uploadedData$THdata()
      
      # Convert concentration to numeric dynamically, handling "<" values
      df$PCORRES_numeric <- sapply(df$PCORRES, function(x) {
        if (grepl("^<", x)) {
          limit_value <- as.numeric(sub("<", "", x))
          return(limit_value * 0.75)
        } else {
          return(as.numeric(x))
        }
      })
      
      # Convert timepoint to numeric for sorting purposes
      df$time_numeric <- as.numeric(sub(" H", "", df$PCTPT))
      
      # Order timepoint dynamically and create a factor with ordered levels
      df$PCTPT_factor <- factor(df$PCTPT, levels = unique(df$PCTPT[order(df$time_numeric)]))
      
      # Create a unique identifier for each VISIT and SUBJID combination
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
    
    # Update checkboxGroupInput for SUBJID based on the search input
    observe({
      req(filtered_subjid())
      updateCheckboxGroupInput(
        session,
        "subjid_filter",
        choices = filtered_subjid(),
        selected = filtered_subjid()  # Select all the values available after the search
      )
    })
    
    # Update filter options dynamically when data is available
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
    
    # Observe the RESET button click and reset all filters to their default values
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
      
      # If no specific filters have been applied, return the entire dataset
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
      
      # Filter the data based on the inputs
      filtered <- data() %>%
        filter(
          SUBJID %in% subjid_filter,
          VISIT %in% visit_filter,
          PCTPT %in% pctpt_filter
        )
      
      return(filtered)
    })
    
    # Create the ggplot object based on filtered data
    output$interactive_plot <- renderPlotly({
      req(filtered_data())
      filtered <- filtered_data()
      
      if (nrow(filtered) == 0) {
        return(NULL)
      }
      
      # Create ggplot object with dynamic theme selection
      ggplot_obj <- ggplot(filtered, aes(x = PCTPT_factor, y = PCORRES_numeric, color = as.factor(SUBJID), group = visit_id, linetype = VISIT)) +
        geom_line(linewidth = 1) +  # Updated for compatibility with ggplot2 v3.4.0
        geom_point(size = 3) +
        scale_y_log10() +  # Log scale for y-axis
        scale_color_viridis(discrete = TRUE) +  # Use a different color palette for SUBJID (discrete scale)
        labs(
          x = "Time (Hours)",
          y = "Concentration (ng/mL)",
          title = "Concentration vs Time by Subject and Visit",
          color = "Subject ID",
          linetype = "Visit"
        )
      
      # Apply the chosen theme based on user input
      ggplot_obj <- switch(input$theme_choice,
                           theme_minimal = ggplot_obj + theme_minimal(),
                           theme_classic = ggplot_obj + theme_classic(),
                           theme_bw = ggplot_obj + theme_bw(),
                           theme_light = ggplot_obj + theme_light(),
                           theme_dark = ggplot_obj + theme_dark(),
                           theme_void = ggplot_obj + theme_void(),
                           theme_gray = ggplot_obj + theme_gray())
      
      # Convert ggplot object to plotly and register the click event
      p <- ggplotly(ggplot_obj, source = "select") %>%
        layout(dragmode = "select")
      event_register(p, "plotly_click")  # Register plotly click event
      
      p
    })
    
    # Initially render an empty data table
    output$data_table <- renderDT({
      datatable(
        data.frame(Message = "Please select a point on the plot to see details."),
        options = list(pageLength = 5, scrollX = TRUE, scrollY = "300px"),
        style = "bootstrap"
      )
    })
    
    # Observe the click event on the plotly plot
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
