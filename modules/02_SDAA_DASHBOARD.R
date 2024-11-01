
SDAA_DASHBOARD_UI <- function(id){
  ns <- NS(id)
  
  tabPanel(
    "Load Data",
    value = "Tab2",
    
    fluidRow(
      column(2, pickerInput(ns("study_id_select"), "Select Study ID", 
                            choices = NULL, options = list(`live-search` = TRUE))),  # Dropdown for Study ID
      
      column(2, pickerInput(ns("subj_id_select"), "Select Subject ID", 
                            choices = NULL, options = list(`live-search` = TRUE))),
      
      column(2, actionBttn(ns("update_plots"), label = "Update Visuals", style = "fill")) # Adding update button
      
    ),
    
    
    fluidRow(
      valueBoxOutput(ns("TreatDesc")),  # Using ns() for modular ID scoping
      
      valueBoxOutput(ns("Biological_Matrix")),
      
      valueBoxOutput(ns("Category")),
      
      valueBoxOutput(ns("Bioanalytical_Method")), 
      
      valueBoxOutput(ns("Total_Abnormalities"))
      
    ),
    
    
    fluidRow(
      box(
        title = "Total Records in Data",
        closable = FALSE,
        width = 5,
        status = "warning",
        solidHeader = FALSE,
        collapsible = TRUE,
        echarts4rOutput(ns("rcount"))
      ),
      
      box(
        title = "Normal and Abnormal Values based on VISIT",
        closable = FALSE,
        width = 7,
        status = "warning",
        solidHeader = FALSE,
        collapsible = TRUE,
        plotlyOutput(ns("pcorres_plot"))
      )
    )
  )
  
}



SDAA_DASHBOARD_server <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Updating Study ID dropdown choices when data is available
    observeEvent(uploadedData$THdata(), {
      study_ids <- unique(uploadedData$THdata()$STUDYID)
      updatePickerInput(session, "study_id_select", choices = study_ids)
    })
    
    
    # Updating Subject ID dropdown based on selected Study ID
    observeEvent(input$study_id_select, {
      req(input$study_id_select) 
      
      filtered_data <- uploadedData$THdata() %>% 
        filter(STUDYID == input$study_id_select)
      
      subj_ids <- unique(filtered_data$SUBJID)
      updatePickerInput(session, "subj_id_select", choices = subj_ids)
    })    
    
    filtered_data <- eventReactive(input$update_plots, { 
      
      req(uploadedData$THdata(), 
          input$study_id_select, 
          input$subj_id_select)
      
      uploadedData$THdata() %>%
        filter(STUDYID == input$study_id_select, SUBJID == input$subj_id_select)
    })
    
    
    plot_data <- reactive({
      req(filtered_data(), uploadedData$data1())
      FLT <- filtered_data()
      
      # Convert PCORRES to numeric,
      FLT <- FLT %>%
        mutate(PCORRES = as.numeric(ifelse(grepl("^<", PCORRES), 0.4, as.numeric(PCORRES))))
      
      # Convert ULOQ and LLOQ to numeric (removing units)
      normal_values_numeric <- uploadedData$data1() %>%
        mutate(ULOQ = as.numeric(gsub("[^0-9.]", "", ULOQ)),  # Extracting numeric part
               LLOQ = as.numeric(gsub("[^0-9.]", "", LLOQ)))
      
      # Join and filter 
      joined_data <- FLT %>%
        left_join(normal_values_numeric, by = c("STUDYID", "TREATXT", "VISIT")) # Joined by relevant columns
      
      
      return(joined_data)
    })
    
    
    print("filtered_data")
    print(filtered_data)
    
    # filtered_data <- reactive({
    #   req(uploadedData$THdata())
    #   req(input$study_id_select)
    #   req(input$subj_id_select)
    #   
    #   uploadedData$THdata() %>%
    #     filter(STUDYID %in% input$study_id_select, SUBJID %in% input$subj_id_select)
    # })
    print("plot_data")
    print(plot_data)
    
    output$pcorres_plot <- renderPlotly({
      req(plot_data())
      
      plot_df <- plot_data()
      
      plot_df <- plot_df %>%
        mutate(Status = ifelse(PCORRES >= LLOQ & PCORRES <= ULOQ, "Normal", "Abnormal"))
      
      fig <- plot_ly(
        data = plot_df,
        x = ~VISIT,
        y = ~PCORRES,
        type = 'scatter',
        mode = 'markers',
        color = ~Status,  
        colors = c("Normal" = "blue", "Abnormal" = "red"),  
        symbol = ~Status,  
        symbols = c("Normal" = "circle", "Abnormal" = "x"),  
        marker = list(
          size = ~ifelse(Status == "Abnormal", 12, 8)  
        ),
        hoverinfo = "text",
        text = ~paste("Subject:", SUBJID, "<br>Visit:", VISIT, "<br>Timepoint:", PCTPT, "<br>PCORRES:", PCORRES)
      ) %>%
        layout(
          title = "Concentration Values Over Visits",
          xaxis = list(title = "Visit"),
          yaxis = list(title = "Concentration"),
          showlegend = TRUE 
        )
      
      fig
    })
    
    
    
    output$TreatDesc <- renderValueBox({
      req(filtered_data())
      Fdata <- filtered_data()
      valueBox(
        elevation = 3,
        value = unique(Fdata$TREATXT),
        subtitle = "Treatment Description",
        color = "primary",
        icon = icon("fa-clipboard"),
        href = NULL
      )
    })
    
    # Rendering the second value box dynamically
    output$Category <- renderValueBox({
      req(filtered_data())
      Fdata <- filtered_data()
      
      valueBox(
        elevation = 3,
        value = unique(Fdata$PCCAT),
        subtitle = "Category",
        color = "primary",
        icon = icon("input-numeric")
      )
    })
    
    output$Biological_Matrix <- renderValueBox({
      req(filtered_data())
      Fdata <- filtered_data()
      
      valueBox(
        elevation = 3,
        value = unique(Fdata$PCSPEC),
        subtitle = "Biological Matrix",
        color = "primary",
        icon = icon("memo")
      )
    })
    
    
    output$Bioanalytical_Method <- renderValueBox({
      req(filtered_data())
      Fdata <- filtered_data()
      
      valueBox(
        elevation = 3,
        value = unique(Fdata$PCMETHOD),
        subtitle = "Bioanalytical Method",
        color = "primary",
        icon = icon("memo")
      )
    })
    
    
    output$Total_Abnormalities <- renderValueBox({
      req(uploadedData$THdata(), uploadedData$data1())
      
      # Joining data to get LLOQ and ULOQ values
      FLT <- uploadedData$THdata()
      
      # Convert PCORRES to numeric, handling "<" values by assuming 0.3 when PCORRES == "<0.400"
      FLT <- FLT %>%
        mutate(PCORRES = as.numeric(ifelse(grepl("^<", PCORRES), 0.3, as.numeric(PCORRES))))
      
      # Convert ULOQ and LLOQ to numeric (removing units)
      normal_values_numeric <- uploadedData$data1() %>%
        mutate(ULOQ = as.numeric(gsub("[^0-9.]", "", ULOQ)),  # Extract numeric part
               LLOQ = as.numeric(gsub("[^0-9.]", "", LLOQ)))
      
      # Join and filter to get abnormal values (Adjusted join and filter logic)
      joined_data <- FLT %>%
        left_join(normal_values_numeric, by = c("STUDYID", "TREATXT", "VISIT")) %>%
        filter(PCORRES < LLOQ | PCORRES > ULOQ)  # Filtering to get only abnormal values
      
      total_abnormalities <- nrow(joined_data)  # Get the count of abnormalities
      
      valueBox(
        elevation = 3,
        value = total_abnormalities,
        subtitle = "Total Abnormalities in Study",
        color = "danger",
        icon = icon("exclamation-triangle"),
        href = NULL
      )
    })
    
    
    output$rcount <- renderEcharts4r({
      req(filtered_data())
      Fdata <- filtered_data()
      nrfdata <- nrow(Fdata)
      
      e_charts() %>%
        e_gauge(as.numeric(nrfdata), "Records")  # Correct: nrow(Fdata)
      # e_title("Total Records in Data")
    })
    
    output$Uniq_SubjID <- renderEcharts4r({
      req(uploadedData$THdata())
      Fdata <- uploadedData$THdata()
      
      print("Fdata")
      print(Fdata)
      
      distinct_count <- Fdata %>%
        summarise(USUBJID = n_distinct(SUBJID))
      
      e_charts() %>%
        e_gauge(as.numeric(distinct_count$USUBJID), "Records") %>%  # Correct: nrow(Fdata)
        e_title("Total Subject ID in Data")
      
      
    })
    
    # output$Normal_Dist_Plot <- renderPlotly({
    #   
    #   req(filtered_data())
    #   
    #   Fdata <- filtered_data()  # Fetch the reactive data
    #   
    #   if (!is.null(Fdata)) {
    #     normal_distribution(data = Fdata, parameter = Fdata$PCLLOQ, xname = "PCLLOQ Range")
    #   }
    # })
    # 
    # output$Subjectid_viz <- renderPlotly({
    #   req(filtered_data())
    #   
    #   Fdata <- filtered_data() # Fetch the reactive data
    #   if (!is.null(Fdata)) {
    #     plot_ly(
    #       x = Fdata$SUBJID,
    #       y = Fdata$PCLLOQ,
    #       name = "PCL Values by Subject ID wise",
    #       type = "bar"
    #     )
    #   }
    # })
    
    
  })
}

SDAA_DASHBOARD_module <- function(id, uploadedData) {
  SDAA_DASHBOARD_UI(id)
  SDAA_DASHBOARD_server(id, uploadedData)
}