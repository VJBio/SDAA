
SDAA_DASHBOARD_UI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Subject's Interactive Visualization"),
  
  dropdown(
    label = "Filters", 
    icon = icon("sliders"),
    status = "primary",
    fluidRow(
      column(2, pickerInput(ns("study_id_select"), "Study ID",
                            choices = NULL, options = list(`live-search` = TRUE))),
      column(2, pickerInput(ns("trtxt_select"), "Treatment", multiple =TRUE,
                            choices = NULL, options = list(`live-search` = TRUE))),
      # Dropdown for Study ID

      column(2, pickerInput(ns("subj_id_select"), "Select Subject ID",
                            choices = NULL, options = list(`live-search` = TRUE))),
      #actionButton
      column(2, actionButton(ns("update_plots"), label = "Update Visuals",
                             class = "btn btn-success", style = "width: 100%; margin-bottom: 20px;"))
      #verbatimTextOutput(ns("records"))
      # Adding update button

    )
  ),


    fluidRow(
      valueBoxOutput(ns("TreatDesc")),  # Using ns() for modular ID scoping

      valueBoxOutput(ns("Biological_Matrix")),

      valueBoxOutput(ns("Category")),

      valueBoxOutput(ns("Bioanalytical_Method")),

      valueBoxOutput(ns("Total_Abnormalities"))

    ),


    fluidRow(
      # box(
      #   title = "Total Records in Data",
      #   closable = FALSE,
      #   width = 4,
      #   status = "warning",
      #   solidHeader = FALSE,
      #   collapsible = TRUE,
      #   echarts4rOutput(ns("rcount"))
      # ),

      box(
        title = "Normal and Abnormal Values based on VISIT",
        
        closable = FALSE,
        width = 12,
        status = "warning",
        solidHeader = FALSE,
        collapsible = TRUE,
        plotlyOutput(ns("pcorres_plot"))
      ),
      box(
        title = "Concentration vs. Time profile",
        #verbatimTextOutput(ns("records")),
        closable = FALSE,
        width = 12,
        status = "warning",
        solidHeader = FALSE,
        collapsible = TRUE,
        plotlyOutput(ns("pcorres_plot2"))
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
      trtxt<-  unique(uploadedData$THdata()$TREATXT)

      updatePickerInput(session, "study_id_select", choices = study_ids)
      updatePickerInput(session, "trtxt_select", choices = trtxt , selected= trtxt)
      
     
      
      
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

      flt<- uploadedData$THdata() %>%
        filter(STUDYID == input$study_id_select, SUBJID == input$subj_id_select ,
               TREATXT %in% input$trtxt_select )
      
      output$records <- renderText({paste("Total Records" ,nrow(flt) ) })
      
      flt
 
    })

   
      
    plot_data <- reactive({
      req(filtered_data(), uploadedData$data1())
      FLT <- filtered_data()

      # Convert PCORRES to numeric,
      data.num <- as.numeric(FLT$PCORRES)
      data.num[is.na(data.num)] <- 0
      FLT$PCORRES <- data.num


      # Convert ULOQ and LLOQ to numeric (removing units)
      normal_values_numeric <- uploadedData$data1()
      normal_values_numeric$Lower_Limit <- as.numeric(normal_values_numeric$Lower_Limit)
      normal_values_numeric$Upper_Limit <- as.numeric(normal_values_numeric$Upper_Limit)

      #print("data1")
      #print(normal_values_numeric)
      # Join and filter
      joined_data <- merge(FLT ,normal_values_numeric, by=c("STUDYID" ,"TREATXT" , "VISIT", "PCTPT") )

      return(joined_data)
    })


    #print("filtered_data")
    #print(filtered_data)

    # filtered_data <- reactive({
    #   req(uploadedData$THdata())
    #   req(input$study_id_select)
    #   req(input$subj_id_select)
    #
    #   uploadedData$THdata() %>%
    #     filter(STUDYID %in% input$study_id_select, SUBJID %in% input$subj_id_select)
    # })
    #print("plot_data")
    #print(plot_data)
   
    
    output$pcorres_plot <- renderPlotly({
      req(plot_data())

      plot_df <- plot_data()

      plot_df <- plot_df %>%
        mutate(

          Status = ifelse( !is.na(Lower_Limit) & !is.na(Upper_Limit) &  (PCORRES < Lower_Limit | PCORRES > Upper_Limit), "Abnormal", "Normal")
        )

      fig <- plot_ly(
        data = plot_df,
        x = ~VISIT,
        y = ~PCORRES,
        type = 'scatter',
        mode = 'markers',
        color = ~Status,
        fill= ~PCTPT,
        colors = c("Normal" = "blue", "Abnormal" = "red"),
        symbol = ~Status,
        symbols = c("Normal" = "circle", "Abnormal" = "cross"),
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

     # fig , shape=PCTPT
      # p<- ggplot(plot_df, aes(x=PCTPT, y=PCORRES , color = Status  )) +
      #   geom_point(size=2 , aes(shape=Status) )+
      #   theme_classic() +
      #   labs(
      #     x = "Visit",
      #     y = "Concentration (ng/mL)",
      #     title = "Concentration Values Over Visits",
      #     subtitle = "Study ID"
      #     
      #   ) #+ facet_wrap(~VISIT , nrow=1)
    })


    
    output$pcorres_plot2 <- renderPlotly({
      req(plot_data())
      
      plot_df <- plot_data()
      
      plot_df <- plot_df %>%
        mutate(
          
          Status = ifelse( !is.na(Lower_Limit) & !is.na(Upper_Limit) &  (PCORRES < Lower_Limit | PCORRES > Upper_Limit), "Abnormal", "Normal")
        )
      
      p<- ggplot(plot_df, aes(x=PCTPT, y=PCORRES, color= Status )) +
        geom_line(aes(linetype=TREATXT, color= TREATXT))+
        geom_point(size=2, aes(shape=Status  ,text = paste("Subject:", SUBJID, "<br>Visit:", VISIT, "<br>Timepoint:", PCTPT )))+
        theme_classic() +
        labs(
          x = "PCTPT",
          y = "Concentration (ng/mL)",
          title = "Concentration Values Over Visits",
          subtitle = "Study ID"
          
        )# +
        #scale_color_manual(labels = c("Abnormal", "Normal"), values = c("blue", "red"))
      ggplotly(p+  guides(color=guide_legend("")) )
     
     
      #fig 
    #p
    })
    
    
    output$TreatDesc <- renderValueBox({0
      req(filtered_data())
      Fdata <- filtered_data()
      valueBox(
        width  = 3,
        value = nrow(Fdata),
        subtitle = "Total Records",
        color = "black",
        href = NULL
      )
    })

    # Rendering the second value box dynamically
    output$Category <- renderValueBox({
      req(filtered_data())
      Fdata <- filtered_data()

      valueBox(
        #elevation = 3,
        value = unique(Fdata$PCCAT),
        subtitle = "Category",
        color = "black"

      )
    })

    output$Biological_Matrix <- renderValueBox({
      req(filtered_data())
      Fdata <- filtered_data()

      valueBox(
        #elevation = 3,
        value = unique(Fdata$PCSPEC),
        subtitle = "Biological Matrix",
        color = "black"

      )
    })


    output$Bioanalytical_Method <- renderValueBox({
      req(filtered_data())
      Fdata <- filtered_data()

      valueBox(
        #elevation = 3,
        value = unique(Fdata$PCMETHOD),
        subtitle = "Bioanalytical Method",
        color = "black"
      )
    })


    output$Total_Abnormalities <- renderValueBox({
      req(uploadedData$THdata(), uploadedData$data1())

      # Joining data to get LLOQ and ULOQ values
      FLT <- uploadedData$THdata()


      data.num <- as.numeric(FLT$PCORRES)
      data.num[is.na(data.num)] <- 0
      FLT$PCORRES <- data.num

      # Convert ULOQ and LLOQ to numeric (removing units)
      normal_values_numeric <- uploadedData$data1()
      normal_values_numeric$Lower_Limit <- as.numeric(normal_values_numeric$Lower_Limit)
      normal_values_numeric$Upper_Limit <- as.numeric(normal_values_numeric$Upper_Limit)

       joined_data <- merge(FLT ,normal_values_numeric, by=c("STUDYID" ,"TREATXT" , "VISIT", "PCTPT") )

       #write.table(as.data.frame(joined_data[c("PCORRES", "Lower_Limit", "Upper_Limit")]), file="ab.txt",sep="\t")
      joined_data <- joined_data %>%
        mutate(
          Status = ifelse( !is.na(Lower_Limit) & !is.na(Upper_Limit) &  (PCORRES < Lower_Limit | PCORRES > Upper_Limit), "Abnormal", "Normal")
        )
       total_abnormalities <- sum(joined_data$Status =="Abnormal")  # Get the count of abnormalities
       #print(joined_data$Status)
      valueBox(
        #elevation = 3,
        value = total_abnormalities,
        subtitle = "Total Abnormalities in Study",
        color = "red",
        icon = icon("exclamation-triangle"),
        href = NULL
      )
    })

    
    #output$records <- renderText({paste("Total Records" ,nrow(filtered_data()) ) })
    
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

      #print("Fdata")
      #print(Fdata)

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
