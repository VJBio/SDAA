
Data_Insights_UI_2 <- function(id){
  ns <- NS(id)
  
  tabPanel(
    "Load Data",
    value = "Tab3",

  # fluidRow(
  #   valueBoxOutput(ns("STUDYID")),  # Using ns() for modular ID scoping
  #   valueBoxOutput(ns("AVG_PCL_Value")),
  #   valueBoxOutput(ns("Total_SUBJECT_ID"))
  # ),
  # 
  # fluidRow(
  #   box(
  #     title = "Normal Distribution of PCL Values",
  #     closable = FALSE, 
  #     width = 12,
  #     status = "warning", 
  #     solidHeader = FALSE, 
  #     collapsible = TRUE,
  #     plotlyOutput(ns("Normal_Dist_Plot"))
  #   )
  # ),
  # 
  # fluidRow(
  #   box(
  #     title = "PCL Values Subject ID Wise",
  #     closable = FALSE, 
  #     width = 12,
  #     status = "warning", 
  #     solidHeader = FALSE, 
  #     collapsible = TRUE,
  #     plotlyOutput(ns("Subjectid_viz"))
  #   )
  # )
)
  
}

Data_Insights_server_2 <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # output$Normal_Dist_Plot <- renderPlotly({
    #   Fdata <- data()  # Fetch the reactive data
    #   if (!is.null(Fdata)) {
    #     normal_distribution(data = Fdata, parameter = Fdata$PCLLOQ, xname = "PCLLOQ Range")
    #   }
    # })
    # 
    # output$Subjectid_viz <- renderPlotly({
    #   Fdata <- data()  # Fetch the reactive data
    #   if (!is.null(Fdata)) {
    #     plot_ly(
    #       x = Fdata$SUBJID,
    #       y = Fdata$PCLLOQ,
    #       name = "PCL Values by Subject ID wise",
    #       type = "bar"
    #     )
    #   }
    # })
    
    # output$STUDYID <- renderValueBox({
    #   Fdata <- data() 
    #   valueBox(
    #     elevation = 4,
    #     value = n_distinct(Fdata$STUDYID),
    #     subtitle = "Study ID Value",
    #     color = "primary",
    #     icon = icon("cart-shopping"),
    #     href = "#"
    #   )
    # })
    # 
    # # Render the second value box dynamically
    # output$AVG_PCL_Value <- renderValueBox({
    #   Fdata <- data() 
    #   valueBox(
    #     elevation = 4,
    #     value = mean(Fdata$PCLLOQ),
    #     subtitle = "AVG PCL Value",
    #     color = "primary",
    #     icon = icon("gears")
    #   )
    # })
    # 
    # output$Total_SUBJECT_ID <- renderValueBox({
    #   Fdata <- data() 
    #   valueBox(
    #     elevation = 4,
    #     value = n_distinct(Fdata$SUBJID),
    #     subtitle = "Processing Rate",
    #     color = "primary",
    #     icon = icon("gears")
    #   )
    # })
    
  })
}

Data_Insights_module_2 <- function(id, data) {
  Data_Insights_UI_2(id)
    Data_Insights_server_2(id, data)
}
