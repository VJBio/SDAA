Data_Insights_UI_4 <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Navigation Guidelines",
    value = "Tab5",
    fluidRow(
      column(
        width = 12,
        h3("Navigation Guidelines for SDAA Dashboard", style = "text-align;"),
        p("Welcome to the SDAA Dashboard! Below, you'll find a comprehensive guide to navigating through the various features and modules available in this application. This guide will help you get the most out of each section by understanding what it offers and how to interact with it.", style = "text-align:left;"),
        h4("1. PREQUISITES Tab", style = "text-align:left;"),
        p("- Purpose: This tab allows you to upload necessary data files, including SDAA Excel files, Clin Pharma Lead Normal Values, and Treatment Codes.", style = "text-align:left;"),
        p("- How to Use:", style = "text-align:left;"),
        tags$ul(
          style = "margin-left: 0; padding-left: 0; text-align: left;",
          tags$li("Click on the \"Upload SDAA Excel File\" button to load the primary dataset."),
          tags$li("Use the \"Upload Clin Pharma Lead Normal Values\" button to add reference data for validation."),
          tags$li("\"Upload Treatment Codes\" helps to align data analysis with the corresponding treatment codes.")
        ),
        p("- Features: After uploading, you can see previews of the loaded data in separate tables.", style = "text-align:left;"),
        
        h4("2. SDAA DASHBOARD Tab", style = "text-align:left;"),
        p("- Purpose: This tab provides an overview of the uploaded data with various analytics.", style = "text-align:left;"),
        p("- How to Use:", style = "text-align:left;"),
        tags$ul(
          style = "margin-left: 0; padding-left: 0; text-align: left;",
          tags$li("Use the \"Select Study ID\" dropdown to filter the data based on specific studies."),
          tags$li("Similarly, select \"Subject ID\" to narrow down to specific subjects."),
          tags$li("Click on the \"Update Visuals\" button to refresh the graphs and visualizations based on your selections.")
        ),
        p("- Features: You can view summary metrics like \"Total Records in Data\", visualize \"PCL Values VISIT ID Wise\", and get further insights using dynamic plots.", style = "text-align:left;"),
        
        h4("3. SD LISTING Tab", style = "text-align:left;"),
        p("- Purpose: This tab provides detailed listing data and helps identify abnormalities in the dataset.", style = "text-align:left;"),
        p("- How to Use:", style = "text-align:left;"),
        tags$ul(
          style = "margin-left: 0; padding-left: 0; text-align: left;",
          tags$li("Filters are provided for SUBJID, VISIT, PCTPT, PKACOM, and PKCOML to help locate specific data points."),
          tags$li("Abnormal rows (i.e., rows with PCORRES values outside the range specified by ULOQ and LLOQ) are highlighted for easy identification.")
        ),
        
        h4("4. VISUAL & DATA TABLE Tab", style = "text-align:left;"),
        p("- Purpose: Provides an interactive graph paired with a data table to explore the relationship between concentration levels and other parameters.", style = "text-align:left;"),
        p("- How to Use:", style = "text-align:left;"),
        tags$ul(
          style = "margin-left: 0; padding-left: 0; text-align: left;",
          tags$li("Filters Panel (left-hand side): Use SUBJID, VISIT, PCTPT checkboxes and the search box to filter the data points of interest."),
          tags$li("RESET ALL: Click to reset all the filters to their default state (i.e., all selected)."),
          tags$li("Update Visuals: Once you've made your filter selections, click to update the graph."),
          tags$li("Graph: The graph displays concentration levels vs. time, color-coded by SUBJID. Click on data points to see corresponding rows in the data table."),
          tags$li("Data Table: When you select a point in the graph, detailed information for that observation is shown in the table below the graph.")
        ),
        
        h4("5. HELP Tab", style = "text-align:left;"),
        p("- Purpose: This section provides assistance and resources for troubleshooting or learning about the dashboard.", style = "text-align:left;"),
        p("- How to Use: Browse the help content or refer to FAQs to clarify any doubts or issues you encounter.", style = "text-align:left;"),
        
        h4("6. VERSION HISTORY Tab", style = "text-align:left;"),
        p("- Purpose: View the different versions and updates of the dashboard, keeping track of changes and improvements.", style = "text-align:left;"),
        p("- How to Use: Check each version entry to understand updates, fixes, or new features added over time.", style = "text-align:left;"),
        
        h4("General Tips", style = "text-align:left;"),
        tags$ul(
          style = "margin-left: 0; padding-left: 0; text-align: left;",
          tags$li("Use the search boxes in filter panels to quickly find specific SUBJID values, especially in larger datasets."),
          tags$li("Remember to update visuals after adjusting filters to see the latest data representation."),
          tags$li("Click on the logo in the header to return to the main page quickly.")
        ),
        
        p("This navigation guide should help you explore and use all the functionalities of the SDAA Dashboard efficiently.", style = "text-align:left;")
      )
    )
  )
}

Data_Insights_server_4 <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

Data_Insights_module_4 <- function(id, data) {
  Data_Insights_UI_4(id)
  Data_Insights_server_4(id, data)
}
