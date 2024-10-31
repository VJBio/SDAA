

server <- function (input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$user<- renderUser({
    dashboardUser(
      name = " ",
      image = "https://www.iprcenter.gov/image-repository/pfizer_-2021-svg.png/@@images/image.png",
      title = NULL,
      subtitle = "Author - Sushmitha",
      footer = NULL
    )
  })
  
  
  # Call the Data Load Module
  uploadedData <- PREQUISITES_server("PREQUISITES")  # No need to use callModule
  
  # print("Reactive Data")
  # print(head(uploadedData, 10))
  
  # Call the Data Insights Module and pass the reactive data correctly
  SDAA_DASHBOARD_module("SDAA_DASHBOARD", uploadedData)
  Data_Insights_module_2("insights_module_2", uploadedData)
  Data_Insights_module_3("insights_module_3", uploadedData)
  Data_Insights_module_4("insights_module_4", uploadedData)
  Data_Insights_module_5("insights_module_5", uploadedData)
  
} 