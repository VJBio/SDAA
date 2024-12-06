

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
  
  
  # Calling the Data Load Module
  uploadedData <- PREQUISITES_server("PREQUISITES")  
  
   print("Reactive Data")
   print(head(uploadedData, 10))
  
  # Calling the Data Insights Module and passing the reactive data correctly
  EditTable_module("Threshold",uploadedData)
  SDAA_DASHBOARD_module("SDAA_DASHBOARD", uploadedData)
  Data_Insights_module_2("insights_module_2", uploadedData)
  Data_Insights_module_3("insights_module_3", uploadedData)
  Data_Insights_module_4("insights_module_4", uploadedData)
  Data_Insights_module_5("insights_module_5", uploadedData)
  
} 

############################################################## 
# 
# server <- function(input, output, session) {
#   
#   session$onSessionEnded(function() {
#     stopApp()
#   })
#   
#   # User Profile rendering
#   output$user <- renderUser({
#  #    #req(credentials()$user_auth)
#      dashboardUser(
#        name = " ",
#        image = "https://www.iprcenter.gov/image-repository/pfizer_-2021-svg.png/@@images/image.png",
#       title = NULL,
#       subtitle = "Author - Sushmitha",
#        footer = NULL
#      )
#    })
#  # 
#  # only when credentials()$user_auth is TRUE, render your desired sidebar menu
#   #if(credentials()$user_auth) {
#   # Calling the Data Load Module
#   
# #  LOGIN <- LoGINMODULE_Server("LOGINMODULE")
# #  print (LOGIN)
#   #if(credentials()$user_auth) {
#   uploadedData <- PREQUISITES_server("PREQUISITES")  
#    #print("Reactive Data")
#    #+200.  print(head(uploadedData, 10))
#   
#   # Calling the Data Insights Modules
#  SDAA_DASHBOARD_module("SDAA_DASHBOARD", uploadedData)
#   Data_Insights_module_2("insights_module_2", uploadedData)
#   Data_Insights_module_3("insights_module_3", uploadedData)
#   Data_Insights_module_4("insights_module_4", uploadedData)
#   Data_Insights_module_5("insights_module_5", uploadedData)
#   #}
#  #
#   #shinyauthr::loginUI(id = "login")
# }
