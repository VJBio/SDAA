# 
# LoGINMODULE_UI <- function(id) {
#   ns <- NS(id)
#  bs4DashPage(
#   # Linking the CSS file
#   #shinyauthr::loginUI("login"),
#   preloader = list(html = tagList(spin_flower(), "Loading ..."), color = "#000485"),
#   
#   
#   dark = FALSE,  # Keep dark theme option
#   fullscreen = TRUE,  # Enable fullscreen mode
#   # Add loginUI here if it isn't present
#   #shinyauthr::loginUI("login"),  # Make sure this is part of the UI
#   
#   
#   header = bs4DashNavbar(
#     title = tags$div(
#       style = "display: flex; justify-content: center; align-items: center; height: 100px; padding: 10px;",
#       tags$img(
#         src = "https://www.iprcenter.gov/image-repository/pfizer_-2021-svg.png/@@images/image.png",
#         width = "160px",
#         style = "margin: 20px auto; padding: 5px; border: 2px solid #ffffff; box-shadow: 0px 4px 10px rgba(255, 255, 255, 0.4); filter: brightness(1.5); border-radius: 5px;"
#       )
#     ),
#     fixed = TRUE,
#     # Add loginUI here if it isn't present
#     #shinyauthr::loginUI("login"),  # Make sure this is part of the UI
#     rightUi = userOutput("user"),
#     shinyauthr::loginUI("login"),
#     #tags$li(class = "dropdown", style = "padding: 8px;", shinyauthr::loginUI("login"))
#     
#     shinyauthr::logoutUI(id = "logout")  # Use correct ID for logout UI
#     
#   ),
#   
#   # Sidebar
#   sidebar = bs4DashSidebar(
#     disable = FALSE,
#     fixed = TRUE,
#     skin = "light",
#     #shinyauthr::loginUI(id = "login", cookie_expiry = cookie_expiry),
#     
#     # custom CSS to style the sidebar
#     tags$style(
#       HTML("
#         .main-sidebar {
#           background-color: #000485 !important;
#         }
#         .sidebar, .sidebar a {
#           color: #ffffff !important;
#         }
#       ")
#     ),
#     bs4SidebarMenu(
#       bs4SidebarMenuItem(
#         text = "LOGINMODULE",
#         tabName = "loginModule",
#         icon = icon("clipboard")
#       ),
#       
#       bs4SidebarMenuItem(
#         text = "PREQUISITES",
#         tabName = "Tab1",
#         icon = icon("clipboard")
#       ),
#       bs4SidebarMenuItem(
#         text = "SDAA DASHBOARD",
#         tabName = "Tab2",
#         icon = icon("chart-bar")
#       ),
#       bs4SidebarMenuItem(
#         text = "SD LISTING",
#         tabName = "Tab3",
#         icon = icon("list")
#       ),
#       bs4SidebarMenuItem(
#         text = "VISUAL & DATA TABLE",
#         tabName = "Tab4",
#         icon = icon("table")
#       ),
#       bs4SidebarMenuItem(
#         text = "HELP",
#         tabName = "Tab5",
#         icon = icon("info-circle")
#       ),
#       bs4SidebarMenuItem(
#         text = "VERSION HISTORY",
#         tabName = "Tab6",
#         icon = icon("history")
#       ),
#       shinyauthr::loginUI("login")
#       
#       #rightUi = userOutput("user"),
#       
#       #shinyauthr::logoutUI(id = "logout")  # Use correct ID for logout UI
#       
#     )
#   ),
#   
#   # Body
#   body = bs4DashBody(
#     # custom CSS file
#     #rightUi = userOutput("user"),
#     #shinyauthr::loginUI(id = "login"),
#     #shinyjs::useShinyjs(),
#     
#     #shinyauthr::loginUI(id = "login", cookie_expiry = cookie_expiry),
#     
#     
#     
#     includeCSS("www/pdash.css"),
#     bs4TabItems(
#       bs4TabItem(
#         tabName = "loginModule",
#         LoGINMODULE_UI("LOGINMODULE")
#       ),
#       
#       bs4TabItem(
#         tabName = "Tab1",
#         PREQUISITES_UI("PREQUISITES")
#       ),
#       bs4TabItem(
#         tabName = "Tab2",
#         SDAA_DASHBOARD_UI("SDAA_DASHBOARD")
#       ),
#       bs4TabItem(
#         tabName = "Tab3",
#         Data_Insights_UI_2("insights_module_2")
#       ),
#       bs4TabItem(
#         tabName = "Tab4",
#         Data_Insights_UI_3("insights_module_3")
#       ),
#       bs4TabItem(
#         tabName = "Tab5",
#         Data_Insights_UI_4("insights_module_4")
#       ),
#       bs4TabItem(
#         tabName = "Tab6",
#         Data_Insights_UI_5("insights_module_5")
#       )
#     )
#   ),
#   
#   # Footer
#   footer = bs4DashFooter(
#     left = tagList(a(href = "https://www.iprcenter.gov/image-repository/pfizer_-2021-svg.png/@@images/image.png", "by Pfizer")),
#     right = a("@VJ")
#   )
#   
#   
# )
# 
# }
# 
# MODULE_Server <- function(id) {
#   
#   moduleServer <- function(input, output, session) {
#   
#   session$onSessionEnded(function() {
#     stopApp()
#   })
#   
#   # User Profile rendering
#   output$user <- renderUser({
#     #    #req(credentials()$user_auth)
#     dashboardUser(
#       name = " ",
#       image = "https://www.iprcenter.gov/image-repository/pfizer_-2021-svg.png/@@images/image.png",
#       title = NULL,
#       subtitle = "Author - Sushmitha",
#       footer = NULL
#     )
#   })
#   # 
#   # only when credentials()$user_auth is TRUE, render your desired sidebar menu
#   #if(credentials()$user_auth) {
#   # Calling the Data Load Module
#   
#   LOGIN <- LoGINMODULE_Server("LOGINMODULE")
#   print (LOGIN)
#   #if(credentials()$user_auth) {
#   uploadedData <- PREQUISITES_server("PREQUISITES")  
#   #print("Reactive Data")
#   #+200.  print(head(uploadedData, 10))
#   
#   # Calling the Data Insights Modules
#   SDAA_DASHBOARD_module("SDAA_DASHBOARD", uploadedData)
#   Data_Insights_module_2("insights_module_2", uploadedData)
#   Data_Insights_module_3("insights_module_3", uploadedData)
#   Data_Insights_module_4("insights_module_4", uploadedData)
#   Data_Insights_module_5("insights_module_5", uploadedData)
#   #}
#   #
#   #shinyauthr::loginUI(id = "login")
# }
# 
#   }
#   
#   # Module to call server function
#   LoGINMODULE <- function(id) {
#     moduleServer(id, function(input, output, session) {
#       LoGINMODULE_Server(id)
#     })
#   }