library(shinyjs)
ui <- dashboardPage(
 # shinyjs::useShinyjs(),
  # Dashboard header with logout UI
  dashboardHeader(
    title = "SDAA",
    tags$li(class = "dropdown", style = "padding: 8px;", shinyauthr::logoutUI(id="logout"))
    #div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
    
  ),
  
  # Sidebar setup
  dashboardSidebar(
    collapsed = TRUE, 
    sidebarMenuOutput("sidebar")
   
  ),
  
  # Body setup, initially visible
  dashboardBody(
   # div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
    shinyjs::useShinyjs(),
    #includeCSS("www/pdash.css"),
    
    shinyauthr::loginUI(id = "login", cookie_expiry = 0.1),
    #appResetButton('appResetId'),
    
    #includeCSS("www/pdash.css"),
    mainPanel(
      id = "mainPanel",
      width=12,
      #align = "right",
      #includeCSS("www/pdash.css"),
      tabItems(
        tabItem(tabName = "abnorm", autouploader_UI("AbnormalStatus")),
        tabItem(tabName = "Tab1", PREQUISITES_UI("PREQUISITES")),
        tabItem(tabName = "TabTH", EditTable_UI("Threshold")),
        tabItem(tabName = "Tab2", SDAA_DASHBOARD_UI("SDAA_DASHBOARD")),
        tabItem(tabName = "Tab3", Data_Insights_UI_2("insights_module_2")),
        tabItem(tabName = "Tab4", Data_Insights_UI_3("insights_module_3")),
        tabItem(tabName = "Tab5", Data_Insights_UI_4("insights_module_4")),
        tabItem(tabName = "Tab6", Data_Insights_UI_5("insights_module_5"))
      )
    )
   
  )
)
