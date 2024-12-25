library(shinyjs)

abstatus <-function(arg)
{
  #print(arg)
  abnormalcon <- dbConnect(SQLite(), "AbnormalStatus")
   abdata<- dbReadTable(abnormalcon  ,"AbnormalStatus")
   dbDisconnect(abnormalcon)
   if(arg == "date")
   {
     return(unique(as.Date(abdata$time)))
   }
   if(arg == "count")
   {
     return(sum(abdata$abnormal>0))
   }
}


ui <- dashboardPage(
 # shinyjs::useShinyjs(),
  # Dashboard header with logout UI
  dashboardHeader(
    title = "SDAA",
    #h3("SDAA"),
    tags$li(
      class = "dropdown",
      h4("Sensitive Data Abnormality Analyzer (SDAA)"),
      style = "padding: 5px;"
       ),
    
    dropdownMenu(type = "notifications",
                 
                 notificationItem(
                   text = paste("Auto Scan run sucessfully on ", abstatus("date") ),
                   icon("truck"),
                   status = "success"
                 ),
                 notificationItem(
                   text = paste("Files with Abormalties",  abstatus("count") ) ,
                   icon = icon("exclamation-triangle"),
                   status = "warning"
                 )
    ),
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
    tags$head(tags$style(HTML('
         height: 100%;
      '))),
    #includeCSS("www/pdash.css"),
    
    shinyauthr::loginUI(id = "login", cookie_expiry = 0.1),
    #appResetButton('appResetId'),
    
    #includeCSS("www/pdash.css"),
    box(
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
