ui <- bs4DashPage(
  # Link the CSS file
  preloader = list(html = tagList(spin_flower(), "Loading ..."), color = "#000485"),
  
  dark = FALSE,
  help = NULL,
  header = dashboardHeader(
    title = img(src = "https://www.iprcenter.gov/image-repository/pfizer_-2021-svg.png/@@images/image.png", width = "145px", style = "margin-top: 1px;"),  # Add your custom logo,  # Set title to NULL to remove the default title
    
    fixed = TRUE,
    
    navbarMenu(id = "navmenu", 
               navbarTab(tabName = "Tab1", text = "PREQUISITES"),
               navbarTab(tabName = "Tab2", text = "SDAA DASHBOARD"),
               navbarTab(tabName = "Tab3", text = "SD LISTING"),
               navbarTab(tabName = "Tab4", text = "VISUALS & DATA TABLEs"),
               navbarTab(tabName = "Tab5", text = "HELP"),
               navbarTab(tabName = "Tab6", text = "VERSION HISTORY")
               
    ),
    
    rightUi = userOutput("user")
  ),
  
  sidebar = dashboardSidebar(disable = TRUE),
  
  body = dashboardBody(
    
    tabItems(
      tabItem(tabName = "Tab1", PREQUISITES_UI("PREQUISITES")),
      tabItem(tabName = "Tab2", SDAA_DASHBOARD_UI("SDAA_DASHBOARD")),
      tabItem(tabName = "Tab3", Data_Insights_UI_2("insights_module_2")),
      tabItem(tabName = "Tab4", Data_Insights_UI_3("insights_module_3")),
      tabItem(tabName = "Tab5", Data_Insights_UI_4("insights_module_4")),
      tabItem(tabName = "Tab6", Data_Insights_UI_5("insights_module_5"))
    ),
    
    footer = dashboardFooter(
      fixed = FALSE,
      left = tagList(a(href="https://www.iprcenter.gov/image-repository/pfizer_-2021-svg.png/@@images/image.png", "by Pfizer")
      ),
      
      right = a("@Sushmitha")
    ), 
    
    fullscreen = TRUE,
    
    htmltools::includeCSS(path = "www/pdash.css")
  )
  
) 