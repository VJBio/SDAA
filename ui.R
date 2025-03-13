library(shinydashboard)


ui <- dashboardPage(

  dashboardHeader(

   title = "SDAA",

    # tags$li(
    #   class = "dropdown",
    #   h4("Sensitive Data Abnormality Analyzer (SDAA)       "),
    #   style = "padding: 5px;"
    #    ),
   dropdownMenuOutput("messageMenu"),
   #rightUi = userOutput("user"),
   tags$li(class = "dropdown"  ,style = "
      display: block;
      font-size: 1.5em;
      margin-block-start: 0.5em;
      color: white;",
           align = "right" , textOutput("user" )),
    tags$li(class = "dropdown",style="color: red;", style = "padding: 8px;", shinyauthr::logoutUI(id="logout") )


  )|>
    tagAppendChild(
      div(
        "Sensitive Data Abnormality Analyzer ",
        style =
      "display; block;
      font-size: 1.5em;
      margin-block-start: 0.5em;
     
      color: white;
      margin-left: 5%",
        align = "left"
      ),
      .cssSelector = "nav"
    ),

  # Sidebar setup
  dashboardSidebar(
    collapsed = TRUE,
    #h3("SDAA"),
    tags$div(
      style = "display: flex; justify-content: center; align-items: center; height: 100px; padding: 10px;",
      tags$img(
        src = "https://www.iprcenter.gov/image-repository/pfizer_-2021-svg.png/@@images/image.png",
        width = "160px",
        style = "margin: 20px auto; padding: 5px; border: 2px solid #ffffff; box-shadow: 0px 4px 10px rgba(255, 255, 255, 0.4); filter: brightness(1.5); border-radius: 5px;"
      )
    ),
    sidebarMenuOutput("Abnormalsidebar"),
    sidebarMenuOutput("Thresholdsidebar"),
    sidebarMenuOutput("uploaddatasidebar"),
    sidebarMenuOutput("Adminsidebar"),
    sidebarMenuOutput("sidebar")


  ),

  # Body setup, initially visible
  dashboardBody(
   # div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
    shinyjs::useShinyjs(),
    #height="100%",
    #includeCSS("www/pdash.css"),

    shinyauthr::loginUI(id = "login", cookie_expiry = 0.1 , 
                        additional_ui =  tags$code(paste("please use following details to login
                                                   Username: SDAA Password: SDAA
                                                   If you are loging for fist time please connect admin (vineet jha) for access"
                                                   ))),
    #appResetButton('appResetId'),

   # includeCSS("www/pdash.css"),
    #mainPanel(
     fluidPage(
      id = "mainPanel",
      width=12,
      # wellPanel(
      # 	id="abc",
      # 	# tabItems(
      # 	# tabItem(tabName = "abnorm", autouploader_UI("AbnormalStatus"))
      # 	# )
      # ),
      tabItems(
      	tabItem(tabName = "abnorm", autouploader_UI("AbnormalStatus")),
        tabItem(tabName = "Tab1", PREQUISITES_UI("PREQUISITES")),
        tabItem(tabName = "TabTH", EditTable_UI("Threshold")),
        tabItem(tabName = "Tab2", SDAA_DASHBOARD_UI("SDAA_DASHBOARD")),
        tabItem(tabName = "Tab3", Data_Insights_UI_2("insights_module_2")),
        tabItem(tabName = "Tab4", Data_Insights_UI_3("insights_module_3")),
        tabItem(tabName = "Admin", Admin_UI("Admin")),
	    tabItem(tabName = "Tab5", Data_Insights_UI_4("insights_module_4")),
        tabItem(tabName = "Tab6", Data_Insights_UI_5("insights_module_5"))
   	)
  )
)

)
