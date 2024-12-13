library(shiny)
library(shinydashboard)
library(shinyauthr)
library(dplyr)
library(shinyjs)
library(lubridate)
library(DBI)
library(RSQLite)

# connect to, or setup and connect to local SQLite db
if (file.exists("my_db_file")) {
  db <- dbConnect(SQLite(), "my_db_file")
} else {
  db <- dbConnect(SQLite(), "my_db_file")
  dbCreateTable(db, "sessionids", c(user = "TEXT", sessionid = "TEXT", login_time = "TEXT"))
}

# a user who has not visited the app for this many days
# will be asked to login with user name and password again
cookie_expiry <- 1 # Days until session expires

# This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.  This function saves to your database.

add_sessionid_to_db <- function(user, sessionid, conn = db) {
  tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessionids", ., append = TRUE)
  audit <- dbConnect(SQLite(), "audit")
  loginaudits<- tibble(user = user,
                       sessionid = sessionid, 
                       time = as.character(now()),
                       action = "login Sucess"  )
  dbWriteTable( audit, "audits" , loginaudits , append =TRUE)
}

# This function must return a data.frame with columns user and sessionid  Other columns are also okay
# and will be made available to the app after log in as columns in credentials()$user_auth

get_sessionids_from_db <- function(conn = db, expiry = cookie_expiry) {
  dbReadTable(conn, "sessionids") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}


# sample logins dataframe with passwords hashed by sodium package
# user_base <- tibble(
#   user = c("vineet", "Prasad"),
#   password = sapply(c("VJ@123", "Pra@123"), sodium::password_store), 
#   permissions = c("admin", "standard"),
#   name = c("User One", "User Two")
# )
#dbWriteTable( db, "user" , user_base )

udb <- dbConnect(SQLite(), "users")
user_base <- dbReadTable(udb , "user")
dbDisconnect(udb)
#audit <- dbConnect(SQLite(), "audit")
#dbCreateTable(audit, "audits", c(user = "TEXT", sessionid = "TEXT", 
#                                     time = "TEXT", action="TEXT"))
#dbReadTable(audit ,"audits")

ui <- dashboardPage(
  
  # put the shinyauthr logout ui module in here
  dashboardHeader(
    title = "SDAA",
    #rightUi = userOutput("user"),
    tags$li(class = "dropdown", style = "padding: 8px;", shinyauthr::logoutUI("logout"))
    #title = "SDAA",
    #tags$li(class = "dropdown", style = "padding: 8px;", shinyauthr::logoutUI("logout"))
  ),
 
  # setup a sidebar menu to be rendered server-side
  
  
 
  dashboardSidebar(
    collapsed = TRUE, sidebarMenuOutput("sidebar")
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    
    # put the shinyauthr login ui module here
    #shinyauthr::loginUI("login"),
    shinyauthr::loginUI(id = "login", cookie_expiry = cookie_expiry),
    #includeCSS("www/pdash.css"),
    # tabItems(
    #   tabItem("Tab1", uiOutput("tab1_ui")),
    #   tabItem("Tab2", uiOutput("tab2_ui"))
    # )
    
    tabItems(
      tabItem(
        tabName = "Tab1",
        PREQUISITES_UI("PREQUISITES")
      ),
      tabItem(
        tabName = "TabTH",
        EditTable_UI("Threshold")
      ),
      tabItem(
        tabName = "Tab2",
        SDAA_DASHBOARD_UI("SDAA_DASHBOARD")
      ),
      tabItem(
        tabName = "Tab3",
        Data_Insights_UI_2("insights_module_2")
      ),
      tabItem(
        tabName = "Tab4",
        Data_Insights_UI_3("insights_module_3")
      ),
      tabItem(
        tabName = "Tab5",
        Data_Insights_UI_4("insights_module_4")
      ),
      tabItem(
        tabName = "Tab6",
        Data_Insights_UI_5("insights_module_5")
      )
    )
    
   # mainPanel(fluidRow(htmlOutput("frame")
   
  )                           
  
)


server <- function(input, output, session) {
  
  # login status and info will be managed by shinyauthr module and stores here
  # credentials <- callModule(shinyauthr::login, "login",
  #                           data = user_base,
  #                           user_col = user,
  #                           pwd_col = password,
  #                           sodium_hashed = TRUE,
  #                           log_out = reactive(logout_init()))

  # logout status managed by shinyauthr module and stored here
  logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
  #print(logout_init)
  # # Call the logout module
  # logout_init <- callModule(shinyauthr::logoutServer, "logout", 
  #                           active = reactive(credentials()$user_auth))  # Handle logout logic
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    cookie_logins = TRUE,
    sessionid_col = sessionid,
    cookie_getter = get_sessionids_from_db,
    cookie_setter = add_sessionid_to_db,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
 # print(credentials()$user_auth)
  # login audit

  # #dbReadTable(audit ,"audits")
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  output$sidebar <- renderMenu({
    req(credentials()$user_auth)
    sidebarMenu(
      id = "tabs",
      menuItem("PREQUISITES", tabName = "Tab1" ,icon = icon("clipboard") ),
      menuItem("Threshold", tabName = "TabTH" , icon = icon("edit")),
      menuItem("SDAA DASHBOARD", tabName = "Tab2" , icon = icon("chart-bar")),
      menuItem("SD LISTING", tabName = "Tab3" ,icon = icon("list")),
      menuItem("VISUAL & DATA TABLE", tabName = "Tab4" ,icon = icon("table")),
      menuItem("HELP", tabName = "Tab5", icon = icon("info-circle")),
      menuItem("VERSION HISTORY", tabName = "Tab6", icon = icon("history"))
    )
  })
  
  # output$tab2_ui <- renderUI({
  #   req(credentials()$user_auth)
  #   uploadedData <- PREQUISITES_server("PREQUISITES") 
  # })
  
  
  observe({ 
    if(credentials()$user_auth){
    #urlSDAA <<- paste0("https://rsc.pfizer.com/SDAA")
    uploadedData <- PREQUISITES_server("PREQUISITES")  
    
    # print("Reactive Data")
    # print(head(uploadedData, 10))
    # 
    # # Calling the Data Insights Module and passing the reactive data correctly
     EditTable_module("Threshold",uploadedData)
     SDAA_DASHBOARD_module("SDAA_DASHBOARD", uploadedData)
     Data_Insights_module_2("insights_module_2", uploadedData)
     Data_Insights_module_3("insights_module_3", uploadedData)
     Data_Insights_module_4("insights_module_4", uploadedData)
     Data_Insights_module_5("insights_module_5", uploadedData)
    }
    else{
      audit <- dbConnect(SQLite(), "audit")
      #print(session)
      loginaudits<- tibble(user =    "",
                           sessionid =  session$token, 
                           time = as.character(now()),
                           action = "Login fail"
      )
      dbWriteTable( audit, "audits" , loginaudits , append =TRUE)
      dbDisconnect(audit)
    }
  })
  output$frame <- renderUI({
    req(credentials()$user_auth)
    
    #my_test <- tags$iframe(src=urlSDAA,  style='width:100vw;height:100vh;')
    #print(my_test)
    #my_test
  })
    
  
}

shiny::shinyApp(ui, server)