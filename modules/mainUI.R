library(shiny)
library(dplyr)
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
}

# This function must return a data.frame with columns user and sessionid  Other columns are also okay
# and will be made available to the app after log in as columns in credentials()$user_auth

get_sessionids_from_db <- function(conn = db, expiry = cookie_expiry) {
  dbReadTable(conn, "sessionids") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}

# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)



LoGINMODULE_UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    
    
  # logout button
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  shinyauthr::logoutUI(id = "logout"),
  tags$li(class = "dropdown", style = "padding: 8px;", shinyauthr::loginUI("login")),
  
  # login section
  shinyauthr::loginUI(id = "login"),
  
  
  uiOutput("sidebarpanel"),
  # Plot to show user info after login
  plotOutput("distPlot")
  
)
}


LoGINMODULE_Server <- function(id) {

  moduleServer<- function(input, output, session) {
  
    # Call the logout module
    logout_init <- callModule(shinyauthr::logoutServer, "logout", 
                              active = reactive(credentials()$user_auth))  # Handle logout logic
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
      log_out = reactive(logout_init())
    )
    output$sidebarpanel <- renderUI({
      
      # Show only when authenticated
     # req(credentials()$user_auth)
      
      tagList(
        # Sidebar with a slider input
        column(width = 4,
               sliderInput("obs",
                           "Number of observations:",
                           min = 0,
                           max = 1000,
                           value = 500)
        ),
        
        column(width = 4,
               p(paste("You have", credentials()$info[["permissions"]],"permission"))
        )
      )
      
    })
    # Plot
    output$distPlot <- renderPlot({
      
      # Show plot only when authenticated
     # req(credentials()$user_auth)
      
      if(!is.null(input$obs)) {
        hist(rnorm(input$obs)) 
      }
      
    })
    #return(credentials()$user_auth)
  
} 
}
  ##########################################################################################
# Module to call server function
LoGINMODULE <- function(id) {
  moduleServer(id, function(input, output, session) {
    LoGINMODULE_Server(id)
  })
}
