library(shiny)
library(shinydashboard)
library(shinyauthr)
library(dplyr)
library(shinyjs)
library(lubridate)
library(DBI)
library(RSQLite)

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
  # loginaudits<- tibble(user = user,
  #                      sessionid = session$token,
  #                      time = as.character(now()),
  #                      action = "login Sucess"  )
  # dbWriteTable( audit, "audits" , loginaudits , append =TRUE)
  dbDisconnect(audit)


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
#   user = c("vineet", "SDAA"),
#   password = sapply(c("VJ@123", "SDAA"), sodium::password_store),
#   permissions = c("admin", "standard"),
#   name = c("User One", "User Two"),
#   AbnormalStatus =c(1,1) ,
#   uploaddata =c(1,1) ,Threshold =c(1,1),Admin=c(1,1)
# )
# dbWriteTable( db, "user" , user_base )

udb <- dbConnect(SQLite(), "users")
user_base <- dbReadTable(udb , "user")
dbDisconnect(udb)
#audit <- dbConnect(SQLite(), "audit")
#dbCreateTable(audit, "audits", c(user = "TEXT", sessionid = "TEXT",
#                                     time = "TEXT", action="TEXT"))
#dbReadTable(audit ,"audits")

#user_base <- dplyr::tibble(
#    user = c("user", "user2"),
#    password = c("pass", "pass2"),
#    permissions = c("admin", "standard"),
#    name = c("User One", "User Two")
#  )
  


server <- function(input, output, session) {
  # session$onSessionEnded(function() {
  #   stopApp()
  # })
  #print(input)
  #print(output)
  # logout status managed by shinyauthr module and stored here
  logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
  #print(logout_init)
  #user_base<-get_user_base()
   
    #username<- reactive({    session$user  }) 
    #print(username)
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    reload_on_logout = TRUE,
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
      ##############Show as per user authentication######
    	shinyjs::show("mainPanel")
      #credentials()$info$user<-session$user
    	output$user <- renderText({session$user })
    	#print(credentials()$info)
    	#print(Sys.getenv("USER"))
    	#print(session$user)
    	user_base2 <- tibble(user = c(session$user) ,
    	 password = sapply(c("SDAA"), sodium::password_store) ,
    	 permissions = "admin",
    	 name = session$user, AbnormalStatus =0 , 
    	 uploaddata =0 ,Threshold =0,Admin=0  )
    	
    	udb <- dbConnect(SQLite(), "users")
    	
    	user_base <- dbReadTable(udb , "user")
    	if(sum(user_base$user %in% user_base2$user)==0)
    	{
    	  dbWriteTable( udb, "user" , 	user_base2 , append  =TRUE)
    	} 
    	dbDisconnect(udb)
    	udb <- dbConnect(SQLite(), "users")
    	print(dbReadTable(udb , "user"))
    	user_base <- dbReadTable(udb , "user")
    	user_base <- subset(user_base, user_base$user == session$user, )[1,]
    	#user_base <- subset(user_base, user_base$user == Sys.getenv("USER"), )[1,]
    	#print(user_base)
    	dbDisconnect(udb)
    	
    	if( user_base$AbnormalStatus == 0)
    	{

    		shinyjs::hide("shiny-tab-abnorm")
    	  
    	}else{
    	  from = c(  "Auto Scan run sucessfully on ","Files with Abormalties" )
    	  message =c( as.character(abstatus("date")) , as.character(abstatus("count")))
    	  icons<-c("truck","exclamation-triangle")
    	  status <-c("success","warning")
    	  messageData =  data.frame(from, message , icons,status)
    	  
    	  output$messageMenu <- renderMenu({
    	    msgs <- apply(messageData, 1, function(row) {
    	      messageItem(from = row[["from"]], message = row[["message"]] ,
    	                  icon=icon(row[["icons"]])
    	      )
    	    })
    	    
    	    dropdownMenu(type = "messages", .list = msgs)
    	  })
    	}


    	if( user_base$uploaddata  == 0)
    	{
    		shinyjs::hide("shiny-tab-Tab1")
    		shinyjs::hide("shiny-tab-Tab2")
    		shinyjs::hide("shiny-tab-Tab3")
    		shinyjs::hide("shiny-tab-Tab4")

    	}
    	 if( user_base$Threshold   == 0)
    	 {

    	 	shinyjs::hide("shiny-tab-TabTH")
    	 }

    	if( user_base$Admin  == 0)
    	{
    		shinyjs::hide("shiny-tab-Admin")

    	}

      shinyjs::show(notificationItem)

       audit <- dbConnect(SQLite(), "audit")
      #
       loginaudits<- tibble(user = session$user,
                            sessionid = session$token,
                          time = as.character(now()),
                            action = "login Sucess"  )
       dbWriteTable( audit, "audits" , loginaudits , append =TRUE)
       dbDisconnect(audit)
       

       # output$user<- renderUser({
       #   dashboardUser(
       #     name = credentials()$info$user,
       #     image = "https://www.iprcenter.gov/image-repository/pfizer_-2021-svg.png/@@images/image.png",
       #     #title = reactive(Sys.time()),
       #     subtitle = "Author - Vineet Jha",
       #     footer = NULL
       #   )
       # })


    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      shinyjs::hide("mainPanel")
      shinyjs::hide("dropdownMenu")

      #session$reload()
      #uploadedData = reactive(uploadedData)
      audit <- dbConnect(SQLite(), "audit")
      #print(session)
      loginaudits<- tibble(user =    session$user,
                           sessionid =  session$token,
                           time = as.character(now()),
                           action = "Logout Sucessfull"
      )
      dbWriteTable( audit, "audits" , loginaudits , append =TRUE)
      dbDisconnect(audit)
     reset(id="uploadedData" )
     #refresh()
    }
  })

  #AbnormalStatus-show

  observeEvent(input$AbnormalStatus$scan, {
    print("<----here---->")
    from = c(  "Auto Scan run sucessfully on ","Files with Abormalties" )
    message =c( as.character(abstatus("date")) , as.character(abstatus("count")))
    icons<-c("truck" , "exclamation-triangle")
    status <-c("success","warning")
    messageData =  data.frame(from , message,icons,status)

    output$messageMenu <- renderMenu({
      msgs <- apply(messageData, 1, function(row) {
        messageItem(from = row[["from"]], message = row[["message"]] ,
        )
      })

      dropdownMenu(type = "messages", .list = msgs)
    })

  })


  observe({
  	if(credentials()$user_auth){
  output$Abnormalsidebar <- renderMenu({
  	req(credentials()$user_auth)
    udb <- dbConnect(SQLite(), "users")
    user_base <- dbReadTable(udb , "user")
    user_base <- subset(user_base, user_base$user == session$user, )[1,]
    #user_base <- subset(user_base, user_base$user == Sys.getenv("USER"), )[1,]
    #print(user_base)
    dbDisconnect(udb)
  	if( user_base$AbnormalStatus == 1)
  	{
  	sidebarMenu(
  		id = "tabs",
  		menuItem("AbnormalStatus", tabName = "abnorm" ,icon = icon("circle-info") )

  	)
  	}
  })
  output$Thresholdsidebar <- renderMenu({
  	req(credentials()$user_auth)
    udb <- dbConnect(SQLite(), "users")
    user_base <- dbReadTable(udb , "user")
    user_base <- subset(user_base, user_base$user == session$user, )[1,]
    #user_base <- subset(user_base, user_base$user == Sys.getenv("USER"), )[1,]
    #print(user_base)
    dbDisconnect(udb)
  	if( user_base$Threshold  == 1)
  	{
  		sidebarMenu(
  			id = "tabs",
  			menuItem("Threshold", tabName = "TabTH" , icon = icon("edit"))

  		)
  	}
  })
  output$uploaddatasidebar <- renderMenu({
  	req(credentials()$user_auth)
    udb <- dbConnect(SQLite(), "users")
    user_base <- dbReadTable(udb , "user")
    user_base <- subset(user_base, user_base$user == session$user, )[1,]
    #user_base <- subset(user_base, user_base$user == Sys.getenv("USER"), )[1,]
    #print(user_base)
    dbDisconnect(udb)
  	if( user_base$uploaddata   == 1)
  	{
  		sidebarMenu(
  			id = "tabs",
  			menuItem("PREQUISITES", tabName = "Tab1" ,icon = icon("clipboard") ),
  			menuItem("SDAA DASHBOARD", tabName = "Tab2" , icon = icon("chart-bar")),
  			menuItem("SD LISTING", tabName = "Tab3" ,icon = icon("list")),
  			menuItem("VISUAL & DATA TABLE", tabName = "Tab4" ,icon = icon("table"))

  		)
  	}
  })
  output$Adminsidebar <- renderMenu({
  	req(credentials()$user_auth)
    udb <- dbConnect(SQLite(), "users")
    user_base <- dbReadTable(udb , "user")
    user_base <- subset(user_base, user_base$user == session$user, )[1,]
    #user_base <- subset(user_base, user_base$user == Sys.getenv("USER"), )[1,]
    #print(user_base)
    dbDisconnect(udb)
  	if( user_base$Admin   == 1)
  	{
  		sidebarMenu(
  			id = "tabs",
  			menuItem("Admin", tabName = "Admin" ,icon = icon("lock"))

  		)
  	}
  })


  	output$sidebar <- renderMenu({
  		req(credentials()$user_auth)
    sidebarMenu(
      id = "tabs",
      #menuItem("AbnormalStatus", tabName = "abnorm" ,icon = icon("circle-info") ),
      #menuItem("Threshold", tabName = "TabTH" , icon = icon("edit")),
      #menuItem("PREQUISITES", tabName = "Tab1" ,icon = icon("clipboard") ),
      #menuItem("SDAA DASHBOARD", tabName = "Tab2" , icon = icon("chart-bar")),
      #menuItem("SD LISTING", tabName = "Tab3" ,icon = icon("list")),
      #menuItem("VISUAL & DATA TABLE", tabName = "Tab4" ,icon = icon("table")),
      #menuItem("Admin", tabName = "Admin" ,icon = icon("lock")),
      menuItem("HELP", tabName = "Tab5", icon = icon("info-circle")),
      menuItem("VERSION HISTORY", tabName = "Tab6", icon = icon("history"))
    )
  })

  	}
  })

  observe({
    if(credentials()$user_auth){
      #urlSDAA <<- paste0("https://rsc.pfizer.com/SDAA")
      #uploadedData()<-NULL
      autouploader_module("AbnormalStatus",uploadedData,credentials)
      uploadedData <-PREQUISITES_server("PREQUISITES" , credentials)
      #print(uploadedData)
      ## Calling the Data Insights Module and passing the reactive data correctly
     	EditTable_module("Threshold",uploadedData,credentials)
      SDAA_DASHBOARD_module("SDAA_DASHBOARD", uploadedData)
      Data_Insights_module_2("insights_module_2", uploadedData)
      Data_Insights_module_3("insights_module_3", uploadedData)
      Admin_module("Admin",credentials)
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
      uploadedData<- NULL
      #session$reload()
      #rm(PREQUISITES_server)
    }
  })



  # output$user<- renderUser({
  #   req(credentials()$user_auth)
  #   dashboardUser(
  #     name = " ",
  #     image = "https://www.iprcenter.gov/image-repository/pfizer_-2021-svg.png/@@images/image.png",
  #     title = NULL,
  #     subtitle = "Author - Sushmitha",
  #     footer = NULL
  #   )
  # })





}

