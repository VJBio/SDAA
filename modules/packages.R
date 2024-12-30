# renv::restore()
# renv::install()
# renv::snapshot()
# renv::activate()
# renv::load()

library(shiny)
library(shinyauthr)
#library(bs4Dash)
library(shinydashboard)
library(shinyFiles)
library(stringr)
library(shinyalert)
library(plotly)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(dplyr)
library(data.table)
library(readxl)
library(ggplot2)
library(purrr)
library("highcharter")
library(lubridate)
library(waiter)
library(shinyWidgets)
library(fontawesome)
library(echarts4r)
library(viridis)
library(DBI)
library(RSQLite)
library(tidyverse)
library(rhandsontable)
library(shinyauthr)
#############################################################


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
#############################################################################


#library(shinyjs)

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


