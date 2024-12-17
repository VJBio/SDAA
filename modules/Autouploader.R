library(tidyverse)
library(shiny)
library(bs4Dash)
library(DT)  
library(waiter)  
library(DBI)
library(RSQLite)
library(rhandsontable)
library(shiny)


if (file.exists("Threshold")) {
  th <- dbConnect(SQLite(), "Threshold")
} else {
  th <- dbConnect(SQLite(), "Threshold")
  
}
dbDisconnect(th)

autouploader_UI <- function(id) {
  ns <- NS(id)
  fluidPage(
  
    
    wellPanel(
      #h3("Update threshold"),
      #br(),
      width=8,
     
      
      actionButton(ns("show"), label = "Show Abnormalities", style = "fill"),
      actionButton(ns("scan"), label = "scan Abnormalities", style = "fill")
    ),
    
    box(
                title = "SDAA ABnormalities scan status",
                closable = FALSE,
                width = 12,
                status = "warning",
                solidHeader = FALSE,
                collapsible = TRUE,
                fluidRow(
                  column(12,
                         align = "center",
                         DT::dataTableOutput(ns("stats")) %>% withSpinner(color = "#0095FF")
                  )
                )
              )
  )
}

autouploader_server <- function(id, uploadedData, credentials) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #print("credentials--EditTable----->")
    #print(credentials()$info)
    #print("credentials---EditTable---->")
    values <- reactiveValues()



if (file.exists("AbnormalStatus")) {
  abnorm <- dbConnect(SQLite(), "AbnormalStatus")
} else {
  abnorm <- dbConnect(SQLite(), "AbnormalStatus")
  
}
dbDisconnect(abnorm)

observeEvent(input$scan, {
  
list_of_files <- list.files(path = "/home/jhav11/ARD",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)


validcol<-c("STUDY" ,"PKTERM" , "VISIT", "PERIOD", "PKCNC" )

for(file in list_of_files)
{
  df<-read.csv(file)
  if(length(intersect(validcol , colnames(df))) ==5)
  {

    data <- distinct( df[c("STUDY" ,"TREATXT" , "VISIT", "PERIOD" )])
    colnames(data) <-c("STUDYID" ,"TREATXT" , "VISIT", "PCTPT" )
    data$Lower_Limit <-""
    data$Upper_Limit <-""
   
    study <- distinct(data[c("STUDYID")])
    #PKTERM <- distinct(data[c("PKTERM")])
    #study <-paste(as.character(study), as.character(PKTERM))
    th <- dbConnect(SQLite(), "Threshold")
   
    if(study %in% dbListTables(th) )
    {
      query= paste0("SELECT * FROM " , "'",study,"'")
      res <- dbSendQuery(th,query )
      data <- dbFetch(res)  
      print("from DB-------->")
     # print(data)
      merge.df <- merge(df,data, by.x=c("STUDY" ,"TREATXT" , "VISIT", "PERIOD" ) , 
           by.y=c("STUDYID" ,"TREATXT" , "VISIT", "PCTPT" ))
      #print(merge.df)
      data.num <- as.numeric(merge.df$PKCNC)
      data.num[is.na(data.num)] <- 0
      merge.df$PKCNC <- data.num
      abnormal=0
      for(i in 1:dim(merge.df)[1])
      {
        if(merge.df[i, c("PKCNC")] < merge.df[i, c("Lower_Limit")] ||
           merge.df[i, c("PKCNC")] > merge.df[i, c("Upper_Limit")] )
        {
          abnormal=abnormal+1
          #print(merge.df[i,c("PKCNC", "Lower_Limit" , "Upper_Limit")])
        }
      }
      print(file)
      print(dim(merge.df)[1])
      print(abnormal)
      
      abnormalcon <- dbConnect(SQLite(), "AbnormalStatus")
      abnormalstatus<- tibble(file = basename(file),
                           Total =dim(merge.df)[1], 
                           abnormal =abnormal,
                           time = as.character(now()),
                           action = paste("autoscan file Sucess")  )
      dbWriteTable( abnormalcon, "AbnormalStatus" , abnormalstatus , append =TRUE)
      #print(dbReadTable(abnormalcon  ,"AbnormalStatus"))
      dbDisconnect(abnormalcon)
      
    }
    else{
      dbWriteTable( th, as.character(study) , data , overwrite  =TRUE)
      
    }
    
    dbDisconnect(th)
  }
}

})

observeEvent(input$show, {
abnormalcon <- dbConnect(SQLite(), "AbnormalStatus")
abdata<- dbReadTable(abnormalcon  ,"AbnormalStatus")
dbDisconnect(abnormalcon)
output$stats <- DT::renderDataTable(abdata, 
             options = list(dom = 't', scroller = TRUE, scrollX = TRUE,
              "pageLength" = 100), rownames = TRUE)

})

  }




)}


# Module to call server function
autouploader_module <- function(id , uploadedData , credentials) {
  autouploader_UI(id)
  autouploader_server(id , uploadedData, credentials)
  
}