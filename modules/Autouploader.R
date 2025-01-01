

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
                h3("SDAA Abnormalities scan status"),
                #actionButton(ns("show"), label = "Show Abnormalities", style = "fill"),
                #actionButton(ns("scan"), label = "scan Abnormalities", style = "fill"),
                br(),
                closable = FALSE,
                width = 12,
                status = "warning",
                solidHeader = FALSE,
                
                collapsible = TRUE,
                fluidRow(
                  column(12,
                         align = "center",
                         DT::DTOutput(ns("stats")) #%>% withSpinner(color = "#0095FF")
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
  
  
  abnormalcon <- dbConnect(SQLite(), "AbnormalStatus")
  if(dbExistsTable(abnormalcon, "abnormalstatus") )
  {
   dbRemoveTable(abnormalcon, "abnormalstatus")
  }
  path = "/home/jhav11/VJ"
  path="Data"
list_of_files <- list.files(path = path,
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)


validcol<-c("STUDYID" ,"TREATXT" , "VISIT", "PCTPT", "PCORRES" )
#print(list_of_files)
for(file in list_of_files)
{
  
  df<-read.csv(file)
  if(length(intersect(validcol , colnames(df))) ==5)
  {

    data <- distinct( df[c("STUDYID" ,"TREATXT" , "VISIT", "PCTPT" )])
    #colnames(data) <-c("STUDYID" ,"TREATXT" , "VISIT", "PCTPT" )
   
   
    study <- distinct(data[c("STUDYID")])
    #PKTERM <- distinct(data[c("PKTERM")])
    #study <-paste(as.character(study), as.character(PKTERM))
    th <- dbConnect(SQLite(), "Threshold")
   
    if(study %in% dbListTables(th) )
    {
      query= paste0("SELECT * FROM " , "'",study,"'")
      res <- dbSendQuery(th,query )
      data <- dbFetch(res)  
      dbClearResult(res)
      
     # print("from DB-------->")
     # print(data)
      merge.df <- merge(df,data, by=c("STUDYID" ,"TREATXT" , "VISIT", "PCTPT") )
      #print(head(merge.df))
      data.num <- as.numeric(merge.df$PCORRES)
      data.num[is.na(data.num)] <- 0
      merge.df$PCORRES <- data.num
      merge.df$Lower_Limit<-as.numeric(merge.df$Lower_Limit)
      merge.df$Upper_Limit<-as.numeric(merge.df$Upper_Limit)
      
     
      
      merge.df <- merge.df %>%
        mutate(
          Status = ifelse( !is.na(Lower_Limit) & !is.na(Upper_Limit) &  (PCORRES < Lower_Limit | PCORRES > Upper_Limit), "Abnormal", "Normal")
          #Status = ifelse( (PCORRES_numeric < Lower_Limit | PCORRES_numeric > Upper_Limit), "Abnormal", "Normal")
          
        )
      #print(table(merge.df$Status))
      remark=paste("Threshold set")
       if(sum(is.na(merge.df$Lower_Limit)) >0 | sum(is.na(merge.df$Upper_Limit)) >0)
       {
       remark=paste("Threshold partially set")
       }
      if(sum(is.na(merge.df$Lower_Limit)) == length(merge.df$Lower_Limit) & 
         sum(is.na(merge.df$Upper_Limit)) ==length(merge.df$Lower_Limit) ) {
        remark=paste("Threshold not set")
      }
      abnormalcon <- dbConnect(SQLite(), "AbnormalStatus")
      abnormalstatus<- tibble(file = basename(file),
                          study=as.character(study),
                          remark=remark,
                           Total =dim(merge.df)[1], 
                           abnormal =sum(merge.df$Status =="Abnormal"),
                           time = as.character(now()),
                           action = paste("autoscan file Sucess")  )
     # print(abnormalstatus)
      dbWriteTable( abnormalcon, "AbnormalStatus" , abnormalstatus , append =TRUE)
      #print(dbReadTable(abnormalcon  ,"AbnormalStatus"))
      dbDisconnect(abnormalcon)
      
    }
    else{
      data$Lower_Limit <-""
      data$Upper_Limit <-""
      dbWriteTable( th, as.character(study) , data , overwrite  =TRUE)
      abnormalcon <- dbConnect(SQLite(), "AbnormalStatus")
      abnormalstatus<- tibble(file = basename(file),
                              study=as.character(study),
                              remark="Threshold not set",
                              Total =dim(data)[1], 
                              abnormal =0,
                              time = as.character(now()),
                              action = paste("autoscan file Sucess")  )
      dbWriteTable( abnormalcon, "AbnormalStatus" , abnormalstatus , append =TRUE)
      #print(dbReadTable(abnormalcon  ,"AbnormalStatus"))
      dbDisconnect(abnormalcon)
      
    }
    
    dbDisconnect(th)
  }
}


audit <- dbConnect(SQLite(), "audit")
loginaudits<- tibble(user = credentials()$info$user,
                     sessionid = credentials()$info$sessionid, 
                     time = as.character(now()),
                     action = paste("scan file Sucess")  )
dbWriteTable( audit, "audits" , loginaudits , append =TRUE)
#print(tail(dbReadTable(audit ,"audits"), n=20))
dbDisconnect(audit)

abnormalcon <- dbConnect(SQLite(), "AbnormalStatus")
abdata<- dbReadTable(abnormalcon  ,"AbnormalStatus")
dbDisconnect(abnormalcon)
output$stats <- DT::renderDT(DT::datatable(abdata, options = list(scrollX = TRUE) ,rownames = FALSE)%>%
  formatStyle(
    columns = names(abdata),
    target = 'row',
    backgroundColor = styleEqual("Threshold not set", "pink") )%>%
  formatStyle(
    columns = names(abdata),
    target = 'row',
    backgroundColor = styleEqual("Threshold partially set", "yellow") )%>%
    formatStyle(
      columns = names(abdata),
      target = 'row',
      backgroundColor = styleEqual("Threshold set", "lightgreen") )
  )



from = c(  "Auto Scan run sucessfully on ","Files with Abormalties" )

message =c( as.character(abstatus("date")) , as.character(abstatus("count")))
icons<-c("truck" , "exclamation-triangle")
messageData =  data.frame(from , message,icons)
#print(messageData)
output$messageMenu <- renderMenu({
  msgs <- apply(messageData, 1, function(row) {
    messageItem(from = row[["from"]], message = row[["message"]])
  })

  dropdownMenu(type = "messages", .list = msgs)
})

})

observeEvent(input$show, {
abnormalcon <- dbConnect(SQLite(), "AbnormalStatus")
abdata<- dbReadTable(abnormalcon  ,"AbnormalStatus")
dbDisconnect(abnormalcon)
output$stats <- DT::renderDT(DT::datatable(abdata, options = list(scrollX = TRUE) ,rownames = FALSE)%>%
                               formatStyle(
                                 columns = names(abdata),
                                 target = 'row',
                                 backgroundColor = styleEqual("Threshold not set", "pink") )%>%
                               formatStyle(
                                 columns = names(abdata),
                                 target = 'row',
                                 backgroundColor = styleEqual("Threshold partially set", "yellow") )%>%
                               formatStyle(
                                 columns = names(abdata),
                                 target = 'row',
                                 backgroundColor = styleEqual("Threshold set", "lightgreen") )
)
})



  }




)}


# Module to call server function
autouploader_module <- function(id , uploadedData , credentials) {
  autouploader_UI(id)
  autouploader_server(id , uploadedData, credentials)
  
}