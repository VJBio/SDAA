

if (file.exists("Threshold")) {
  th <- dbConnect(SQLite(), "Threshold")
} else {
  th <- dbConnect(SQLite(), "Threshold")
 
}
dbDisconnect(th)


# UI Function
PREQUISITES_UI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "",
    value = "Tab1",
    box(
      width = 12,
      column(12, fileInput(ns("files"), "Upload SDAA Excel File", accept = ".csv"))

    ),
   
    
#     fluidRow(
#      
#        box(
#          title = "SDAA DATA",
#          closable = FALSE,
#          width = 12,
#          status = "warning",
#          solidHeader = FALSE,
#          collapsible = TRUE,
#          fluidRow(
#            column(12,
#                   align = "center",
#                   DT::dataTableOutput(ns("dtout")) %>% withSpinner(color = "#0095FF")
#            )
#          )
#        ),
#       
#        box(
#          title = "Clin Pharma Lead Normal Values",
#          closable = FALSE,
#          width = 12,
#          status = "warning",
#          solidHeader = FALSE,
#          collapsible = TRUE,
#          fluidRow(
#            column(12,
#                   align = "center",
#                   DT::dataTableOutput(ns("dtout1")) %>% withSpinner(color = "#0095FF")
#            )
#          )
#        ),
# 
# box(
#   title = "Treatment Codes File",
#   closable = FALSE,
#   width = 12,
#   status = "warning",
#   solidHeader = FALSE,
#   collapsible = TRUE,
#   fluidRow(
#     column(12,
#            align = "center",
#            DT::dataTableOutput(ns("dtout2")) %>% withSpinner(color = "#0095FF")
#     )
#   )
# )
#    )
  )
}



# Server Function
# Server Function (modified)
PREQUISITES_server <- function(id , credentials) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    #credentials<-NS(credentials)
    # print("credentials------->")
    # print(credentials()$info)
    # print("credentials------->")
    # 
    observe({ 
      if(credentials()$user_auth){
        
    THdata  <- reactiveValues()
    data2  <- reactiveValues()
    data1  <- reactiveValues()
      }
    })
    w <- Waiter$new(id = ns("waiter"))  # Namespaced waiter ID
    
    read_data <- function(file_input , data_type) {
      req(file_input)
      #####################
      # audit log for file input
      ####################
      validcol<-c("STUDYID" ,"TREATXT" , "VISIT", "PCTPT", "PCORRES" )
      if (str_sub(file_input$name, -3) == "csv") {
        w$show()
        tryCatch({
          #data <- readxl::read_excel(file_input$datapath)
          data <- read.csv(file_input$datapath)
          
          w$hide()
          data
        }, error = function(e) {
          w$hide()
          showModal(modalDialog(title = "Error", paste("Error reading file:", e$message)))
          NULL
        })
      } else {
        w$hide() # Hide waiter if wrong file type
        sendSweetAlert(session, title = "Error", text = "Please Select .csv file.", type = "error")
        NULL
      }
      # data <- read.csv(file_input$datapath)
      #   validcol<-c("STUDYID" ,"TREATXT" , "VISIT", "PCTPT", "PCORRES" )
      #   if (length(intersect(validcol , colnames(data))) ==5 ) {
      #     w$show()
      #     tryCatch({
      #       
      #   w$hide()
      #   data
      #   
      #     }, error = function(e) {
      #       w$hide()
      #       showModal(modalDialog(title = "Error", paste("Error reading file:", e$message)))
      #       NULL
      #     })
      #     
      #  
      #   
      # } else {
      #   w$hide() # Hide waiter if wrong file type
      #   sendSweetAlert(session, title = "Error", text = "Please Select .csv file.", type = "error")
      #   NULL
      # }
    }
   
    read_data2 <- function(file_input , dataTab, data_type) {
      req(file_input , dataTab)
      data <-  dataTab
      #print(data1)
      if(data_type ==1)
      {
        #print(data_type)
        data <- distinct(data[c("STUDYID")])
        #th <- dbConnect(SQLite(), "Threshold")
        #dbWriteTable( th, "STUDYID" , data2 , append =TRUE)
        
        #dbDisconnect(th)
        
      }else if(data_type ==2)
      {
        #print(data_type)
        
        data <- distinct( data[c("STUDYID" ,"TREATXT" , "VISIT", "PCTPT" )])
        data$Upper_Limit <-""
        data$Lower_Limit <-""
        #data<- readRDS("table.rds")
        study <- distinct(data[c("STUDYID")])
        th <- dbConnect(SQLite(), "Threshold")
        #query= paste0("SELECT * FROM threshold where STUDYID in ", study)
        #res <- dbSendQuery(th,query )
        #print(dbFetch(res))
        #print(data,n=4)
        #print(dbListTables(th))
        if(study %in% dbListTables(th) )
        {
          query= paste0("SELECT * FROM " , study)
          res <- dbSendQuery(th,query )
          data <- dbFetch(res) 
          dbClearResult(res)
        }
        else{
          dbWriteTable( th, as.character(study) , data , overwrite  =TRUE)
          
        }
        
        dbDisconnect(th)
      }
      return(data)
      
    }

    
    THdata <- reactive({ read_data(input$files ,1) })
    data2 <- reactive({ read_data2(input$files , THdata(),1) })
    
    data1 <- reactive({read_data2(input$files ,THdata() ,2) })
    
    observeEvent(input$files, {
      #audit for upload files
      
    })
      
      
    data1 <-  eventReactive(input$files, {
      #session$reload()
       # print("upload button ---------->")
     # print(credentials()$info$user)
     # print(credentials()$info$sessionid)
       audit <- dbConnect(SQLite(), "audit")
       loginaudits<- tibble(user = session$user,
                            sessionid = credentials()$info$sessionid, 
                            time = as.character(now()),
                            action = paste("uploaded file Sucess", input$files)  )
       dbWriteTable( audit, "audits" , loginaudits , append =TRUE)
       #print(tail(dbReadTable(audit ,"audits"), n=20))
       dbDisconnect(audit)
       
     read_data2(input$files ,THdata() ,2)
     
      
      
})
    # output$dtout <- DT::renderDataTable(datatable(THdata(), 
    #                                               options = list(dom = 't', scroller = TRUE, scrollX = TRUE, "pageLength" = 100),
    #                                               rownames = FALSE))
    # 
    # output$dtout1 <- DT::renderDataTable(datatable(data1(), 
    #                                                options = list(dom = 't', scroller = TRUE, scrollX = TRUE, "pageLength" = 100),
    #                                                rownames = FALSE))
    # 
    # output$dtout2 <- DT::renderDataTable(datatable(data2(), 
    #                                                options = list(dom = 't', scroller = TRUE, scrollX = TRUE, "pageLength" = 100),
    #                                                rownames = FALSE))
    
    return(list(THdata = THdata, data1 = data1, data2 = data2))
  })
}


# Module to call server function
PREQUISITES_module <- function(id, credentials) {
  PREQUISITES_UI(id)
  PREQUISITES_server(id , credentials)
  
  
}