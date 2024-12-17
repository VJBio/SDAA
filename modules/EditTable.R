library(rhandsontable)
library(shiny)
#editTable <- function(DF, outdir=getwd(), outfilename="table"){
# UI Function

EditTable_UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # fluidRow(
    #   column(2, pickerInput(ns("study_id_select_th"), "Select Study ID", 
    #                         choices = NULL, options = list(`live-search` = TRUE))),  # Dropdown for Study ID
    #   
    #        column(2, actionBttn(ns("update_table"), label = "show table", style = "fill")) # Adding update button
    #   
    # ),
   
          wellPanel(
        #h3("Update threshold"),
        #br(),
            width=8,
            pickerInput(ns("study_id_select_th"), "Select Study ID", 
                        choices = NULL, options = list(`live-search` = TRUE)),
            actionButton(ns("update_table"), label = "Show table", style = "fill"),
        #actionBttn(ns("save"), label = "Save table", style = "fill")
        
        actionButton(ns("save"), label = "Save table", style = "fill")
      ),
    mainPanel(
      width=12,
      rHandsontableOutput(ns("hot")),
    )
  )
}

EditTable_server <- function(id, uploadedData, credentials) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #print("credentials--EditTable----->")
    #print(credentials()$info)
    #print("credentials---EditTable---->")
    values <- reactiveValues()
    observe({ 
      if( !is.null(uploadedData$data1())) {
    DF <- uploadedData$data1()
      }
    })
    ################
    # add logic to hide save button if DF is not defined
    ###############
   
    # Updating Study ID dropdown choices when data is available
    #observeEvent(uploadedData$THdata(), {
      th <- dbConnect(SQLite(), "Threshold")
      study_id <- dbListTables(th)
      dbDisconnect(th)
      updatePickerInput(session, "study_id_select_th", choices = study_id)
    #})
      
      uploadedData$data1 <- eventReactive(input$update_table, { 
       # print("inside select--------------->")
         thedit <- dbConnect(SQLite(), "Threshold")
         study_id <- dbListTables(thedit)
         updatePickerInput(session, "study_id_select_th", choices = study_id)
         
         query= paste0("SELECT * FROM " , "'",input$study_id_select_th,"'")
         #print(query)
         res <- dbSendQuery(thedit,query )
         DF <- dbFetch(res)
         #print(head(DF))
         dbDisconnect(thedit)
         DF
      })
      
      observeEvent(input$update_table, {
       # print("inside select 2--------------->")
        DF <- uploadedData$data1()
       # print(head(DF))
        values[["DF"]] <- DF
         
        output$hot = renderRHandsontable(rhandsontable(DF ,useTypes = as.logical("TRUE"), stretchH = "all")  %>%
          hot_col("STUDYID" ,readOnly = TRUE) %>%
          hot_col("TREATXT" ,readOnly = TRUE) %>%
          hot_col("VISIT", readOnly = TRUE) %>%
          hot_col("PCTPT",readOnly = TRUE) 
        )                                           
         })
         
        
      
    ## Handsontable
    observe({
      DF <- uploadedData$data1()
      if (!is.null(input$hot)) {
         #DF <- uploadedData$data1()
        DF = hot_to_r(input$hot)
      } else {
        if (is.null(values[["DF"]]))
          DF <- DF
        else
          DF <- values[["DF"]]
      }
      values[["DF"]] <- DF
      #print("EditTable2")
      #print(DF)
    })
    
    
    output$hot <- renderRHandsontable({
      DF <- uploadedData$data1()
      DF <- values[["DF"]]
      if (!is.null(DF))
        rhandsontable(DF, useTypes = as.logical("TRUE"), stretchH = "all") %>%
      hot_col("STUDYID" ,readOnly = TRUE) %>%
      hot_col("TREATXT" ,readOnly = TRUE) %>%
      hot_col("VISIT", readOnly = TRUE) %>%
      hot_col("PCTPT",readOnly = TRUE) 
    })
    
    ## Save 
    observeEvent(input$save, {
      #DF <-uploadedData$data1()
      finalDF <- isolate(values[["DF"]])
      #print(finalDF)
      #print(class(finalDF))
      #print("heehehe")
      #saveRDS(finalDF, file=file.path("./", sprintf("%s.rds", "table")))
      study <- distinct(finalDF[c("STUDYID")])
     # print(study)
      th <- dbConnect(SQLite(), "Threshold")
      
      dbWriteTable( th, as.character(study) , finalDF , overwrite  =TRUE)
      dbDisconnect(th)
      
      audit <- dbConnect(SQLite(), "audit")
      loginaudits<- tibble(user = credentials()$info$user,
                           sessionid = credentials()$info$sessionid, 
                           time = as.character(now()),
                           action = paste("Threshold values updated for", input$study_id_select_th  ))
      dbWriteTable( audit, "audits" , loginaudits , append =TRUE)
      #print(tail(dbReadTable(audit ,"audits"), n=20))
      dbDisconnect(audit)
      
    })
    
    
    
    
  })
  
} 
# Module to call server function
EditTable_module <- function(id , uploadedData , credentials) {
  EditTable_UI(id)
  EditTable_server(id , uploadedData, credentials)
  
}



