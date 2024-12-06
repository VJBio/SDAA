library(rhandsontable)
library(shiny)
#editTable <- function(DF, outdir=getwd(), outfilename="table"){
# UI Function

EditTable_UI <- function(id) {
  ns <- NS(id)
  fluidPage(
   
          wellPanel(
        h3("Update threshold"),
        br(),
        actionButton(ns("save"), "Save table")
      ),
    mainPanel(
      width=12,
      rHandsontableOutput(ns("hot")),
    )
  )
}

EditTable_server <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
    values <- reactiveValues()
   

    ## Handsontable
    observe({
      DF <- uploadedData$data1()
      if (!is.null(input$hot)) {
         DF <- uploadedData$data1()
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
        rhandsontable(DF, useTypes = as.logical("TRUE"), stretchH = "all")
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
    })
    
    
    
    
  })
  
} 
# Module to call server function
EditTable_module <- function(id , uploadedData) {
  EditTable_UI(id)
  EditTable_server(id , uploadedData)
  
}



