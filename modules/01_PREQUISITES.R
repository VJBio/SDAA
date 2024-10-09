library(shiny)
library(bs4Dash)
library(DT)  # Ensure DT is loaded for rendering data tables
library(waiter)  # Make sure to include this for the Waiter functionality

# UI Function
PREQUISITES_UI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "",
    value = "Tab1",
    fluidRow(
      column(3, fileInput(ns("files"), "Upload SDAA Excel File", accept = ".xlsx"))
    ),
    fluidRow(
      box(
        title = "SDAA DATA",
        closable = FALSE,
        width = 12,
        status = "warning",
        solidHeader = FALSE,
        collapsible = TRUE,
        fluidRow(
          column(12,
                 align = "center",
                 DT::dataTableOutput(ns("dtout")) %>% withSpinner(color = "#0095FF")
          )
        )
      )
    )
  )
}



# Server Function
PREQUISITES_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)  # Correctly create the namespace
    
    uploadedData <- reactiveVal(NULL)
    
    
    w<- Waiter$new()
    observeEvent(input$files,{
      if(is.null(input$files))
      {
        sendSweetAlert(session = session, title = "Error", text = "No Records Present", type = "error")
      }
      else
      {
        w$show()
        
        output$dtout <- DT::renderDataTable({
          if(str_sub(input$files$name, -4) != "xlsx"){
            shinyalert("Error.....!", "Please Select .xlsx file.", type = "error")
          }
          
          else
          {
            data <- readxl::read_excel(input$files$datapath)
            uploadedData(data)
            # FinalData <- readxl::read_xlsx(input$files$datapath)
            print(head(uploadedData, 10))
            # print(colnames(data))
            datatable(uploadedData(), 
                      options = list(
                        dom = 't',
                        scroller = TRUE,
                        scrollX = TRUE, 
                        "pageLength" = 100),
                      rownames = FALSE
            )
          }
          
        })
        
        w$hide()
      }
      print("Uploaded Data")
      print(uploadedData)
      
    })
    return(uploadedData)
  })
}

# Module to call server function
PREQUISITES_module <- function(id) {
  moduleServer(id, function(input, output, session) {
    PREQUISITES_server(id)
  })
}
