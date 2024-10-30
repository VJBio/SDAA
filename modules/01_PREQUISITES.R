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
      column(3, fileInput(ns("files"), "Upload SDAA Excel File", accept = ".xlsx")),
      
      column(3, fileInput(ns("files1"), "Upload Clin Pharma Lead Normal Values", accept = ".xlsx")),
      
      column(3, fileInput(ns("files2"), "Upload Treatment Codes File", accept = ".xlsx"))
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
      ),
      
      box(
        title = "Clin Pharma Lead Normal Values",
        closable = FALSE,
        width = 12,
        status = "warning",
        solidHeader = FALSE,
        collapsible = TRUE,
        fluidRow(
          column(12,
                 align = "center",
                 DT::dataTableOutput(ns("dtout1")) %>% withSpinner(color = "#0095FF")
          )
        )
      ),
      
      box(
        title = "Treatment Codes File",
        closable = FALSE,
        width = 12,
        status = "warning",
        solidHeader = FALSE,
        collapsible = TRUE,
        fluidRow(
          column(12,
                 align = "center",
                 DT::dataTableOutput(ns("dtout2")) %>% withSpinner(color = "#0095FF")
          )
        )
      )
    )
  )
}



# Server Function
# Server Function (modified)
PREQUISITES_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    w <- Waiter$new(id = ns("waiter"))  # Namespaced waiter ID
    
    read_data <- function(file_input) {
      req(file_input)
      if (str_sub(file_input$name, -4) == "xlsx") {
        w$show()
        tryCatch({
          data <- readxl::read_excel(file_input$datapath)
          w$hide()
          data
        }, error = function(e) {
          w$hide()
          showModal(modalDialog(title = "Error", paste("Error reading file:", e$message)))
          NULL
        })
      } else {
        w$hide() # Hide waiter if wrong file type
        sendSweetAlert(session, title = "Error", text = "Please Select .xlsx file.", type = "error")
        NULL
      }
    }
    
    THdata <- reactive({ read_data(input$files) })
    data1 <- reactive({ read_data(input$files1) })
    data2 <- reactive({ read_data(input$files2) })
    
    output$dtout <- DT::renderDataTable(datatable(THdata(), 
                                                  options = list(dom = 't', scroller = TRUE, scrollX = TRUE, "pageLength" = 100),
                                                  rownames = FALSE))
    
    output$dtout1 <- DT::renderDataTable(datatable(data1(), 
                                                   options = list(dom = 't', scroller = TRUE, scrollX = TRUE, "pageLength" = 100),
                                                   rownames = FALSE))
    
    output$dtout2 <- DT::renderDataTable(datatable(data2(), 
                                                   options = list(dom = 't', scroller = TRUE, scrollX = TRUE, "pageLength" = 100),
                                                   rownames = FALSE))
    
    return(list(THdata = THdata, data1 = data1, data2 = data2))
  })
}


# Module to call server function
PREQUISITES_module <- function(id) {
  moduleServer(id, function(input, output, session) {
    PREQUISITES_server(id)
  })
}