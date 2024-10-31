library(shiny)
library(bs4Dash)

Data_Load_UI <- function(id){
  ns <- NS(id)

  tabPanel(
    "Load Data",
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
                 align="center",
                 DT::dataTableOutput("dtout") %>% withSpinner(color="#0095FF")
          )

        )

      )
    )

  )
}

#   
#   
  Data_Load_server <- function(id) {
    moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    
    w <- Waiter$new()
    
    req(file)
    
    # file <- input$files
    # ext <- tools::file_ext(file$datapath)
    
    # req(input$files$datapath)

    observeEvent(input$files,{
      if(is.null(input$files)||input$files == 0)
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
            FinalData <- readxl::read_xlsx(input$files$datapath)
            print(head(FinalData))
            # head(FinalData, 50)
            # print(colnames(data))
            datatable(FinalData,
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


    })

    }

    )}


# Data_Load_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     ns <- NS(id)  # Namespace for the UI IDs
#     
#     w <- Waiter$new()
#     
#     # Observe when a file is uploaded
#     observeEvent(input$files, {
#       # Ensure a file is uploaded
#       req(input$files)
#       
#       # Show the loading spinner
#       # w$show()
#       
#       # Render the DataTable with the uploaded file
#       output$dtout <- DT::renderDataTable({
#         # Check if the uploaded file is an Excel file
#         if (tools::file_ext(input$files$name) != "xlsx") {
#           shinyalert("Error", "Please upload a valid .xlsx file", type = "error")
#           return(NULL)  # Exit if the file is not .xlsx
#         }
#         
#         # If it's a valid .xlsx file, read it and display in the datatable
#         FinalData <- readxl::read_excel(input$files$datapath)
#         
#         # Debugging print to check if FinalData is loaded
#         print(FinalData)
#         
#         # Display the first 50 rows in the datatable
#         datatable(FinalData, 
#                   options = list(
#                     dom = 't',      # Only show the table without search bar or pagination
#                     scroller = TRUE, 
#                     scrollX = TRUE, 
#                     pageLength = 50),  # Show up to 50 rows by default
#                   rownames = FALSE
#         )
#       })
#       
#       # Hide the loading spinner after the file has been processed
#       # w$hide()
#     })
#   })
# }


data_load_module <- function(id) {
  moduleServer(id, function(input, output, session) {
    Data_Load_server(id)
  })
}
