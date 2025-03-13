# Load libraries ----
library(AzureAuth)  # authentication
library(AzureGraph)  # using Graph API directly
library(Microsoft365R)  # accessing SharePoint lists
library(shiny)  # web application development
# packages for wrangling, reading data, and rendering data table
library(dplyr)
library(tibble)
library(tidyr)
library(DT)
library(readr)

# Define key configuration values ----
# TODO - AFTER successfully running this example you can replace the siteURL with the Sharepoint URL of interest
siteURL <- "https://pfizer.sharepoint.com/sites/RinPfizer/"
# define redirect address
# example app URL below
# TODO - replace with the redirect URL for your app (called appURL)
appURL <- "https://rsc.pfizer.com/connect/#/apps/d2a478e8-a0cd-4516-a585-f3ca981a4e2f"

# Source helper script ----
source("setup2.R")
#setup port based on app URL
setup_port(appURL)

# Define app UI for successful authorization ----
app_ui <- fluidPage(
  h1("Using Microsoft365R in a Shiny App"),
  h2("Display the contents of the Example Data folder for R in Pfizer Sharepoint"),
  DT::dataTableOutput("table1"),
  h2("Display example CSV file from the Example Data folder"),
  DT::dataTableOutput("table2"),
  h2("Display Sharepoint List called Test in R in Pfizer Sharepoint"),
  DT::dataTableOutput("table3")
)

# Define UI for app ----
ui <- build_ui(app_ui, appURL)

# Define server logic ----
server <- function(input, output, session) {
  # generate authorization token using helper function
  azureToken <- generate_azureToken(session, appURL)
  
  # the code to obtain a docsTable reactive is specific to the R in Pfizer Sharepoint site (not every site will have an Example Data folder)  
  # read Example Data list of files from SharePoint and process into data.frame
  docsTable <- reactive({
    # use authorization token to attempt to access SharePoint list
    # try ms_graph
    rawData <- try({AzureGraph::ms_graph$
        new(token = azureToken)$
        get_sharepoint_site(site_url = siteURL)$
        get_drive()$
        list_files(path = file.path("General", "Example Data"))
    }) 
    # if user doesn't have correct permissions redirect to SharePoint URL to request access
    # if error
    if ("try-error" %in% class(rawData)) {
      session$sendCustomMessage("accessfail", siteURL)
      return()
    } 
    # display raw data
    rawData %>%
      tibble() %>%
      unnest(cols = everything())
  })
  # the code to obtain the reactives or download files is specific to the R in Pfizer Sharepoint site (not every site will have an Example Data folder) 
  # the remaining server code assigns the data to shiny outputs
  # create temp file path to save Sharepoint file
  tempFile <- reactive({
    tempFile <- tempfile(fileext = paste0(".", "csv"))
    tempFile
  })
  # after confirming list of tables read in from Document
  # use tempFile reactive to define temp save destination
  observe({
    req(docsTable())
    AzureGraph::ms_graph$
      new(token = azureToken)$
      get_sharepoint_site(site_url = siteURL)$
      get_drive()$
      download_file(src = file.path("General", "Example Data", "mtcars.csv"),
                    dest = tempFile())
  })
  # get reactive with the example SharePoint list as dataframe
  exampleSpl <- reactive({
    req(docsTable())
    sharepointSite <- AzureGraph::ms_graph$
      new(token = azureToken)$
      get_sharepoint_site(site_url = siteURL)
    # get a SharePoint list
    sharepointLst <- sharepointSite$get_list("Test")
    # read from the SharePoint
    lst_items <- sharepointLst$list_items(as_data_frame=TRUE)
    lst_items
  })
  # the code to obtain a docsTable reactive is specific to the R in Pfizer Sharepoint site (not every site will have an Example Data folder)  
  # render data frame into table UI
  output$table1 <- DT::renderDataTable({
    docsTable()
  }) 
  # render data frame into table UI
  output$table2 <- DT::renderDataTable({
    readr::read_csv(tempFile())
  })
  # render data frame into table UI
  output$table3 <- DT::renderDataTable({
    exampleSpl()
  })
}

# Run Shiny app ----
shinyApp(ui, server)