Admin_UI <- function(id) {
  ns <- NS(id)
  
      fluidRow(

         box(
           title = "AutoScan setting",
           closable = FALSE,
           width = 12,
           status = "warning",
           solidHeader = FALSE,
           collapsible = TRUE,
           fluidRow(
             column(12,
                    align = "center",
                    DT::dataTableOutput(ns("dtout")) #%>% withSpinner(color = "#0095FF")
             )
           )
         ),

         box(
           title = "User's setting",
           closable = FALSE,
           width = 12,
           status = "warning",
           solidHeader = FALSE,
           collapsible = TRUE,
           fluidRow(
             column(12,
                    actionButton(ns("update_table"), label = "update user", style = "fill"),
                    rHandsontableOutput(ns("hot"))
                    
                    
             )
           )
         ),

  box(
    title = "Audit table",
    closable = FALSE,
    width = 12,
    status = "warning",
    solidHeader = FALSE,
    collapsible = TRUE,
    fluidRow(
      column(12,
             actionButton(ns("update_audit"), label = "update", style = "fill"),
             br(),
             
             DT::dataTableOutput(ns("dtout2")) #%>% withSpinner(color = "#0095FF")
      )
    )
  )
      )}
     

Admin_server <- function(id , credentials) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    values <- reactiveValues()
    udb <- dbConnect(SQLite(), "users")
    DF <- dbReadTable(udb , "user")
    dbDisconnect(udb)
    #to add new colun 
    # DF$AbnormalStatus =FALSE
    # DF$uploaddata =FALSE
    # DF$Threshold =FALSE
    # DF$Admin =FALSE
    #print(DF)
    DF$AbnormalStatus <-as.logical(DF$AbnormalStatus)
    DF$uploaddata <-as.logical(DF$uploaddata)
    DF$Threshold <-as.logical(DF$Threshold)
    DF$Admin <-as.logical(DF$Admin)
    
    
    #print(DF)
    #print(class(DF))
    values[["DF"]] <- DF
    
    
    ## Handsontable
    observe({
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
    
    output$hot = renderRHandsontable(rhandsontable(DF ,useTypes = as.logical("TRUE"), stretchH = "none") %>%
                                        hot_col("user" ,readOnly = TRUE)  %>%
                                        hot_col("password" ,readOnly = TRUE , type="password")  %>%
                                        hot_col("AbnormalStatus", type = "checkbox") %>%
                                        hot_col("uploaddata",type = "checkbox")  %>%
                                        hot_col("Threshold",type = "checkbox" ) %>%
                                        hot_col("Admin",type = "checkbox" )  )    
    
    
    
    observeEvent(input$update_table, {
      #DF <-uploadedData$data1()
      finalDF <- isolate(values[["DF"]])
      #print(finalDF)
      udb <- dbConnect(SQLite(), "users")
      dbWriteTable( udb, "user" , finalDF , overwrite  =TRUE)
      dbDisconnect(udb)
    
      
      audit <- dbConnect(SQLite(), "audit")
      loginaudits<- tibble(user = credentials()$info$user,
                           sessionid = credentials()$info$sessionid, 
                           time = as.character(now()),
                           action = paste("user updated"))
      dbWriteTable( audit, "audits" , loginaudits , append =TRUE)
      #print(tail(dbReadTable(audit ,"audits"), n=20))
      dbDisconnect(audit)
      
    })
    
    # output$dtout <- DT::renderDataTable(datatable(THdata(),
    #                                               options = list(dom = 't', scroller = TRUE, scrollX = TRUE, "pageLength" = 100),
    #                                               rownames = FALSE))
    # 
    # output$dtout1 <- DT::renderDataTable(datatable(data1(),
    #                                                options = list(dom = 't', scroller = TRUE, scrollX = TRUE, "pageLength" = 100),
    #                                                rownames = FALSE))
    
    observeEvent(input$update_audit, {
      
    audit <- dbConnect(SQLite(), "audit")
    audit.df<-dbReadTable(audit ,"audits")
    dbDisconnect(audit)
    
    output$dtout2 <- DT::renderDataTable(datatable(audit.df, 
                                        options = list(scrollX = TRUE)))
    })
    
  })
}


# Module to call server function
Admin_module <- function(id, credentials) {
  Admin_UI(id)
  Admin_server(id , credentials)
  
  
}