# The utility script here helps to deploy Shiny apps 
# that connect to Sharepoint and get published onto RStudio Connect
# Define needed values ----
# define SharePoint site and Azure app identifier
tenantName <- "pfizer"
azureId <- "4bd12a20-75e7-4b1a-bd53-0f0d04a96ded"

# Helper function to define Shiny port ----
setup_port <- function(appURL) {
  port <- httr::parse_url(appURL)$port
  options(shiny.port = if (is.null(port)) {
    443
  } else {
    as.numeric(port)
  })
}

# Access client secret stored in environment variable ----
# note: client secrets should NEVER be written in code
azureSecret <- Sys.getenv("SHINY_CLIENT_SECRET", "")
if (azureSecret == "")
  azureSecret <- NULL

# Get the Graph permissions listed for the app, plus an ID token ----
azureScopes <-
  c("https://graph.microsoft.com/.default",
    "openid",
    "offline_access")

# Define JavaScript for redirect when access failure occurs  ----
# https://shiny.rstudio.com/articles/js-send-message.html (Scenario 1)
# https://stackoverflow.com/questions/47157880/redirect-in-shiny-app (first answer)
jsAccessFailure <- paste0(
  "Shiny.addCustomMessageHandler('accessfail', function(message) {",
  "window.location.assign(message);",
  "});"
)

# Define function to build UI for the app ----
build_ui <- function(app_ui, appURL) {
  function(request) {
    # on app start-up, check if authorization has occurred
    query <- shiny::parseQueryString(request$QUERY_STRING)
    if (is.null(query$code)) {
      # if it hasn't occurred redirect
      # build_authorization_uri
      authURI <- AzureAuth::build_authorization_uri(
        resource = azureScopes,
        tenant = tenantName,
        app = azureId,
        redirect_uri = appURL,
        version = 2
      )
      jsRedirect <- sprintf("location.replace(\"%s\");", authURI)
      tags$script(HTML(jsRedirect))
    } else{
      # note that the app_ui has the shiny UI the user should expect to see after authorization has occured
      app_ui
    }
  }
}


# Define fucntion to get Azure Token ----
generate_azureToken <- function(session, appURL){
  # check whether authorization has occurred
  # parseQueryString and isolate
  query <- shiny::parseQueryString(
    isolate(
      session$clientData$url_search
    )
  ) 
  # if it hasn't do not progress server code
  if (is.null(query$code)) {  
    return()
  } 
  # generate authorization token from URL
  # try get_azure_token
  azureToken <- try({AzureAuth::get_azure_token(
    resource = azureScopes, 
    tenant = tenantName, 
    app = azureId, 
    password = azureSecret, 
    auth_type = "authorization_code",
    authorize_args = list(redirect_uri = appURL), 
    version = 2, 
    use_cache = FALSE, 
    auth_code = query$code
  )})  
  # if URL has been tampered with or is from an expired session, redirect to app URL
  # if error  
  if ("try-error" %in% class(azureToken)) {
    session$sendCustomMessage("accessfail", appURL)
    return()
  }
  
  return(azureToken)
}
