# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This is a Shiny web application. You can run the application locally by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#   http://shiny.rstudio.com/
#
#   http://rstudio.github.io/shinydashboard/get_started.html
#
# To deploy an update, update code and data, then load >library(rsconnect), set working
# directory to app.R directory and >deployApp(appName = "GCS_app", appId = 1065708)

#####
# METADATA for app
updateDate <- "September 2023" # <<---- Update with release version

## load libraries  ----
## installs any missing packages this script uses
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('shiny')) install.packages('shiny')
if (!require('shinydashboard')) install.packages('shinydashboard')
if (!require('rsconnect')) install.packages('rsconnect')
if (!require('DT')) install.packages('DT')
# if (!require('GAlogger')) devtools::install_github("bnosac/GAlogger")
if (!require('data.table')) install.packages('data.table')
if (!require('tools')) install.packages('tools')
if (!require('openxlsx')) install.packages('openxlsx')

options(stringsAsFactors = FALSE)
options(shiny.maxRequestSize=100*1024^2)

# ga_set_tracking_id("UA-150850915-4")
# ga_set_approval(consent = TRUE)
# ga_collect_pageview(page = "/GCS_app")

# Define UI for app that draws a histogram ----
ui <- fluidPage(title = "Geocoding Self-Service", 
                theme = "bootstrap.css",
                HTML("<html lang='en'>"),

      fluidRow(
        column(width = 12, 
               style = "background-color:#003366; border-bottom:2px solid #fcba19; position:fixed; z-index:10000",
               tags$header(class="header", style="padding:0 0px 0 0px; display:flex; height:80px; width:100%;",
                           tags$div(class="banner", style="display:flex; justify-content:flex-start; align-items:center; margin: 0 10px 0 10px",
                                    a(href="https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats",
                                      img(src = "bcstats_logo_rev.png", title = "BC Stats", height = "80px", alt = "British Columbia - BC Stats"),
                                      onclick="gtag"
                                    ),
                                    h1("BC Stats - Geocoding Self-Service", style="font-weight:400; color:white; margin: 5px 5px 0 18px;")
                           )
               )
        ),
        column(width = 12,
               tags$fieldset(
                 tags$legend(h2("How to use the geocoding self-service application", style="margin-top:90px")),
                 p("To use the geocoding self-service (GCS), follow the instructions below. You can also download the user guide to access more detailed instructions
                   and definitions of all the fields available. Lookup tables linking boundary codes to regions' names can also be downloaded."),
                 tags$ol(
                   tags$li("Select a file to geocode by clicking the Browse button below. The file should be in .csv, .xls or .xlsx format
                 and include at least one column with a header listing the postal codes to be geocoded. Postal codes should be in the 
                           A1B2C3 format, else, GCS will remove white spaces and capitalize letters from user-provided postal codes."),
                   tags$li("Select the field from your file containing the postal codes to be geocoded."),
                   tags$li("Select the GCS version to use. A new version is released every quarter following the naming convention GCS_YYYYMM."),
                   tags$li("Select the GCS fields you want your postal codes to be geocoded to. Use the Ctrl or Shift key to select multiple entries."),
                   tags$li("Click the 'Geocode Input File' button to begin the process. It should take a few seconds. 
                           When geocoding is done, results will appear in a table. Note that the table will always include the 'ACTIVE' field to indicate
                           if a postal code was in use (Y) or retired (N) for the selected GCS version. Once the file is geocoded, two download buttons will appear."),
                   tags$li("Click one of the 'Download Results' buttons to retrieve either a .csv or .xlsx copy of your geocoded postal codes."),
                   style="font-size:14px; color:#494949"
                   ),
                 br()
               )
        ),
        column(width = 12,
               sidebarLayout(
                 sidebarPanel(style="background-color:#F2F2F2;",
                   tags$fieldset(
                     tags$legend(h3("Geocoding self-service user guide")),
                     p("Click the 'Download user guide' button below to get a PDF copy of the geocoding self-service user guide. 
                       The guide includes detailed instructions and definitions of the fields available."),
                     downloadButton(outputId = "downloadGuide", "Download user guide")
                   ),
                   br(), 
                   tags$fieldset(
                     tags$legend(h3("GeoCoding self-service fields lookup tables")),
                     p("Click the 'Download lookup tables' button below to get a copy of the lookup tables. 
                       The lookup tables can be used to relate field value returned by the geocoding self-service to a region's full name."),
                     downloadButton(outputId = "downloadLookup", "Download lookup tables")
                     
                   ),
                   br(), 
                   tags$fieldset(
                     tags$legend(h3("Additional information")),
                     HTML("Produced by BC Stats "),
                     tags$a(href = "https://github.com/bcgov/GCS", icon("github")),
                     HTML(paste0("<br>", "Last updated: ", updateDate))
                   )
               
               ),
               mainPanel(
                 tags$fieldset(
                   style = "margin-top:20px;",
                   tags$legend(h3("Data selection")),
                   column(width = 12,
                          column(width = 6,
                                 
                                 fileInput(inputId = "geo_input", 
                                           label = h4("File to geocode (.csv, .xls, .xlsx):"),
                                           accept = c(".csv", ".xls", ".xlsx"),
                                           buttonLabel = "Browse...",
                                           placeholder = NULL
                                           ),
                                 selectInput(inputId = "upload_field",
                                             label = h4("Field containing postal codes:"),
                                             choices = c(""),
                                             selectize= FALSE,
                                             multiple = FALSE,
                                             size = 5),
                                 selectInput(inputId = "gcs_version",
                                             label = h4("GCS version to use:"),
                                             choice = sort(str_replace(list.files("data", pattern = "\\.rds$"), ".rds", ""), decreasing = TRUE),
                                             selectize= FALSE,
                                             multiple = FALSE,
                                             size = 5)
                          ),
                          column(width = 6,
                                     selectInput(inputId = "gcs_fields",
                                                 label = h4("GCS fields to return (Ctrl or Shift for multiple):"),
                                                 choices = c(" "),
                                                 selectize= FALSE,
                                                 multiple = TRUE,
                                                 size = 20)
                        )
                   )
                 ),
                 br(),
                 
                 tags$fieldset(
                   tags$legend(h3("Actions")),
                   column(width = 12,
                          tags$div(style = "display:inline-block", actionButton(inputId = "geo_button", label = "Geocode input file")),
                          tags$div(style = "display:inline-block", actionButton(inputId = "resetButton", label = "Reset selection")),
                          tags$div(style = "display:inline-block", uiOutput(outputId = "download_button_csv")),
                          tags$div(style = "display:inline-block", uiOutput(outputId = "download_button_xlsx"))
                          )
                 ),
                 
                 br(),br(),
                 DTOutput("table"),
                 br()
               )
            )
         ),
         column(width = 12,
               style = "background-color:#003366; border-top:2px solid #fcba19;",
               
               tags$footer(class="footer",
                           tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                    tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                            tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                    )
                           )
               )
        )
      )
    )
    

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  ## reactive resetButton send analytics when reset ----
  observeEvent(input$resetButton, {
    
    #ga_collect_event(event_category = "resetButton", event_label = "Reset", event_action = "Reset application")
    
    ## just reload the session
    session$reload()
    
  })
  
  observeEvent(input$geo_input, {
    extension <- tolower(file_ext(input$geo_input$datapath))
    
    if (extension %in% c("csv")){
      head <- colnames(read.csv(input$geo_input$datapath))
    } else if (extension %in% c("xls", "xlsx")) {
      head <- colnames(readxl::read_excel(input$geo_input$datapath, n_max = 10))
    } else {
      stop()
    }
    updateSelectInput(session, "upload_field", choices = head)
  })
  
  observeEvent(input$gcs_version, {
    # NOTE: Hardcode the order of fields to the historical one so clients do not have to redevelop systems.
    # If a new fields is added it will be at the end
    head <- colnames(readRDS(paste0("data/", input$gcs_version, ".rds")) %>% 
                       select(-POSTALCODE))
    # list all possible fields
    all_ordered <- c('MEP_ID', 'POSTALCODE', 'SLI', 'PROV', 'BIRTH_DATE', 'RET_DATE', 'LATITUDE', 'LONGITUDE', 'COMM_NAME', 
                     'CD_2011', 'CSD_2011', 'CDCSD_2011', 'MUN_NAME_2011', 'CMACA_2011', 'CT_2011', 'DA_2011', 'DPL_2011', 'DR_2011', 
                     'CD_2016', 'CSD_2016', 'CDCSD_2016', 'MUN_NAME_2016', 'CMACA_2016', 'CT_2016', 'DA_2016', 'DPL_2016', 'DR_2016',
                     'CD_2021', 'CSD_2021', 'CDCSD_2021', 'MUN_NAME_2021', 'CMACA_2021', 'CT_2021', 'DA_2021', 'DPL_2021', 'DR_2021',
                     'HA', 'HSDA', 'LHA', 'MHA', 'MCFD', 'MCFD_SDA', 'MCFD_LSA', 'PED_1999', 'PED_2009', 'PED_2015', 'SD', 'CR', 
                     'FED_2011', 'FED_2016', 'FED_2021', 'RESP', 'TOURISM', 'CHSA', 'LHA_PRE_2018', 'DB_2016', 'DB_2021', 'GZ', 'TEA', 
                     'POPCTR_2016', 'POPCTR_2021', 'SOURCE', 'SBC', 'TSA', 'WorkBC','ACTIVE')
    
    # which possible fields are used in selected gcs version, we do it this way so we keep the order
    # from all_ordered for output
    all_in_head <- all_ordered %in% head
    
    # keep existing fields in order
    ordered_head <- all_ordered[which(all_in_head == TRUE)]

    updateSelectInput(session, "gcs_fields", choices = ordered_head)
  })
  
  data_df <- eventReactive(input$geo_button, {
    postal_field <- input$upload_field
    
    if(is.null(postal_field)) {stop("Select which field in your data contains postal codes")}
    
    extension <- tolower(file_ext(input$geo_input$datapath))
    if (extension %in% c("csv")){
      in_postal <- read.csv(input$geo_input$datapath) %>% rename(POSTALCODE = rlang::sym(postal_field))
    } else if (extension %in% c("xls", "xlsx")) {
      in_postal <- readxl::read_excel(input$geo_input$datapath) %>% rename(POSTALCODE = rlang::sym(postal_field))
    } else {
      stop()
    }
    
    in_postal$POSTALCODE <- toupper(gsub(" ", "", in_postal$POSTALCODE)) # Remove white spaces and make everything capitalized
    
    if ("ACTIVE" %in% input$gcs_fields) {
      fields <- input$gcs_fields
    } else {
      fields <- c(input$gcs_fields, "ACTIVE")
    }
    
    gcs_data <- readRDS(paste0("data/", input$gcs_version, ".rds")) %>%
      select(POSTALCODE, all_of(fields))
    
    join_data <- in_postal %>%
      left_join(gcs_data, by = "POSTALCODE")
  })
    
  ## reactive resetButton send analytics when reset ----
  observeEvent(input$resetButton, {
    
    #ga_collect_event(event_category = "resetButton", event_label = "Reset", event_action = "Reset application")
    
    ## just reload the session
    session$reload()
    
  })
  
  ## reactive send analytics when download ----
  rv <- reactiveValues(download_flag = 0)
  
  observeEvent(rv$download_flag, {
    
    #ga_collect_event(event_category = "downloadButtonUserVersion", event_label = paste0("User/Version/", session$user, "/", input$gcs_version), event_action = "Download data username/version")
    
  }, ignoreInit = TRUE)
  
  ## reactive send analytics when query table ----
  observeEvent(input$geo_button, {
    
   # ga_collect_event(event_category = "geoButtonUserVersionLength", event_label = paste0("User/Version/Length/", session$user, "/", input$gcs_version, "/", length(data_df()$POSTALCODE)), event_action = "Generate data username/version/length")
    output$download_button_csv <-renderUI({downloadButton('download_file_csv', label = 'Download results (.csv)') })
    output$download_button_xlsx <-renderUI({downloadButton('download_file_xlsx', label = 'Download results (Excel)') })
    
  })
  
  output$download_file_csv <- downloadHandler(
    filename = function() {
      "gcs_results.csv"
    },
    content = function(file_geocoded) {
      write_csv(data_df(), file_geocoded, na = "")
      rv$download_flag <- rv$download_flag + 1
    }
  )
  
  output$download_file_xlsx <- downloadHandler(
    filename = function() {
      "gcs_results.xlsx"
    },
    content = function(file_geocoded) {
      write.xlsx(data_df(), file_geocoded)
      rv$download_flag <- rv$download_flag + 1
    }
  )
  
  output$table <- DT::renderDataTable(datatable({
    
    ## call function to create specified data table
    data_df()
      
    },
    filter="none",
    ## table options: https://shiny.rstudio.com/articles/datatables.html
    options = list(
      pageLength = 10,       ## show only X rows/page; https://datatables.net/reference/option/pageLength
      lengthMenu = c(10, 20, 25, 50), ## choices of pageLength to display
      scrollX = TRUE,        ## allows horizontal scrolling; https://datatables.net/reference/option/scrollX
      dom ="ltpi"
    )
    )
  )
  
  output$downloadLookup <- downloadHandler(
    filename = "GCS_Lookup_Table.xlsx",
    content = function(file) {
      file.copy("data/GCS_Lookup_Table.xlsx", file)
    }
  )
  
  output$downloadGuide <- downloadHandler(
    filename = "GCS_User_Guide.pdf",
    content = function(file) {
      file.copy("data/GCS_User_Guide.pdf", file)
    }
  )
}

shinyApp(ui = ui, server = server)
