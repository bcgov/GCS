library(shiny)
library(magrittr)
library(tidyverse)
library(data.table)
library(grid)
library(writexl)
library(shinyjs)
options(stringsAsFactors = FALSE)
options(shiny.maxRequestSize=100*1024^2)

# Define UI for app that draws a histogram ----
ui <- navbarPage("GeoCoding Self-Service", 

      tabPanel("GeoCoding",
               useShinyjs(),
               h2("GeoCoding Self-Service (GCS) Application"),
               br(),
               
               h3("How to use the geocoding application"),
               br(),
               p("1 - Select a file to geocode by clicking the Browse button below. The file should be in .csv (comma-separated values) format
                 and include at least one column with a header listing the postal codes to be geocoded."),
               p("2 - Select the field from your file containing the postal codes to be geocoded"),
               p("3 - Select the GCS version to use. A new version is released every quarter following the naming convention GCS_YYYYMM."),
               p("4 - Select the GCS fields you wnat your postal codes to be geocoded to."),
               p("5 - Click Geocode Input File to begin the process. It should take a few seconds."),
               p("6 - When the geocoding is done, you can click the Download Results button to retrieve a .csv copy of your geocoded postal codes."),
               
               br(),
               
               fileInput(inputId = "geo_input", 
                         label = "File to geocode (.csv):",
                         accept = c(".csv")),
               
               selectInput(inputId = "upload_field",
                           label = "Field containing postal codes:",
                           choices = c("")
                           ),
               
               br(),
               
               selectInput(inputId = "gcs_version",
                           label = "GCS version to use:",
                           choice = str_replace(list.files("data"), ".csv", "")
                           ),
               
               checkboxGroupInput(inputId = "gcs_fields",
                                  label = "GCS fields to return:",
                                  choices = c(" ")
                                  ),
               
               br(),
               
               actionButton(inputId = "geo_button",
                            label = "Geocode Input File"
                            ),
               
               textOutput(outputId = "geo_progress"),
               
               br(),
               
               downloadButton(outputId = "download_button", label = "Download Results")
               
               
               ),
      
      tabPanel("User Guide", 
               h2("GeoCoding Self-Service User Guide"),
               p("Click the download button below to get a PDF copy of the GeoCoding Self-Service User Guide. You can also
                 read it below."),
               downloadButton(outputId = "downloadGuide", "Download"),
               includeHTML("GCS_DefinitionsWeb.htm")
               ),
      
      tabPanel("Lookup Tables", 
               h2("GeoCoding Self-Service Fields Lookup Tables"),
               p("The lookup tables can be used to relate field value returned by the GeoCoding Self-Service to a region's full name. 
                 Click the download button below to get a copy of the tables."),
               downloadButton(outputId = "downloadLookup", "Download")
               )

      
    )
    

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  observeEvent(input$geo_input, {
    head <- colnames(read.csv(input$geo_input$datapath))
    updateSelectInput(session, "upload_field", choices = list(head))
  })
  
  observeEvent(input$gcs_version, {
    head <- colnames(read.csv(paste0("data/", input$gcs_version, ".csv"), nrows = 10) %>% select(-POSTALCODE))
    updateCheckboxGroupInput(session, "gcs_fields", choices = head)
  })
  
  observeEvent(input$geo_button, {
    postal_field <- input$upload_field
    
    in_postal <- read.csv(input$geo_input$datapath) %>% select(postal_field) %>% rename(POSTALCODE = postal_field)

    gcs_data <- read.csv(paste0("data/", input$gcs_version, ".csv")) %>%
      select(POSTALCODE, input$gcs_fields)
    
    join_data <- in_postal %>%
      left_join(gcs_data, by = "POSTALCODE")
    
    output$geo_progress <- renderText("Done")
    
    output$download_button <- downloadHandler(
      filename = function() {
        "gcs_results.csv"
      },
      content = function(file_geocoded) {
        write.csv(join_data, file_geocoded, row.names = FALSE)
      }
    )

    enable("download_button")
  })
  
  output$geo_progress <- renderText("Click the button above to begin geocoding.")
  
  output$downloadLookup <- downloadHandler(
    filename = function() {
      "GCS_Lookup_Table.xlsx"
    },
    content = function(geo_file) {
      file.copy("GCS_Lookup_Table_201907.xlsx", geo_file)
    }
  )
  
  output$downloadGuide <- downloadHandler(
    filename = function() {
      "GCS_User_Guide.pdf"
    },
    content = function(file) {
      file.copy("GCS_Definitions_20190716.pdf", file)
    }
  )
  
  disable("download_button")
}


shinyApp(ui = ui, server = server)
