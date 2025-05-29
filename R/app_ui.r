#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import DT
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      titlePanel("UpSet Plot Generator"),
      
      sidebarLayout(
        sidebarPanel(
          fileInput("file1", "Upload CSV File",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          
          checkboxInput("header", "Header", TRUE),
          
          actionButton("load_columns", "Load Columns from File", class = "btn-info"),
          
          br(), br(),
          
          selectInput("id_column", "Select Identifier Column:", choices = NULL),
          
          selectInput("group_column", "Select Grouping Column:", choices = NULL),
          
          checkboxInput("remove_duplicates", "Remove Duplicate Pairs", TRUE),
          
          textInput("plot_title", "Plot Title:", "UpSet Plot"),
          
          selectInput("plot_color", "Plot Color:",
                      choices = c("darkblue", "darkred", "darkgreen", "purple", "orange", "black"),
                      selected = "darkblue"),
          
          actionButton("generate", "Generate Plot", class = "btn-primary"),
          
          br(), br(),
          
          downloadButton("download_zip", "Download All Intersections", class = "btn-success"),
          
          br(), br(),
          
          downloadButton("download_binary", "Download Binary Matrix", class = "btn-primary")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Plot", plotOutput("upset_plot", height = "600px")),
            tabPanel("Intersection Information",
                     DT::DTOutput("intersection_table")),
            tabPanel("Data Preview",
                     DT::DTOutput("data_preview")),
            tabPanel("Binary Matrix",
                     DT::DTOutput("binary_matrix_preview"))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "UpSet Plot Generator"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}