# R/app_server.R
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract
#' @importFrom dplyr mutate
#' @importFrom stringdist stringdistmatrix
#' @importFrom readr read_csv
#' @import shiny
#' @import dplyr
#' @import tidyr
#' @import ComplexHeatmap
#' @import ggplot2
#' @import grid
#' @import zip
#' @import DT
#' @noRd
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.

app_server <- function(input, output, session) {
  # Set maximum upload size
  options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB

  # Load data
  data <- reactive({
    req(input$file1)

    tryCatch({
      df <- read_csv(input$file1$datapath, col_names = input$header)
      return(df)
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      return(NULL)
    })
  })

  # When load columns button is clicked, populate the column dropdowns
  observeEvent(input$load_columns, {
    req(input$file1)
    tryCatch({
      df <- data()
      if (is.null(df)) return()

      cols <- names(df)

      # Update column choices
      updateSelectInput(session, "id_column", choices = cols)
      updateSelectInput(session, "group_column", choices = cols)

      # Try to make an intelligent default selection if possible
      if ("junction_aa" %in% cols) {
        updateSelectInput(session, "id_column", selected = "junction_aa")
      }

      if ("Dataset" %in% cols) {
        updateSelectInput(session, "group_column", selected = "Dataset")
      }

      showNotification("Columns loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading columns:", e$message), type = "error")
    })
  })

  # Data preview
  output$data_preview <- DT::renderDT({
    req(data())
    head(data(), 10)
  })

  # Generate plot
  plot_result <- eventReactive(input$generate, {
    req(data(), input$id_column, input$group_column)

    withProgress(message = 'Generating UpSet plot...', value = 0, {
      # Create temporary directory for exports
      export_dir <- tempfile("upset_intersections")
      dir.create(export_dir, recursive = TRUE)

      # Generate the plot
      result <- make_upset_plot_from_data(
        data_df = data(),
        id_col = input$id_column,
        group_col = input$group_column,
        plot_title = input$plot_title,
        color = input$plot_color,
        remove_duplicates = input$remove_duplicates,
        export_dir = export_dir
      )

      # Create zip file
      zip_file <- create_zip_file(export_dir)
      result$zip_file <- zip_file

      return(result)
    })
  })

  # Display plot
  output$upset_plot <- renderPlot({
    req(plot_result())
    grid.newpage()
    draw(plot_result()$plot)
  })

  # Display intersection information
  output$intersection_table <- DT::renderDT({
    req(plot_result())
    intersection_info <- plot_result()$intersection_info

    # Create a data frame from the intersection information
    if (length(intersection_info) > 0) {
      data.frame(
        Intersection = sapply(intersection_info, function(x) gsub("_AND_", " ∩ ", x$name)),
        Count = sapply(intersection_info, function(x) x$count),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        Intersection = character(0),
        Count = integer(0),
        stringsAsFactors = FALSE
      )
    }
  })

  # Display binary matrix preview
  output$binary_matrix_preview <- DT::renderDT({
    req(plot_result())
    req(plot_result()$binary_table)

    plot_result()$binary_table
  })

  # Download handler for ZIP file
  output$download_zip <- downloadHandler(
    filename = function() {
      paste("upset_intersections_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      req(plot_result()$zip_file)
      file.copy(plot_result()$zip_file, file)
    }
  )

  # Download handler for binary matrix
  output$download_binary <- downloadHandler(
    filename = function() {
      paste("binary_matrix_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(plot_result()$binary_table)
      write.csv(plot_result()$binary_table, file, row.names = FALSE)
    }
  )
}
