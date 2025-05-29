#' Generate UpSet Plot from data
#'
#' @param data_df A data frame containing the data
#' @param id_col The name of the identifier column
#' @param group_col The name of the grouping column
#' @param plot_title The title for the plot
#' @param color The color for the plot
#' @param remove_duplicates Whether to remove duplicate pairs
#' @param export_dir Directory to export intersection sets
#'
#' @return A list containing the plot and export information
#' @import dplyr
#' @import tidyr
#' @import ComplexHeatmap
#' @import grid
#' @import readr
#' @export
make_upset_plot_from_data <- function(data_df, id_col, group_col, plot_title = "UpSet Plot",
                                      color = "darkblue", remove_duplicates = TRUE,
                                      export_dir = "intersection_sets") {
  
  # Prepare the data frame for processing
  bcr_df <- data_df %>%
    select(all_of(c(id_col, group_col))) %>%
    rename(junction_aa = !!id_col, Dataset = !!group_col)
  
  # Remove duplicates if requested
  if (remove_duplicates) {
    bcr_df <- bcr_df %>% distinct()
  }
  
  # Prepare the matrix for UpSet
  mat <- bcr_df %>%
    distinct(junction_aa, Dataset) %>%
    mutate(value = 1) %>%
    pivot_wider(names_from = Dataset, values_from = value, values_fill = list(value = 0))
  
  raw_junctions <- mat$junction_aa
  clone_matrix <- as.matrix(mat[, -1, drop = FALSE])
  rownames(clone_matrix) <- raw_junctions
  clone_matrix <- apply(clone_matrix, 2, as.numeric)
  
  # Create binary code for each junction_aa
  binary_codes <- apply(clone_matrix, 1, function(row) paste(row, collapse = ""))
  
  # Create a table with junction_aa and binary indicators
  binary_table <- data.frame(
    junction_aa = raw_junctions,
    clone_matrix,
    code = binary_codes,
    stringsAsFactors = FALSE
  )
  
  # Add a Group column based on patterns
  binary_table <- binary_table %>%
    mutate(Group = case_when(
      # These are examples - adjust based on your needs
      rowSums(clone_matrix) == 1 ~ paste0(names(clone_matrix)[max.col(clone_matrix)], "_Only"),
      rowSums(clone_matrix) == ncol(clone_matrix) ~ "All_Sets",
      TRUE ~ "Multiple_Sets"
    ))
  
  # Add a placeholder Clone_ID column
  binary_table <- binary_table %>%
    mutate(Clone_ID = paste0("Clone", row_number()),
           Cluster = NA)
  
  # Save the binary table
  write.csv(binary_table, file.path(export_dir, "binary_matrix.csv"), row.names = FALSE)
  
  # Create the combination matrix for UpSet
  comb_mat <- make_comb_mat(clone_matrix)
  
  # UpSet plot with annotations
  upset_plot <- UpSet(
    comb_mat,
    set_order = order(set_size(comb_mat), decreasing = TRUE),
    comb_col = color,
    column_title = plot_title,
    top_annotation = HeatmapAnnotation(
      "Count" = anno_barplot(comb_size(comb_mat), border = FALSE),
      "Values" = anno_text(as.character(comb_size(comb_mat)),
                           location = 0.5, just = "center",
                           gp = gpar(fontsize = 10))
    ),
    left_annotation = rowAnnotation(
      "Set Size" = anno_barplot(set_size(comb_mat), border = FALSE,
                                gp = gpar(fill = "gray")),
      "Count" = anno_text(as.character(set_size(comb_mat)),
                          location = 0.5, just = "left",
                          gp = gpar(fontsize = 10))
    )
  )
  
  # Export intersection sets
  # Create directory if it doesn't exist
  if (!dir.exists(export_dir)) {
    dir.create(export_dir, recursive = TRUE)
  }
  
  # Get set names (column names from the original matrix)
  set_names <- colnames(clone_matrix)
  
  # Export individual sets first
  for (set_name in set_names) {
    # Get junctions in this set
    set_junctions <- raw_junctions[clone_matrix[, set_name] == 1]
    
    # Filter data for these junctions
    set_df <- data_df %>%
      filter(!!sym(id_col) %in% set_junctions)
    
    # Export to CSV
    output_set_file <- file.path(export_dir, paste0("set_", set_name, ".csv"))
    write.csv(set_df, output_set_file, row.names = FALSE)
  }
  
  # Now create all possible combinations of sets
  n_sets <- length(set_names)
  
  # Helper function to generate all possible combinations
  get_all_combinations <- function() {
    all_combinations <- list()
    count <- 0
    
    # Generate combinations of sets (from 2 to n_sets)
    for (k in 2:n_sets) {
      # Get all combinations of size k
      combos <- combn(set_names, k, simplify = FALSE)
      
      for (combo in combos) {
        count <- count + 1
        all_combinations[[count]] <- combo
      }
    }
    
    return(all_combinations)
  }
  
  # Function to export specific intersection
  export_specific_intersection <- function(included_sets, intersection_name, i) {
    # Find junctions in this specific intersection
    in_intersection <- rep(TRUE, nrow(clone_matrix))
    
    # Must be in all included sets
    for (set in included_sets) {
      in_intersection <- in_intersection & (clone_matrix[, set] == 1)
    }
    
    # Must NOT be in any excluded sets
    excluded_sets <- setdiff(set_names, included_sets)
    for (set in excluded_sets) {
      in_intersection <- in_intersection & (clone_matrix[, set] == 0)
    }
    
    # Get the junction_aa values for this intersection
    intersection_junctions <- raw_junctions[in_intersection]
    
    # Skip empty intersections
    if (length(intersection_junctions) == 0) {
      return(NULL)
    }
    
    # Create a data frame with these junctions
    intersection_df <- data_df %>%
      filter(!!sym(id_col) %in% intersection_junctions)
    
    # Export to CSV
    output_intersection_file <- file.path(export_dir, paste0("intersection_", i, "_", intersection_name, ".csv"))
    write.csv(intersection_df, output_intersection_file, row.names = FALSE)
    
    return(list(
      name = intersection_name,
      count = length(intersection_junctions),
      file = output_intersection_file
    ))
  }
  
  # Export pairwise intersections
  intersection_info <- list()
  
  # Export each pairwise intersection
  if (n_sets > 1) {
    for (i in 1:(n_sets-1)) {
      for (j in (i+1):n_sets) {
        set_i <- set_names[i]
        set_j <- set_names[j]
        
        # Both sets
        intersection_name <- paste(set_i, set_j, sep = "_AND_")
        info <- export_specific_intersection(c(set_i, set_j), intersection_name, paste0(i, "_", j))
        if (!is.null(info)) {
          intersection_info[[length(intersection_info) + 1]] <- info
        }
      }
    }
  }
  
  # If there are more than 2 sets, generate all combinations
  if (n_sets > 2) {
    all_combinations <- get_all_combinations()
    
    # For each combination
    for (i in seq_along(all_combinations)) {
      included_sets <- all_combinations[[i]]
      
      # Only process if not already a pairwise combo
      if (length(included_sets) > 2) {
        # Create a name for this intersection
        intersection_name <- paste(included_sets, collapse = "_AND_")
        
        info <- export_specific_intersection(included_sets, intersection_name, i)
        if (!is.null(info)) {
          intersection_info[[length(intersection_info) + 1]] <- info
        }
      }
    }
  }
  
  return(list(
    plot = upset_plot,
    export_dir = export_dir,
    intersection_info = intersection_info,
    binary_table = binary_table
  ))
}

#' Create ZIP file of all intersection files
#'
#' @param export_dir Directory containing files to zip
#'
#' @return Path to the created zip file
#' @import zip
#' @export
create_zip_file <- function(export_dir) {
  zip_file <- paste0(export_dir, ".zip")
  files_to_zip <- list.files(export_dir, full.names = TRUE)
  
  # Create the zip file
  zip::zip(zip_file, files_to_zip, mode = "cherry-pick")
  
  return(zip_file)
}