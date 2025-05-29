#' The application configuration
#'
#' @param ... Additional parameters passed to `golem::get_golem_config()`
#'
#' @return The configuration of the application
#' @export
get_golem_config <- function(...) {
  golem::get_golem_config(...)
}

#' Access files in the current app
#'
#' @param ... Path to the file, not starting with /
#'
#' @return The full path to the file
#' @export
app_sys <- function(...) {
  system.file(..., package = "upsetplotgen")
}

#' Read App Config
#'
#' @param value The value to retrieve from the config
#' @param config The name of the configuration to use
#' @param use_parent If TRUE, the parent config will be used if the value is not found
#'
#' @return The value from the config
#' @export
get_golem_config <- function(value = NULL, config = Sys.getenv("R_CONFIG_ACTIVE", "default"), use_parent = TRUE) {
  config::get(value = value, config = config, file = app_sys("golem-config.yml"), use_parent = use_parent)
}
