#' Run the Shiny Application
#'
#' @param onStart A function that will be called before the app is actually run.
#'     This is typically used for shinyApp().
#' @param options Named options that should be passed to the `shinyApp()` call.
#' @param enableBookmarking Can be "url" or "server", the same as in shinyApp()
#' @param ... arguments to pass to golem_opts.
#'     See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking
    ),
    golem_opts = list(...)
  )
}