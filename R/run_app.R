#' Run the qry4ms Application
#' Online version: https://plyush1993.shinyapps.io/qry4ms/
#'
#' @export
run_qry4ms <- function(...) {
  options(shiny.maxRequestSize = 1 * 1024^3)
  shiny::addResourcePath("www", system.file("www", package = "qry4ms"))
  shiny::shinyApp(ui = app_ui(), server = app_server, ...)
}
