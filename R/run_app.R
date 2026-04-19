#' Run the qry4ms Application
#'
#' @importFrom crayon blue red bold
#' @export
run_qry4ms <- function(...) {

  cat("\n")
  cat(crayon::blue("             +--------------------+\n"))
  app_name <- paste0(
    crayon::blue(crayon::bold("      qry4ms      "))
  )
  cat(crayon::blue("             | "), app_name, crayon::blue(" |\n"), sep = "")
  cat(crayon::blue("             +--------------------+\n"))
  cat("\n")
  cat(crayon::red(crayon::bold("Making an MS query and basic MS-related calculations\n")))
  cat("\n")

  flush.console()

  old_opts <- options(shiny.maxRequestSize = 1 * 1024^3)

    on.exit({
    options(old_opts)
    gc()
    }, add = TRUE)

  shiny::addResourcePath("www", system.file("www", package = "qry4ms"))

  app <- shiny::shinyApp(ui = app_ui(), server = app_server)
  shiny::runApp(app, ...)
}
