#' Launch the Cusp Catastrophe Shiny application
#' @export
run_app <- function(...) {
  app_dir <- system.file("app", package = "CuspCatastrophe")
  if (app_dir == "") {
    stop("Could not find app directory. Try reinstalling the package.")
  }
  shiny::runApp(app_dir, ...)
}
