run_app <- function() {
  app_dir <- system.file("literacyrates-app", package = "IndianLiteracyRates")
  shiny::runApp(app_dir, display.mode = "normal")
}
