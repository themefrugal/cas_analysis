port <- as.integer(Sys.getenv("PORT", "10000"))
if (is.na(port)) {
    stop("PORT must be an integer")
}

app_dir <- Sys.getenv("SHINY_APP_DIR", "/srv/cas/app")
if (!dir.exists(app_dir)) {
    stop("Shiny app directory does not exist: ", app_dir)
}

setwd(app_dir)

if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The shiny package is not installed in the runtime R library")
}

message("Starting CAS Shiny app on 0.0.0.0:", port)
shiny::runApp(appDir = app_dir, host = "0.0.0.0", port = port)
