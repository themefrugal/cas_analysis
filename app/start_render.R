port <- as.integer(Sys.getenv("PORT", "10000"))
if (is.na(port)) {
    stop("PORT must be an integer")
}

app_dir <- Sys.getenv("SHINY_APP_DIR", "/srv/cas/app")
if (!dir.exists(app_dir)) {
    stop("Shiny app directory does not exist: ", app_dir)
}

setwd(app_dir)

renv_activate <- file.path(app_dir, "renv", "activate.R")
if (file.exists(renv_activate)) {
    source(renv_activate)
}

message("Starting CAS Shiny app on 0.0.0.0:", port)
shiny::runApp(appDir = app_dir, host = "0.0.0.0", port = port)
