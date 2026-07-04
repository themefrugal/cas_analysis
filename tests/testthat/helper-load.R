library(data.table)
library(stringr)
library(tidyr)
library(rjson)
library(DBI)
library(RSQLite)
library(httr)

find_repo_root <- function(path = getwd()) {
    path <- normalizePath(path, winslash = '/', mustWork = TRUE)
    repeat {
        if (file.exists(file.path(path, 'app', 'cas_regex.R'))) return(path)
        parent <- dirname(path)
        if (identical(parent, path)) stop('Could not locate repository root.')
        path <- parent
    }
}

repo_root <- find_repo_root()
options(cas_analysis.app_dir = file.path(repo_root, 'app'))
source(file.path(repo_root, 'app', 'cas_regex.R'))
source(file.path(repo_root, 'app', 'cas_parser.R'))
source(file.path(repo_root, 'app', 'market_data.R'))
source(file.path(repo_root, 'app', 'analytics.R'))
