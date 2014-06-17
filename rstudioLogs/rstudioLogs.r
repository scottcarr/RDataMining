#see http://cran-logs.rstudio.com/
library("R.utils");
library("wordcloud");
LOGDIR="~/RDataMining/rstudioLogs/logs" # this directory should exist

getMissingLogs <- function() {
    setwd(LOGDIR)

    # Here's an easy way to get all the URLs in R
    start <- as.Date('2014-06-01')
    today <- as.Date('2014-06-16')

    all_days <- seq(start, today, by = 'day')

    year <- as.POSIXlt(all_days)$year + 1900
    existing_files <- tools::file_path_sans_ext(dir(), TRUE)
    existing_days <- as.Date(existing_files)
    missing_days_raw <- setdiff(all_days, existing_days)

    # it loses the date format for some strange reason
    as.Date(missing_days_raw, origin="1970-1-1")
}

syncLogs <- function(missing_days) {

    if (length(missing_days) > 0) {
        setwd(LOGDIR)

        year <- as.POSIXlt(missing_days)$year + 1900
        urls <- paste0('http://cran-logs.rstudio.com/', year, '/', missing_days, '.csv.gz')
        destFiles <- paste0(missing_days, '.csv.gz')

        for (i in 1:length(urls)) {
            download.file(urls[i], destFiles[i], "wget")
            gunzip(destFiles[i])
        }
    }
}

countPackages <- function() {
    setwd(LOGDIR)
    logs <- dir()
    for (i in 1:length(logs)) {
        df <- read.csv(logs[i])
        t <- table(df[["package"]])
        if (i == 1) {
            counts <- t
        } else {
            counts <- mergeTables(counts, t)
        }
    }
    counts <- countPackages()
    sort(counts, decreasing=TRUE)
}

mergeTables <- function(a, b) {
    n <- intersect(names(a), names(b))
    c(a[!(names(a) %in% n)], b[!(names(b) %in% n)], a[n] + b[n])
}

analyze <- function() {
    counts <- countPackages()
    wordcloud(names(counts), margin(counts,1), max.words=100)
}

run <- function() {

    setwd("/Users/scott/RDataMining/rstudioLogs")
    source("rstudioLogs.r")
    analyze()
}









