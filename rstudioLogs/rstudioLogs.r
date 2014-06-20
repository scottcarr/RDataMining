library("R.utils");
library("wordcloud");
LOGDIR="~/RDataMining/rstudioLogs/logs" # this directory should exist

getMissingLogs <- function() {
    #see http://cran-logs.rstudio.com/
    #setwd(LOGDIR)

    # Here's an easy way to get all the URLs in R
    start <- as.Date('2014-06-01')
    today <- as.Date('2014-06-16')

    all_days <- seq(start, today, by = 'day')

    year <- as.POSIXlt(all_days)$year + 1900
    existing_files <- tools::file_path_sans_ext(dir(LOGDIR), TRUE)
    existing_days <- as.Date(existing_files)
    missing_days_raw <- setdiff(all_days, existing_days)

    # it loses the date format for some strange reason
    as.Date(missing_days_raw, origin="1970-1-1")
}

syncLogs <- function(missing_days) {

    if (length(missing_days) > 0) {
        #setwd(LOGDIR)

        year <- as.POSIXlt(missing_days)$year + 1900
        urls <- paste0('http://cran-logs.rstudio.com/', year, '/', missing_days, '.csv.gz')
        destFiles <- paste0(LOGDIR, "/", missing_days, '.csv.gz')

        for (i in 1:length(urls)) {
            download.file(urls[i], destFiles[i], "wget")
            gunzip(destFiles[i])
        }
    }
}

countPackages <- function() {
    #setwd(LOGDIR)
    logs <- dir(LOGDIR)
    for (i in 1:length(logs)) {
        df <- read.csv(paste0(LOGDIR, "/", logs[i]))
        t <- table(df[["package"]])
        if (i == 1) {
            counts <- t
        } else {
            counts <- mergeTables(counts, t)
        }
    }
    counts <- sort(counts, decreasing=TRUE)
    df <- as.data.frame(counts)
    colnames(df) <- c("downloads")
    write.csv(df, file="downloads.csv")
    return (df)
}

mergeTables <- function(a, b) {
    n <- intersect(names(a), names(b))
    c(a[!(names(a) %in% n)], b[!(names(b) %in% n)], a[n] + b[n])
}


plotResults <- function() {
    jpeg("wordcloud.jpg")
    #d <- read.csv("countsSorted.csv")
    d <- read.csv("downloads.csv", row.names=1)
    wordcloud(rownames(d), d[["downloads"]], max.words=100)
    #wordcloud(d[["X.1"]], d[["x"]], max.words=100)
    dev.off()
}

hbarPlot <- function() {
    jpeg("hbar.jpg")
    d <- read.csv("downloads.csv", row.names=1)
    par(las=2) # make label text perpendicular to axis
    range <- 25:1
    #barplot(d[range, 3], horiz=TRUE, names.arg=d[range, 2], cex.names=0.7)
    barplot(d[["downloads"]][range], horiz=TRUE, names.arg=rownames(d)[range], cex.names=0.7)
    dev.off()
}

run <- function() {

    setwd("/Users/scott/RDataMining/rstudioLogs")
    source("rstudioLogs.r")
    #countPackages()
    #plotResults()
    hbarPlot()
}









