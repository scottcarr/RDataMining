source("unZip.R")

cloc <- function(file) {
    nHeader <- 4
    nFooter <- 3
    output <- system(paste0("cloc --force-lang=R,r --csv ", file), intern=TRUE)
    output <- output[-(1:nHeader)]
    con <- textConnection(output)
    d <- read.csv(con)
    d <- d[-length(d)] # the lost column is some weird string
    languages <- d[["language"]]
    #print(languages)
    df <- data.frame(t(as.numeric(d[["code"]])), basename(file))
    colnames(df) <- c(as.character(languages), "package")
    #row.names(df) <- basename(file)
    return(df)
}

clocTopN <- function() {
    topN <- getTopN(DLCSV, 5)
    files <- paste0(EXDIR, topN)
    clocs <- Map(cloc, files)
    merged <- Reduce(function(x, y){merge(x, y, all=TRUE)}, clocs)
    return (merged)
}

calcStats <- function(merged) {
    cns <- colnames(merged)
    keeps <- cns[which(cns != "package")]
    print(keeps)
    m <- data.frame(merged, total=rowSums(merged[,keeps], na.rm=TRUE))
    return (m)
}

test1 <- function() {
    c <- cloc("/Volumes/HFS/untared/Rcpp")
}

test2 <- function() {
    m <- clocTopN()
    calcStats(m)
}

#test2()
