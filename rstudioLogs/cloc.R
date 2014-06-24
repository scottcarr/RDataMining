source("unZip.R")
library("ggplot2")
library("reshape2")

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

clocTopN <- function(N) {
    topN <- getTopN(DLCSV, N)
    files <- paste0(EXDIR, topN)
    clocs <- Map(cloc, files)
    merged <- Reduce(function(x, y){merge(x, y, all=TRUE)}, clocs)
    merged[is.na(merged)] <- 0
    return (merged)
}

writeCloc <- function(N, filename) {
    m <- clocTopN(N)
    write.csv(m, filename)
}

calcStats <- function(merged) {
    cns <- colnames(merged)
    keeps <- cns[which(cns != "package")]
    print(keeps)
    m <- data.frame(merged, total=rowSums(merged[,keeps], na.rm=TRUE))
    return (m)
}

#stackedBarPlot <- function(filename) {
#    par(las=2)
#    x <- read.csv(filename, row.names=1)
#    langs <- c("R", "C", "C..", "Java")
#    displangs <- c("R", "C", "C++", "Java")
#    pkgs <- x[["package"]]
#    colors <- c("red", "blue", "green", "yellow")
#    y <- t(x[langs])
#    #barplot(t(x[langs]), names.arg=pkgs, col=colors, legend=displangs, horiz=TRUE, cex.names=0.8)
#    #barplot(t(x[langs]), names.arg=pkgs, col=colors, horiz=TRUE, cex.names=0.8)
#    #legend("center", col=colors)
#    #qplot(langs, data=y, geom="bar", stat="identity")
#
#}

stackedBarPlot <- function(filename) {
    x <- read.csv(filename, row.names=1)
    langs <- c("R", "C", "C..", "Java")
    displangs <- c("R", "C", "C++", "Java")
    y <- x[c("package", langs)] # get only the columns we want
    y[langs] <- y[langs] /1000 # convert to KLOC
    df <- data.frame(y)
    colnames(df) <- c("package", displangs)
    m <- melt(df, id.vars="package")
    colnames(m) <- c("package", "Language", "KLOC")
    #print(m)
    #qplot(m, aes(x=package, y=value, fill=variable)) + geom_bar(stat="identity")
    ggplot(m, aes(x = package, y = KLOC, fill = Language)) + 
      geom_bar(stat = "identity") +
      ylab("KLOC") + 
      theme(axis.text.x = element_text(angle=-90, hjust=0))
    ggsave(file="pkg_comp.png")
}

test1 <- function() {
    c <- cloc("/Volumes/HFS/untared/Rcpp")
}

test2 <- function() {
    clocs <- read.csv("clocs.csv", row.names=1)
    #m <- clocTopN()
    #calcStats(m)
}

#writeCloc(5, "clocs.csv")
#test2()
N <- 25
#unzipTopN("downloads.csv", N)
writeCloc(N, "clocs.csv")
stackedBarPlot("clocs.csv")
