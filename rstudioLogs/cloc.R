cloc <- function(file) {
    nHeader <- 9
    nFooter <- 3
    output <- system(paste0("cloc ", file), intern=TRUE)
    importantRows <- output[seq(nHeader,length(output)-nFooter)]
    splits <- Map(function(x){strsplit(x, "\\s\\s+")}, importantRows)
    langs <- Map(function(x){x[[1]][1]}, splits)
    files <- Map(function(x){x[[1]][2]}, splits)
    blank <- Map(function(x){x[[1]][3]}, splits)
    comment <- Map(function(x){x[[1]][4]}, splits)
    code <- Map(function(x){x[[1]][5]}, splits)
    langs <- unlist(langs)
    names(files) <- langs
    names(blank) <- langs
    names(code) <- langs
    names(comment) <- langs
    m <- cbind(files, blank, comment, code)
    df <- data.frame(m)
    return(df)
}

test <- function() {
    cloc("/Volumes/HFS/untared/Rcpp")
}

#test()
