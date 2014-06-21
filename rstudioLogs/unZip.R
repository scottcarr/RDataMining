PKGDIR = "/Volumes/HFS/cran/cran/src/contrib/" #an existing dir with the packages
EXDIR = "/Volumes/HFS/untared/"
DLCSV = "downloads.csv" 

getTopN <- function(inFile, N) {
    df <- read.csv(inFile, row.names=1)
    topN <- rownames(df)[1:N]
}

unzipTopN <- function(inFile, N) {
    topN <- getTopN(inFile,N)
    for (i in 1:N) {
        tarball <- findPackageTarBall(topN[i])
        print(paste0("Untaring: ", tarball)) 
        untar(tarball, compressed="gzip", tar="/usr/bin/tar", exdir=EXDIR)
    }
}

findPackageTarBall <- function(packageName) {
    files <- dir(PKGDIR)
    substrfiles <- substr(files, 1, nchar(packageName) + 1)
    #print(substrfiles)
    #hits <- grep(paste0(packageName, "_"), files, fixed=TRUE)
    hits <- grep(paste0(packageName, "_"), substrfiles)
    if (length(hits) != 1) {
        print("hits:")
        print(files[hits])
        return ("AMBIGUOUS PACKAGE NAME")
    }
    paste0(PKGDIR, files[hits])
}

run2 <- function() {
    source("packageMining.r")
    unzipTopN(DLCSV, 10)
}

#unzipTopN("downloads.csv", 10)
