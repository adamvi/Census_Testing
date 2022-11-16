
freadZIP =
  function(
    zipfile,
    all.to.char = FALSE,
    sep = NULL,
    keep = NULL
  ) {
    if (is.null(sep)) sep <- "auto"
    zipfile <- normalizePath(zipfile)
    fname <- gsub(".zip", "", basename(zipfile))
    tmp.dir <- getwd()
    setwd(tempdir())
    system(paste0("unzip '", zipfile, "'"))

    if (all.to.char) {
        TMP <- fread(fname, sep = sep, nrows = 10L)
        TMP <- fread(fname, sep = sep, colClasses = rep("character", ncol(TMP)))
    } else {
        TMP <- fread(fname, sep = sep)
    }

    unlink(fname)
    setwd(tmp.dir)

    if (!is.null(keep)) {
        TMP[, ..keep]
    } else {
        TMP
    }
}
