#' Load GEO Dataset.
#'
#' \code{loadGEO} returns the file with serialized ExpressionSet using
#'     ProtoBuf, parsed from data downloaded from GEO by identifier.
#'
#' @param name String, containing GEO identifier of the dataset.
#'     It should start with 'GSE' or 'GDS' and can include exact GPL
#'     to annotate dataset, separated with dash ('-') from the identifier.
#'
#' @param type Type of the dataset: 'GSE' or 'GDS'. If not specified,
#'     the function will take first three letters
#'     of \code{name} variable as type.
#'
#' @return File with ProtoBuf-serialized ExpressionSet-s
#'     that were downloaded by this identifier.
#'     For GSE-datasets there can be multiple annotations, so in file will be a
#'     list mapping name with GPL to ExpressionSet.
#'
#' @examples
#' \dontrun{
#'     loadGEO("GSE27112")
#'     loadGEO("GDS4922")
#' }
#'
#' @importFrom utils tail
#' @import Biobase
#' @import GEOquery
#' @keywords internal
loadGEO <- function(name, type = NA) {
    cacheDir <- getPhantasusConf("cache_folders")$geo_path

    if (!dir.exists(cacheDir)) {
        dir.create(cacheDir)
    }

    mirrorPath <- getPhantasusConf("geo_mirrors")


    geoDir <- getGEODir(name, cacheDir)
    binaryName <- paste0(name, '.bin')
    filePath <- file.path(geoDir, binaryName)
    urls <- c()


    ess <- getES(name, type, destdir = cacheDir, mirrorPath = mirrorPath)

    files <- list()
    assign("es", utils::tail(ess, n = 1)[[1]], envir = parent.frame())

    current_rda <- file.path(geoDir, paste0(name, ".rda"))
    bin_is_valid <- checkBinValidity(filePath, file.info(current_rda)$ctime)
    if (!bin_is_valid) {
        for (i in seq_along(ess)) {
            seriesName <- if (!grepl(pattern = "-", name) && length(ess) > 1)
                paste0(name, "-", annotation(ess[[i]])) else name
            files[[seriesName]] <- writeToList(ess[[i]])
        }
        tempBinFile <- tempfile(paste0(binaryName, ".binsave"), tmpdir = geoDir)
        protolite::serialize_pb(list(layout_version = as.raw(PROTOBUF_LAYOUT_VERSION), ess = files), tempBinFile)
        message('Saved binary file: ', tempBinFile)
        file.rename(tempBinFile, filePath)
    }


    urls <- c(urls, paste0("/geo/",unlist(strsplit(filePath, cacheDir, fixed = TRUE))[2]))
    jsonlite::toJSON(urls)
}

#' Load ExpressionSet from GEO Datasets
#'
#'\code{getGDS} return the ExpressionSet object corresponding
#'     to GEO Dataset identifier.
#'
#' @param name String, containing GEO identifier of the dataset.
#'     It should start with 'GSE' or 'GDS' and can include exact GPL
#'     to annotate dataset, separated with dash ('-') from the identifier.
#'
#' @param destdir Directory for caching loaded Series and GPL
#'     files from GEO database.
#'
#' @param mirrorPath URL string which specifies the source of matrices.
#'
#' @return ExpressionSet object wrapped in list, that was available by given
#'     in \code{name} variable GEO identifier.
#'
#' @examples
#' \dontrun{
#'    getGDS('GDS4922', destdir = tempdir(), mirrorPath = "https://ftp.ncbi.nlm.nih.gov")
#' }
#' @export
getGDS <- function(name, destdir = getPhantasusConf("cache_folders")$geo_path,
                   mirrorPath = getPhantasusConf("geo_mirrors")) {
    stub <- gsub("\\d{1,3}$", "nnn", name, perl = TRUE)
    filename <- sprintf("%s.soft.gz", name)
    gdsurl <- "%s/geo/datasets/%s/%s/soft/%s"

    fullGEODirPath <- getGEODir(name, destdir)
    destfile <- file.path(fullGEODirPath, filename)

    infile <- file.exists(destfile)

    # Several GEO mirrors supported, but use only one for now.
    mirrorPath <- head(mirrorPath,1)

    if (!infile) {
        tempDestFile <- tempfile(paste0(filename, ".load"), tmpdir=fullGEODirPath)
        for (url in sprintf(gdsurl, mirrorPath,
                            stub, name, filename)){
            tryCatch({
                utils::download.file(url,
                                     destfile = tempDestFile,
                                     method="libcurl")
                file.rename(tempDestFile, destfile)
                infile <- TRUE
            },
            error = function(e) {
                file.remove(tempDestFile)
            },
            warning = function(w) {
                file.remove(tempDestFile)
            })
            if (file.exists(destfile)){
                break
            }
        }

    } else {
        message(paste("Loading from locally found file", destfile))
    }

    l <- suppressWarnings(getGEO(GEO = name, destdir = fullGEODirPath, AnnotGPL = TRUE))

    # extracting all useful information on dataset
    table <- methods::slot(l, "dataTable")


    # extracting table ID_REF | IDENTIFIER/SAMPLE | SAMPLE1 | ...
    data <- Table(table)
    columnsMeta <- Columns(table)  # phenoData
    sampleNames <- as.vector(columnsMeta[["sample"]])
    rownames <- as.vector(data[["ID_REF"]])
    symbol <- as.vector(data[["IDENTIFIER"]])

    data <- data[, sampleNames]  # expression data
    exprs <- as.matrix(data)
    row.names(exprs) <- rownames

    row.names(columnsMeta) <- sampleNames
    pData <- AnnotatedDataFrame(data.frame(columnsMeta, check.names = FALSE))

    fData <- data.frame(id=rownames, symbol=symbol, row.names = rownames, stringsAsFactors = FALSE)
    fData <- AnnotatedDataFrame(fData)

    mabstract=ifelse(is.null(Meta(l)$description),"",Meta(l)$description)
    mpubmedids=ifelse(is.null(Meta(l)$pubmed_id),"",Meta(l)$pubmed_id)
    mtitle=ifelse(is.null(Meta(l)$title),"",Meta(l)$title)

    list(ExpressionSet(assayData = exprs,
                       phenoData = pData,
                       featureData = fData,
                       experimentData=new("MIAME",
                                          abstract=mabstract,
                                          title=mtitle,
                                          pubMedIds=mpubmedids,
                                          other=Meta(l))))
}


#' Returns list of ARCHS4 hdf5 files with expression data
#' @param cacheDir base directory for cache
#' @return list of .h5 files
#' @keywords internal
getArchs4Files <- function(cacheDir) {
    list.files(paste(file.path(cacheDir), 'archs4', sep = .Platform$file.sep), '\\.h5$', full.names = TRUE)
}


#' Loads expression data from ARCHS4 count files.
#' Only sapmles with counted expression are kept.
#' If es already containts expression data it is returned as is.
#' @param es ExpressionSet from GEO to check for expression in ARCHS4
#' @param archs4_files list of available .h5 files from ARCHS4 project
#' @return either original es or an ExpressionSet with loaded count data from ARCHS4
#' @keywords internal
loadFromARCHS4 <- function(es, archs4_files) {

    if (nrow(es) > 0 ) {
        return(es)
    }
    for (destfile in archs4_files) {

        samples <- h5read(destfile, "meta/Sample_geo_accession")
        sampleIndexes <- match(es$geo_accession,
                               samples)

        if (sum(!is.na(sampleIndexes)) == 0) {
            # no needed samples in this file
            H5close()
            next
        }
           genes <- as.character(h5read(destfile, "meta/genes"))
        h5Indexes = list(seq_len(length(genes)),
                         stats::na.omit(sampleIndexes))

        expression <- h5read(destfile,
                             "data/expression",
                             index=h5Indexes)
        rownames(expression) <- genes
        colnames(expression) <- colnames(es)[!is.na(sampleIndexes)]

        es2 <- ExpressionSet(assayData = expression,
                             phenoData = phenoData(es[, !is.na(sampleIndexes)]),
                             annotation = annotation(es),
                             experimentData = experimentData(es)
                            )

        tryCatch({

          fData(es2) <- cbind(fData(es2), "ENSEMBLID"=as.character(h5read(destfile, "meta/gene_ensemblid")))
          }, error=function (e) {})

        tryCatch({
          fData(es2) <- cbind(fData(es2), "Gene ID"=as.character(h5read(destfile, "meta/gene_entrezid")))
        }, error=function (e) {})

        fData(es2) <- cbind(fData(es2), "Gene symbol"=rownames(es2))
        H5close()
        return(es2)
    }

    return(es)
}

#' Loads expression data from .h5 count files.
#' Only samples with counted expression are kept.
#' If es already containts expression data it is returned as is.
#' @param es ExpressionSet from GEO to check for expression in ARCHS4/dee2 or other h5 files
#' @param counts_dir directory with  .h5 files  collections. There must be meta.rda file
#' in counts_dir and each collection's sub directory must have meta.txt file with description.
#' Also \code{counts_dir} must contain \code{counts_priority.txt} file.
#' @return either original es or an ExpressionSet with loaded count data from ARCHS4
#' @import data.table
#' @keywords internal
loadCounts <- function(es, counts_dir) {
    if (!file.exists(file.path(counts_dir, "counts_priority.txt"))) {
      return(es)
    }
    priority <- fread(file.path(counts_dir, "counts_priority.txt"))[, .(directory), keyby = priority]$directory
    if (nrow(es) > 0) {
        return(es)
    }
    load(paste(counts_dir,"meta.rda",sep = "/"))
    sample_amount <- DT_counts_meta[accession %in% es$geo_accession,
                                   .(.N),
                                   by = list(file, type_fac = factor(x = collection_type, levels = priority))]
    if (nrow(sample_amount) == 0) {
        return(es)
    }
    setorderv(x = sample_amount,cols = c("N","type_fac"),order = c(-1,1))
    destfile <- sample_amount[,.SD[1]]$file
    destfile <- file.path(counts_dir, destfile)
    h5_meta <- fread(file.path(dirname(destfile), "meta.txt"), index = "file_name")[file_name == basename(destfile)]
    h5f <- H5Fopen(destfile, flags = "H5F_ACC_RDONLY")
    samples <- h5read(h5f, h5_meta$sample_id)
    sampleIndexes <- match(es$geo_accession,
                           samples)

    gene_id <- strsplit(h5_meta$gene_id, split = ":")[[1]]
    genes <- as.character(h5read(h5f,gene_id[2]))
    h5Indexes = list(stats::na.omit(sampleIndexes),
                     seq_len(length(genes)))
    expression <- NULL
    if (h5_meta$sample_dim == "rows"){
        expression <- h5read(h5f,
                             "data/expression",
                             index = h5Indexes)
        expression <- t(expression)
    } else {
        expression <- h5read(h5f,
                             "data/expression",
                             index = rev(h5Indexes))
    }
    rownames(expression) <- genes
    colnames(expression) <- colnames(es)[!is.na(sampleIndexes)]
    es2 <- ExpressionSet(assayData = expression,
                         phenoData = phenoData(es[, !is.na(sampleIndexes)]),
                         annotation = annotation(es),
                         experimentData = experimentData(es)
     )
    genes_annot <- strsplit(h5_meta$genes_annot, split = ";")[[1]]
    genes_annot <- unlist( lapply(strsplit(genes_annot, split = ":"), function(annot){
        setNames(annot[2], annot[1])
    }))

    genes_annot <- lapply(genes_annot, function(annot){
      tryCatch({
          as.character(h5read(h5f, annot))
      }, error = function(e) {})
    })
    genes_annot [[gene_id[1]]] <- rownames(es2)
    genes_annot <- genes_annot[!unlist(lapply(genes_annot, is.null))]
    fData(es2) <- cbind(fData(es2), genes_annot )
    H5Fclose(h5f)
    return(es2)
}

filterFeatureAnnotations <- function(es) {
    fvarsToKeep <- c()
    if ("Gene symbol" %in% fvarLabels(es)) {
        fvarsToKeep <- c(fvarsToKeep, "Gene symbol")
    } else {
        fvarsToKeep <- c(fvarsToKeep, grep("symbol",
                                           fvarLabels(es),
                                           ignore.case = TRUE,
                                           value = TRUE))
    }

    if ("Gene ID" %in% fvarLabels(es)) {
        fvarsToKeep <- c(fvarsToKeep, "Gene ID")
    } else if ("ENTREZ_GENE_ID" %in%  fvarLabels(es)){
        fvarsToKeep <- c(fvarsToKeep, "ENTREZ_GENE_ID")
    }else if ("ID" %in% fvarLabels(es)) {
        fvarsToKeep <- c(fvarsToKeep, "ID")
    } else {
        fvarsToKeep <- c(fvarsToKeep, grep("entrez",
                                           fvarLabels(es),
                                           ignore.case = TRUE,
                                           value = TRUE))
    }

    if (length(setdiff(fvarsToKeep, "ID")) == 0) {
        fvarsToKeep <- c(fvarsToKeep,
                         setdiff(colnames(featureData(es)), fvarsToKeep))
    }


    featureData(es) <- featureData(es)[, fvarsToKeep]

    if (!any(sapply(fData(es),
                    function(x) identical(rownames(es), as.character(x))
                    ))) {
        fData(es) <- cbind("id"=rownames(es), fData(es))
    }

    es
}
#' @importFrom tidyr spread
filterPhenoAnnotations <- function(es) {
    phenoData(es) <- phenoData(es)[,
                                   grepl("characteristics",
                                         varLabels(es),
                                         ignore.case = TRUE) |
                                       (varLabels(es) %in% c("title",
                                                             "id",
                                                             "geo_accession"
                                       ))]

    chr <- varLabels(es)[grepl("characteristics",
                               varLabels(es),
                               ignore.case = TRUE)]

    parsePData <- function(old.phenodata) {
        old.pdata <- pData(old.phenodata)
        labels <- varLabels(old.phenodata)

        new.pdata <- as.data.frame(matrix(NA, nrow = nrow(old.pdata), ncol = 0))
        rownames(new.pdata) <- rownames(old.pdata)
        sample_key_value = as.data.table(matrix(NA, nrow = 0, ncol =4))
        colnames(sample_key_value) <-c("sample","key","value", "channel")
        for (i in seq_len(ncol(old.pdata))) {
            channel <- sub(pattern = "characteristics_([^.]+)(\\..+)?",replacement = "\\1",x =  names(old.pdata)[i])
            splitted <- strsplit(as.vector(old.pdata[[i]]), ':')
            lengths <- sapply(splitted, length)
            if (all(lengths == 0)) {
                next
            }
            if (any(lengths != 2 & lengths != 0)) {
                new.pdata[[labels[i]]] <- old.pdata[[i]]
            } else {
                zeros <- lengths == 0
                keys <- trimws(take(splitted[!zeros], 1))
                values <- trimws(take(splitted[!zeros], 2))
                samples <- rownames(old.pdata)[!zeros]
                sample_key_value <- rbind(sample_key_value, data.frame(sample = samples, key = keys, value = values, channel = channel))
            }
        }
        if (length(unique(sample_key_value$channel)) >1){
            sample_key_value$key <- paste(sample_key_value$key, sample_key_value$channel, sep = "_")

        }
        sample_key_value$channel <- NULL
        chr_df <- spread(unique(sample_key_value), key, value, fill= NA)
        rownames(chr_df) <- chr_df$sample
        chr_df$sample <- NULL
        if (nrow(chr_df)>0) {
            new.pdata <- cbind(new.pdata, chr_df)
        }
        new.pdata
    }

    if (ncol(es) > 0 && length(chr)>0) {
        new.pdata <-  parsePData(phenoData(es)[, chr])
        pData(es)[, chr] <-NULL
        pData(es)[,colnames(new.pdata)] <- new.pdata
    }

    es
}


#' Load ExpressionSet from GEO Series
#'
#'\code{getGSE} return the ExpressionSet object(s) corresponding
#'     to GEO Series Identifier.
#'
#' @param name String, containing GEO identifier of the dataset.
#'     It should start with 'GSE' or 'GDS' and can include exact GPL
#'     to annotate dataset, separated with dash ('-') from the identifier.
#'
#' @param destdir Directory for caching loaded Series and GPL
#'     files from GEO database.
#'
#' @param mirrorPath URL string which specifies the source of matrices.
#'
#' @return List of ExpressionSet objects, that were available by given
#'     in \code{name} variable GEO identifier.
#'
#' @examples
#' \dontrun{
#'     getGSE('GSE14308', destdir = 'cache')
#'     getGSE('GSE27112')
#'     getGSE('GSE53986')
#' }
#'
#' @export
#' @import rhdf5
getGSE <- function(name, destdir = getPhantasusConf("cache_folders")$geo_path,
                   mirrorPath = getPhantasusConf("geo_mirrors")) {
    if (!isValidExperimentID(name)) {
      stop(name, " does not look like a valid GEO Series ID")
    }
    GEO <- unlist(strsplit(name, "-"))[1]

    stub <- gsub("\\d{1,3}$", "nnn", GEO, perl = TRUE)
    filename <- sprintf("%s_series_matrix.txt.gz", name)
    gseurl <- "%s/geo/series/%s/%s/matrix/%s"

    fullGEODirPath <- getGEODir(name, destdir)
    destfile <- file.path(fullGEODirPath, filename)

    if (!checkGSEType(name, destdir)) {
      stop('Currently unsupported experiment type')
    }

    infile <- file.exists(destfile)

    # Several GEO mirrors supported, but use only one for now.
    mirrorPath <- head(mirrorPath,1)
    if (!infile) {
        tempDestFile <- tempfile(paste0(filename, ".load"), tmpdir=fullGEODirPath)
        for (url in  sprintf(gseurl, mirrorPath,
                             stub, GEO, filename)){
            tryCatch({
                # TODO: should safeDownload() be used here?
                # it doesn't include deletion of temp dest file though, and doesn't suppress errors/warnings
                utils::download.file(url,
                                     destfile = tempDestFile,
                                     method="libcurl")
                file.rename(tempDestFile, destfile)
                infile <- TRUE
            },
            error = function(e) {
                if (file.exists(tempDestFile)) {
                    file.remove(tempDestFile)
                }

            },
            warning = function(w) {
                if (file.exists(tempDestFile)) {
                    file.remove(tempDestFile)
                }
            })
            if(file.exists(destfile)){
                break
            }
        }

    } else {
        message(paste("Loading from locally found file", destfile))
    }

    if (infile && file.size(destfile) > 0) {
        ess <- list(suppressWarnings(getGEO(filename = destfile, destdir = fullGEODirPath, getGPL = FALSE, AnnotGPL = FALSE)))
        for (i in seq_len(length(ess))) {
            ess[[i]] <- annotateFeatureData(ess[[i]], destdir)
        }
    } else {
        gpls <- fromJSON(checkGPLs(name))
        if (length(gpls) == 0) {
            stop(paste("Dataset", name, "not found"))
        }
        if (length(gpls) == 1 && gpls == name) {
            stop(paste("Can't download dataset ", name))

        }
        ess <- list()
        for (i in 1:length(gpls)) {
          ess[[gpls[[i]]]] <- getGSE(gpls[[i]], destdir = destdir, mirrorPath = mirrorPath)[[1]]
        }
        return(ess)
    }

    ess <- lapply(ess, filterFeatureAnnotations)
    archs4_files <- getArchs4Files(destdir)
    useHSDS <- getOption("PhantasusUseHSDS")
    counts_path <- getPhantasusConf("cache_folders")$rnaseq_counts
    if (is.null(useHSDS)){
        if (dir.exists(counts_path)){
            ess <- lapply(ess, loadCounts, counts_dir = counts_path)
        }
    } else{
        if (useHSDS == TRUE){
            ess <- lapply(ess, phantasusLite::loadCountsFromHSDS, url = counts_path)
        }
    }
    if (length(archs4_files) > 0)  {
        ess <- lapply(ess, loadFromARCHS4, archs4_files=archs4_files)
    }
    ess <- lapply(ess, filterPhenoAnnotations)

    ess <- lapply(ess, inferCondition)

    ess


}

#' Load ExpressionSet by GEO identifier
#'
#'\code{getES} return the ExpressionSet object(s) corresponding
#'     to GEO identifier.
#'
#' @param name String, containing GEO identifier of the dataset.
#'     It should start with 'GSE' or 'GDS' and can include exact GPL
#'     to annotate dataset, separated with dash ('-') from the identifier.
#'
#' @param type Type of the dataset: 'GSE' or 'GDS'. If not specified,
#'     the function will take first three letters
#'     of \code{name} variable as type.
#'
#' @param destdir Directory for caching loaded Series and GPL
#'     files from GEO database.
#'
#' @param mirrorPath URL string which specifies the source of matrices.
#'
#' @return List of ExpressionSet objects, that were available by given
#'     in \code{name} variable GEO identifier.
#'
#' @examples
#' \dontrun{
#'     getES('GSE14308', type = 'GSE', destdir = 'cache')
#'     getES('GSE27112')
#'     getES('GDS4922')
#' }
#'
#' @export
getES <- function(name, type = NA, destdir = getPhantasusConf("cache_folders")$geo_path,
                  mirrorPath = getPhantasusConf("geo_mirrors")) {
    if (is.na(type)) {
        type <- substr(name, 1, 3)
    }
    geoDir <- getGEODir(name, destdir)
    possibly.cached <- file.path(geoDir, paste0(name, ".rda"))
    if (file.exists(possibly.cached)) {
        load(possibly.cached)
        message(paste("Loaded from locally cached parsed file", possibly.cached))
    } else {
        if (type == "GSE") {
            res <- getGSE(name, destdir, mirrorPath)
        } else if (type == "GDS") {
            res <- getGDS(name, destdir, mirrorPath)
        } else {
            stop("Incorrect name or type of the dataset")
        }
        if (length(res) > 1) {
            for (i in 1:length(res)) {
                ess <- list(res[[i]])
                destfile <- file.path(geoDir,
                                      paste0(name,
                                             "-",
                                             annotation(res[[i]]),
                                             ".rda"))
                message(paste("Cached dataset to ", destfile))
                save(ess, file = destfile)
            }
        }
        ess <- res
        destfile <- file.path(geoDir, paste0(name, ".rda"))
        message(paste("Cached dataset to ", destfile))
        save(ess, file = destfile)
    }
    ess
}


listCachedESs <- function(destdir) {
    res <- list.files(destdir, pattern=".*\\.rda$", recursive = TRUE)
    res <- grep("\\.gz\\.rda$", res, invert = TRUE, value = TRUE)
    res <- res[grep("^(GSE|GDS)", basename(res), value = FALSE)]
    res <- sub("\\.rda$", "", res)
    res
}

#' Reparse cached expression sets from GEO.
#'
#' The function should be used on phantasus version updates that change
#' behavior of loading datasets from GEO. It finds all the datasets
#' that were cached and runs `getES` for them again. The function
#' uses cached Series and other files from GEO.
#'
#' @param destdir Directory used for caching loaded Series files from GEO database.
#'
#' @param mirrorPath URL string which specifies the source of matrices.
#'
#' @return vector of previously cached GSE IDs
#'
#' @examples
#' reparseCachedESs(destdir=tempdir(), "https://ftp.ncbi.nlm.nih.gov")
#'
#' @export
reparseCachedESs <- function(destdir,
                                mirrorPath = getPhantasusConf("geo_mirrors")) {
    toReparse <- listCachedESs(destdir)

    for (path in toReparse) {
        name <- basename(path)
        geoDir <- getGEODir(name, destdir)
        message(paste0("Reparsing dataset ", name))
        destfile <- file.path(geoDir, paste0(name, ".rda"))
        bakfile <- paste0(destfile, ".bak")
        binfile <- file.path(geoDir, paste0(name, ".bin"))
        if (file.exists(binfile)) {
            file.remove(binfile)
        }

        tryCatch({
            if (file.exists(destfile)) {
                file.rename(destfile, bakfile)
            }

            getES(name, destdir = destdir, mirrorPath = mirrorPath)
            if (file.exists(bakfile)) {
                file.remove(bakfile)
            }

        }, error = function(e) {
            message(paste0("Error occured while reparsing, old file stored as ",
                           bakfile))
        })
    }
    return(basename(toReparse))
}


#' Check possible annotations for GEO Dataset.
#'
#' \code{checkGPLs} returns GPL-names for
#'     the specified GEO identifier.
#'
#' @param name String, containing GEO identifier of the dataset.
#'
#' @return Vector of filenames serialized in JSON format.
#'     If there is only one GPL for that dataset, the function will
#'     return \code{name}.
#'
#' @examples
#' \dontrun{
#' checkGPLs('GSE27112')
#' checkGPLs('GSE14308')
#' }
#' @keywords internal
checkGPLsFallback <- function(name) {
    spl <- unlist(strsplit(name, "-", fixed=TRUE))
    if (length(spl) == 2) {
        gpls <- jsonlite::fromJSON(checkGPLs(spl[1]))
        gpls <- intersect(gpls, name)
        return(jsonlite::toJSON(gpls))
    }

    mirrorPath <-  getPhantasusConf("geo_mirrors")

    cacheDir <- getPhantasusConf("cache_folders")$geo_path


    type <- substr(name, 1, 3)
    assertthat::assert_that( (type == "GDS" || type == "GSE")
                            && nchar(name) >= 4)

    stub <- gsub("\\d{1,3}$", "nnn", name, perl = TRUE)
    gdsurl <- "%s/geo/%s/%s/%s/"

    url <- sprintf(gdsurl, mirrorPath,
                   if (type == "GDS") "datasets" else "series", stub, name)

    cachePath <- file.path(cacheDir,
                        if (type == "GDS") "datasets" else "series",
                        stub,
                        name,
                        "gpls")

    dir.create(dirname(cachePath), recursive = TRUE, showWarnings = FALSE)

    if (file.exists(cachePath)) {
        gpls <- readLines(cachePath)
        if (length(gpls) != 0) {
            return(jsonlite::toJSON(gpls))
        }
    }

    if (type != "GDS") {
        url <- paste0(url, "matrix/")
    }

    gpls <- c()

    tryCatch({
        for (mirror in url){
            resp <- httr::GET(mirror)
            if (httr::status_code(resp) != 404) {
                break
            }
        }
        if (httr::status_code(resp) == 404) {
            warning("No such dataset")
            return(jsonlite::toJSON(c()))
        } else {
            if (type == "GDS") {
                gpls <- c(name)
            } else {
                con <- rawConnection(resp$content)
                file.names <- GEOquery:::getDirListing(con)
                close(con)

                file.names <- file.names[grepl(pattern = paste0("^", name),
                                            x = file.names)]

                file.names <- unlist(lapply(file.names, function(x) {
                    paste0(substr(x, 1, regexpr("_", x) - 1))
                }))
                if (length(file.names) == 1) {
                    file.names <- c(name)
                }
                gpls <- file.names
            }

            writeLines(gpls, cachePath)
            return(jsonlite::toJSON(gpls))
        }
    },
    error = function(e) {
        message("Problems establishing connection.")
        return(jsonlite::toJSON(c()))
    })
}

checkGPLs <- function(name) {
  spl <- unlist(strsplit(name, "-", fixed=TRUE))
  GEO <- spl[1]
  if (length(spl) == 2) {
    gpls <- jsonlite::fromJSON(checkGPLs(spl[1]))
    gpls <- intersect(gpls, name)
    return(jsonlite::toJSON(gpls))
  }

  cacheDir <- getPhantasusConf("cache_folders")$geo_path

  GEOdir <- dirname(getGEODir(GEO, cacheDir))
  GPLCacheFile <- file.path(GEOdir, 'gpls')
  if (file.exists(GPLCacheFile)) {
    gpls <- readLines(GPLCacheFile)
    if (length(gpls) != 0) {
      return(jsonlite::toJSON(gpls))
    }
  }

  tryCatch({
    briefData <- getBriefData(name, cacheDir)
    platforms <- briefData$platform_id

    if (length(platforms) < 1) {
      stop('No platforms in brief data')
    }

    if (length(platforms) == 1) {
      gpls <- c(name)
    }

    if (length(platforms) >= 2) {
      gpls <- unlist(lapply(platforms, function (platform) {
        paste(spl[1], platform, sep='-')
      }))
    }

    writeLines(gpls, GPLCacheFile)
    return(jsonlite::toJSON(gpls))
  }, error=function(e) {
    return(checkGPLsFallback(name))
  })
}

#' Checks GSE to be supported
#' @param name GSE id, with optional GPL specification
#' @param destDir path to cache directory
#' @param combine function on how to combine results, when multiple platforms are present
#' @return logical vector if the dataset is supported or not
#' @keywords internal
checkGSEType <- function (name, destDir, combine=any) {
  spl <- unlist(strsplit(name, "-", fixed=TRUE))
  GEO <- spl[1]

  briefData <- getBriefData(name, destDir)

  gpls <- spl[2]
  if (is.na(gpls)) {
      gpls <- briefData$platform_id
  }

  gplsOK <- sapply(gpls, function(gpl) {
      gplBrief <- getBriefData(gpl, destdir=destDir)
      return(as.numeric(gplBrief$data_row_count) <= 100000)
  })

  return(combine(gplsOK))
}

removeRepeatWords <- function(titles) {
    titles_without_repeat_words <- titles
    repeat_words <- regmatches(titles, regexpr("(?![-+}\\]\\)])(\\W|_)*\\w*$", titles, ignore.case = TRUE, perl = TRUE))
    lsuff <- lcSuffix(repeat_words, ignore.case = TRUE)
    if ((all(stringr::str_length(lsuff) == stringr::str_length(repeat_words))) & (all(sub("(\\W*|_)*\\w*$", "", titles, ignore.case = TRUE, perl = TRUE) != ""))) {
        titles_without_repeat_words <- sub("(?![-+}\\]\\)])(\\W|_)*\\w*$", "", titles, ignore.case = TRUE, perl = TRUE)
    }
    if (all(grepl("-$", titles_without_repeat_words, ignore.case = TRUE))) titles_without_repeat_words <- sub("-$", "", titles_without_repeat_words, ignore.case = TRUE, perl = TRUE)
    return(titles_without_repeat_words)
}

inferConditionImpl <- function(gse_titles) {
    inferCondition <- gse_titles
    rep_num <- NULL
    if ((length(inferCondition) > 40) | (length(inferCondition) < 3))
    {
        return(list())
    } else if (! all(grepl("[A-z]", inferCondition, ignore.case = TRUE)))
    {
        return(list())
    } else if (all(grepl(" vs[. ]{1}", inferCondition)))
    {
        return(list())
    } else
    {
        lsuff <- lcSuffix(inferCondition)
        suff_length <- stringr::str_length(lsuff)
        if (suff_length > 1) inferCondition <- stringr::str_sub(inferCondition, 1, -suff_length-1)
        if (all(grepl("((bio[A-z]*)|(tech[A-z]*))?[ .#_-]*((rep.*)|(set.*)|(sample)|(exp.*)|(case))[ .#_-]*\\d+", inferCondition, ignore.case = TRUE)))
        {
            sample <- regmatches(inferCondition, regexpr("((bio[A-z]*)|(tech[A-z]*))?[ .#_-]*((rep.*)|(set.*)|(sample)|(exp.*)|(case))[ .#_-]*\\d+", inferCondition, ignore.case = TRUE))
            rep_num <- regmatches(sample, regexpr("\\d+$", sample))
            inferCondition <- sub("(?![-+}\\]\\)])(\\W|_)*((bio[A-z]*)|(tech[A-z]*))?[ .#_-]*((rep.*)|(set.*)|(sample)|(exp.*)|(case))[ .#_-]*\\d+(\\W|_)*", "", inferCondition, ignore.case = TRUE, perl = TRUE)

        }
        else if (all(grepl("((bio[A-z]*)|(tech[A-z]*))?[ .#_-]*((rep.*)|(set.*)|(sample)|(exp.*)|(case))[ .#_-]*[A-z]$", inferCondition, ignore.case = TRUE)))
        {
            sample <- regmatches(inferCondition, regexpr("((bio[A-z]*)|(tech[A-z]*))?[ .#_-]*((rep.*)|(set.*)|(sample)|(exp.*)|(case))[ .#_-]*[A-z]$", inferCondition, ignore.case = TRUE))
            rep_num <- regmatches(sample, regexpr("[A-z]$", sample))
            inferCondition <- sub("(?![-+}\\]\\)])(\\W|_)*((bio[A-z]*)|(tech[A-z]*))?[ .#_-]*((rep.*)|(set.*)|(sample)|(exp.*)|(case))[ .#_-]*[A-z]$", "", inferCondition, ignore.case = TRUE, perl = TRUE)

        }
        else if (length(unique(sub("[- \\.#_]*\\d+$", "", inferCondition))) == 1) {
            return(list())
        }
        else if (all(grepl("[- \\.#_]*\\d+$", inferCondition)) & (length(unique(regmatches(inferCondition, regexpr("\\d+$", inferCondition)))) > 1))
        {
            sample <- sub("\\d+$", "", inferCondition)
            if (length(unique(regmatches(sample, regexpr("[- \\.#_]$", sample)))) > 1)
            {
                return(list())
            }
            else
            {
                rep_num <- regmatches(inferCondition, regexpr("\\d+$", inferCondition))
                inferCondition <- removeRepeatWords(sub("[ \\.#_]*\\d+$", "", inferCondition))

            }
        }
        else return(list())
    }

    if ((length(rep_num) > 1) & (length(unique(inferCondition)) < length(unique(gse_titles))) & (length(unique(inferCondition)) > 1))
        return(list(condition=inferCondition, replicate=rep_num))
    else return(list())
}

inferCondition <- function(es) {
    newAnnot <- inferConditionImpl(es$title)
    if (length(newAnnot) == 2) {
        pData(es)$condition <- newAnnot$condition
        pData(es)$replicate <- newAnnot$replicate
    }
    es
}


downloadGPL <- function (GPL, destdir = getPhantasusConf("cache_folders")$geo_path) {
  GPL <- toupper(GPL)
  stub = gsub('\\d{1,3}$','nnn',GPL,perl=TRUE)
  GPLDirPath <- '%s/platforms/%s/%s/annot'
  fullGPLDirPath <- file.path(sprintf(GPLDirPath, destdir, stub, GPL))

  cachedFile <- file.path(fullGPLDirPath, paste0(GPL, ".annot.gz"))
  cachedSoft <- file.path(fullGPLDirPath, paste0(GPL, ".soft"))
  cachedSoftGz <- file.path(fullGPLDirPath, paste0(GPL, ".soft.gz"))
  if (file.exists(cachedFile)) {
    if (file.size(cachedFile) == 0) {
      file.remove(cachedFile)
      message(cachedFile, ' size is 0')
    } else {
      return (cachedFile)
    }
  } else if (file.exists(cachedSoft)) {
    if (file.size(cachedSoft) == 0) {
      file.remove(cachedSoft)
      message(cachedSoft, ' size is 0')
    } else {
      return (cachedSoft)
    }
  } else if (file.exists(cachedSoftGz)) {
    if (file.size(cachedSoftGz) == 0) {
      file.remove(cachedSoftGz)
      message(cachedSoftGz, ' size is 0')
    } else {
      return (cachedSoftGz)
    }
  }

  mirrorPath <- getPhantasusConf("geo_mirrors")
  annotPath <- paste0(mirrorPath,  '/geo/platforms/%s/%s/annot/%s')
  annotURL <- sprintf(annotPath,stub,GPL,paste0(GPL,'.annot.gz'))
  dir.create(fullGPLDirPath, showWarnings = FALSE, recursive = TRUE)
  targetFile <- ''

  for (url in annotURL){
      req <- httr::HEAD(url)
      if (httr::status_code(req) != 404) {
          # annot available
          tmp <- tempfile(pattern=paste0(GPL, ".annot.gz"), tmpdir=fullGPLDirPath)
          tryCatch({
              download.file(url, tmp)
          }, error=function (e) {
              unlink(tmp)
              stop('Could not download GPL ', GPL, e)
          })

          file.copy(tmp, cachedFile)
          unlink(tmp)
          targetFile <- cachedFile
      }
      if (targetFile != ''){
          break
      }
  }

  if (targetFile == ''){
    # need submitter
    tmp <- tempfile(pattern=paste0(GPL, ".soft"), tmpdir=fullGPLDirPath)
    apiURL <- "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi"
    submitterURL <- paste(apiURL,'?targ=self&acc=',GPL,'&form=text&view=data',sep='')
    tryCatch({
      download.file(submitterURL, tmp, headers = c("accept-encoding" = "gzip"))
    }, error=function (e) {
      unlink(tmp)
      stop('Could not download GPL ', GPL, e)
    })

    file.rename(tmp, cachedSoftGz)
    targetFile <- cachedSoftGz
  }

  if (file.size(targetFile) == 0) {
    file.remove(targetFile)
    stop('Could not download GPL ', GPL, '. Zero file')
  }

  con <- file(targetFile, 'r')
  first_hundred_lines <- readLines(con, n=100)
  close(con)
  found<-grep('^\\^(PLATFORM|ANNOTATION)', first_hundred_lines, ignore.case = TRUE)
  if (!length(found)) {
    file.remove(targetFile)
    stop('Could not download GPL ', GPL, '.  Possible NCBI problems')
  }


  return(targetFile)
}

getGPLAnnotation <- function (GPL, destdir = getPhantasusConf("cache_folders")$geo_path) {
    filename <- downloadGPL(GPL, destdir)
    # ret <- parseGEO(filename)

    # TODO: workaround for https://github.com/seandavi/GEOquery/pull/120,
    txt <- data.table::fread(filename,sep="")[[1]]
    if (length(txt) == 0) {
      txt <- c("table_begin", "table_end")
    }
    ret <- GEOquery:::.parseGPLTxt(txt)
    # end of workaround
    return(ret)
}

annotateFeatureData <- function (es, destdir = getPhantasusConf("cache_folders")$geo_path) {
    platform <- as.character(es$platform_id)[1]
    platformParsed <- getGPLAnnotation(platform, destdir)

    #https://github.com/seandavi/GEOquery/blob/master/R/parseGEO.R#L569
    #############################
    vmd <- Columns(platformParsed)
    dat <- Table(platformParsed)
    ## GEO uses "TAG" instead of "ID" for SAGE GSE/GPL entries, but it is apparently
    ##     always the first column, so use dat[,1] instead of dat$ID
    ## The next line deals with the empty GSE
    tmpnames=character(0)
    if(ncol(dat)>0) {
        tmpnames=as.character(dat[,1])
    }
    ## Fixed bug caused by an ID being "NA" in GSE15197, for example
    tmpnames[is.na(tmpnames)]="NA"
    rownames(dat) <- make.unique(tmpnames)
    ## Apparently, NCBI GEO uses case-insensitive matching
    ## between platform IDs and series ID Refs ???
    dat <- dat[match(tolower(rownames(es)),tolower(rownames(dat))),]
    # Fix possibility of duplicate column names in the
    # GPL files; this is prevalent in the Annotation GPLs
    rownames(vmd) <- make.unique(colnames(Table(platformParsed)))
    colnames(dat) <- rownames(vmd)
    ##############################

    featureData(es) <- new('AnnotatedDataFrame',data=dat,varMetadata=vmd)
    es
}
