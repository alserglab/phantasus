PROTOBUF_LAYOUT_VERSION = c(0x00, 0x02)

getIndicesVector <- function(current, neededLength) {
    if (length(current) == 0) {
        current <- 0:(neededLength - 1)
    }
    current + 1
}


#' Reads ExpressionSet from a GCT file.
#' Function is deprecated, please use phantasusLite:::readGct() instead
#'
#' @param ... parameters for phantasusLite:::readGct() call
#'
#' @return ExpressionSet object
#' @export
read.gct <- function(...) {
    warning("phantasus::read.gct() function is deprecated, please use phantasusLite:::readGct() instead")
    phantasusLite::readGct(...)
}

read.tsv <- function(file, header = TRUE, sep = "\t", quote = "",
                        comment.char = "",
                        check.names = FALSE, ...) {
    args <- list(...)
    res <- utils::read.table(file, header = header, sep = sep, quote = quote,
                    comment.char = comment.char, check.names = check.names,
                    stringsAsFactors = FALSE,
                    ...)
    if ( (!"row.names" %in% names(args)) && (colnames(res)[1] == "") ) {
        rownames(res) <- res[, 1]
        res[[1]] <- NULL
    }
    res
}

#' Saves ExpressionSet to a GCT file (version 1.3).
#' Function is deprecated, please use phantasusLite:::writeGct() instead
#' @param ... parameters for phantasusLite:::writeGct() call
#' @return Result of the closing file (as in `close()` function`)
#' @import Biobase
#' @export
write.gct <- function(...) {
    warning("phantasus::write.gct() function is deprecated, please use phantasusLite:::writeGct() instead")
    phantasusLite::writeGct(...)
}


makeAnnotated <- function(data) {
    meta <- data.frame(labelDescription = colnames(data))
    rownames(meta) <- colnames(data)

    methods::new("AnnotatedDataFrame", data = data, varMeta = meta)
}

take <- function(x, n) {
    sapply(x, function(x) {
        x[[n]]
    })
}

writeToList <- function(es) {
    data <- as.matrix(exprs(es))
    colnames(data) <- NULL
    row.names(data) <- NULL

    pdata <- pData(es)
    row.names(pdata) <- NULL
    pdata <- as.list(pdata)

    rownames <- rownames(es)

    fdata <- fData(es)
    row.names(fdata) <- NULL
    fdata <- as.list(fdata)


    ed <- experimentData(es)
    experimentList <- as.list(expinfo(ed))
    experimentList$other <- as.list(ed@other)
    experimentList$pubMedIds <- pubMedIds(ed)

    res <- list(data = data, pdata = pdata, fdata = fdata,
                rownames = rownames,
                colMetaNames = varLabels(es),
                rowMetaNames = fvarLabels(es),
                experimentData = experimentList)
    res
}

#' Update archs4 files.
#'
#' Download archs4 or archs4zoo counts in \code{cacheDir}. If directory does not exists function makes nothing and produce corresponding warnings.
#' @importFrom utils download.file
#' @param cacheDir file path to \bold{archs4} cache directory
#' @param organism vector which determines organisms to download: human, mouse, zoo or all as default.
#' Also can be a genus. Possible genus: \enumerate{ \item drosophila \item gallus \item bos \item caenorhabditis
#' \item danio \item rattus \item saccharomyces \item arabidopsis}
#' @param force logical value which let function replace current files
#' @keywords internal
updateARCHS4 <- function (cacheDir = file.path(getPhantasusConf("cache_folders")$rnaseq_counts, "archs4"), organism = c("all"), force = FALSE){
    zoo_dict <- c("drosophila" = "Drosophila_melanogaster",
                 "bos" = "Bos_taurus",
                 "caenorhabditis" = "Caenorhabditis_elegans",
                 "danio" = "Danio_rerio",
                 "gallus" = "Gallus_gallus",
                 "rattus" = "Rattus_norvegicus",
                 "saccharomyces" = "Saccharomyces_cerevisiae",
                 "arabidopsis" = "Arabidopsis_thaliana"
               )
    organism <- tolower(organism)
    if (any(organism %in% c("all","human")) && (force || !file.exists(file.path(cacheDir, "human_gene_v2.2.h5")))) {
        download.file(url = "https://s3.dev.maayanlab.cloud/archs4/files/human_gene_v2.2.h5",
                      destfile = file.path(cacheDir, "human_gene_v2.2.h5"),
                      mode = "wb")
    }
    if (any(organism  %in% c("all","mouse")) && (force || !file.exists(file.path(cacheDir, "mouse_gene_v2.2.h5")))) {
        download.file(url = "https://s3.dev.maayanlab.cloud/archs4/files/mouse_gene_v2.2.h5",
                      destfile = file.path(cacheDir, "mouse_gene_v2.2.h5"),
                      mode = "wb")
    }
    for (sp in names(zoo_dict)){
        if (any(organism %in% c("all","zoo", sp))) {
            sp_file_name <- paste0(zoo_dict[sp], "_genecount_v1.h5")
            sp_file_path <-  file.path(cacheDir, sp_file_name)
            if (force || !file.exists(sp_file_path)) {
                download.file(url = paste0("https://s3.amazonaws.com/mssm-archs4-zoo/", sp_file_name),
                              destfile = sp_file_path,
                              mode = "wb")
            }
        }
    }
}

#' Update ARCHS4 meta files
#'
#' Creates \code{meta.txt} file, which describes typical archs4 and archs4Zoo files.
#' @param archDir path to directory with arch4 .h5 files.
#' @details This function produces very specific "hardcoded" \code{meta.txt} file for arch4 and archs4ZOO counts collections.
#' See \code{\link{validateCountsCollection}} for more common information and \code{meta.txt}  file structure
#' @seealso \code{\link{validateCountsCollection}}
#' @import data.table
#' @keywords internal
updateARCHS4meta <- function(archDir = file.path(getPhantasusConf("cache_folders")$rnaseq_counts, "archs4")){
    archs4files <- list.files(archDir, pattern = '\\.h5$')
    DT_meta <- data.frame(matrix(ncol = 5, nrow = length(archs4files), dimnames = list(NULL, c("file_name", "sample_id", "sample_dim",	"gene_id", "genes_annot"))))
    DT_meta$file_name <- archs4files
    genus <- tolower(sapply(strsplit(x = archs4files, split = "_"), function(x) x[1]))
    for (i_file in seq_along(archs4files)) {
        cur_file <- file.path(archDir, archs4files[i_file])
        h5f <- H5Fopen(cur_file, flags = "H5F_ACC_RDONLY")
        arch_version <- if (H5Lexists(h5f, "info/version")) {
            h5read(h5f, "info/version")
        } else {
            h5read(h5f, "meta/info/version")
        }
        gene_id_type <- NA
        if (genus[i_file] %in% c("human", "mouse")) {
            gene_id_type <- "Gene Symbol"
        } else if (genus[i_file] %in% c("rattus", "bos", "gallus", "danio")) {
            gene_id_type <- "ENSEMBLID"
        } else if (genus[i_file] %in% c("arabidopsis")) {
            gene_id_type<- "TAIR id"
        } else if (genus[i_file] %in% c("saccharomyces")) {
            gene_id_type <- "ORF id"
        } else if (genus[i_file] %in% c("caenorhabditis")) {
            gene_id_type <- "WormBase id"
        } else if (genus[i_file] %in% c("drosophila")) {
            gene_id_type <- "FlyBase id"
        } else{
            gene_id_type<- "gene"
        }
        if (is.na(arch_version)) {
            arch_version <- "8"
        }
        if (arch_version == "1"){
            DT_meta$sample_id[i_file] <-  "/meta/Sample_geo_accession"
            DT_meta$gene_id[i_file] <-  paste(gene_id_type, "/meta/genes", sep = ":")
            DT_meta$sample_dim[i_file] = "columns"
            annot_str <- ""
            DT_meta$genes_annot[i_file] = annot_str
        }
        if (arch_version %in% c("7", "8")){
            DT_meta$sample_id[i_file] <-  "/meta/Sample_geo_accession"
            DT_meta$gene_id[i_file] <-  paste(gene_id_type, "/meta/genes", sep = ":")
            DT_meta$sample_dim[i_file] = "columns"
            annot_str <- "Gene ID:/meta/gene_entrezid"
            if (!toupper(gene_id_type) == "GENE SYMBOL") {
              annot_str <- paste(annot_str, "Gene symbol:/meta/genes", sep = ";")
            }
            if (!toupper(gene_id_type) == "ENSEMBLID") {
                annot_str <- paste(annot_str, "ENSEMBLID:/meta/gene_ensemblid", sep = ";")
            }
            DT_meta$genes_annot[i_file] = annot_str
        }
        if (arch_version %in% c("9", "10", "11")) {
            DT_meta$sample_id[i_file] <- "/meta/samples/geo_accession"
            DT_meta$gene_id[i_file] <-  paste(gene_id_type, "/meta/genes/genes", sep = ":")
            DT_meta$sample_dim[i_file] = "rows"
            annot_str <- ""
            if (!toupper(gene_id_type) == "GENE SYMBOL") {
                annot_str <-  "Gene symbol:/meta/genes/gene_symbol"
            }
            if (!toupper(gene_id_type) == "ENSEMBLID") {
                annot_str <- paste(annot_str, "ENSEMBLID:/meta/genes/ensembl_gene_id", sep = ";")
            }
            DT_meta$genes_annot[i_file] = trimws(annot_str, which = "left", whitespace = ";")
        }
        if(arch_version %in% c("2.2" , "2.2.1")){
            gene_id_type <- "ENSEMBLID"
            DT_meta$sample_id[i_file] <- "/meta/samples/geo_accession"
            DT_meta$gene_id[i_file] <-  paste(gene_id_type, "/meta/genes/ensembl_gene_id", sep = ":")
            DT_meta$sample_dim[i_file] = "rows"
            annot_str <- "Gene symbol:/meta/genes/symbol"
            DT_meta$genes_annot[i_file] = annot_str
        }

        H5Fclose(h5f)
    }

    write.table(x = DT_meta,
                file = file.path(archDir, "meta.txt"),
                sep = "\t",
                col.names = TRUE, row.names = FALSE,
                quote = FALSE)
}
#' Update DEE2 meta files
#'
#' Creates \code{meta.txt} file, which describes typical dee2 files.
#' @param destDir path to directory with DEE2 .h5 files.
#' @details This function produces very specific "hardcoded" \code{meta.txt} file for dee2 counts colletction.
#' See \code{\link{validateCountsCollection}} for more common information and \code{meta.txt}  file structure
#' @seealso \code{\link{validateCountsCollection}}
#' @import data.table
#' @keywords internal
updateDEE2meta <- function(destDir = file.path(getPhantasusConf("cache_folders")$rnaseq_counts, "dee2")){
    dee2files <- list.files(destDir, pattern = '\\.h5$')
    DT_meta <- data.frame(matrix(ncol = 5, nrow = length(dee2files), dimnames = list(NULL, c("file_name", "sample_id", "sample_dim",	"gene_id", "genes_annot"))))
    DT_meta$file_name <- dee2files
    DT_meta$sample_dim <- "rows"
    DT_meta$sample_id <- "/meta/samples/geo_accession"
    genus <- sapply(strsplit(x = dee2files, split = "_"), function(x) x[1])
    for (i_file in 1:length(dee2files)) {
        if (genus[i_file] %in% c("hsapiens", "mmusculus", "drerio", "rattus")) {
            gene_id_type <- "ENSEMBLID"
        } else if (genus[i_file] %in% c("athaliana", "ecoli")) {
            gene_id_type <- "Locus tag"
        } else if (genus[i_file] %in% c("scerevisiae")) {
            gene_id_type <- "Yeast id"
        } else if (genus[i_file] %in% c("dmelanogaster")){
            gene_id_type <- "FlyBase id"
        } else if (genus[i_file] %in% c("celegans")) {
            gene_id_type <- "WormBase id"
        } else{
            gene_id_type <- "gene"
        }
        DT_meta$gene_id[i_file] <- paste(gene_id_type, "/meta/genes/ensembl_gene_id", sep = ":")

        DT_meta$genes_annot[i_file]  <- "Gene symbol:/meta/genes/gene_symbol"
    }
    write.table(x = DT_meta, file = file.path(destDir, "meta.txt"), sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
}

stopPhantasus <- function(){
    stop("Phantasus is not configured. Run phantasus::setupPhantasus() to complete setup.")
}

areCacheFoldersValid <- function(cache_folders){
    for (folder in cache_folders[names(cache_folders) != "rnaseq_counts"]) {
        folder <- normalizePath(folder)
        if (!dir.exists(folder) || !rw_dir_check(folder)){
            return(FALSE)
        }

    }
    counts_source <- getPhantasusConf("cache_folders")$rnaseq_counts

    if (nchar(counts_source) == 0){
        return(FALSE)
    }
    valid_hsds <- tryCatch(expr = isHSDS(getPhantasusConf("cache_folders")$rnaseq_counts),
                           error = function(e) {FALSE})
    options(PhantasusUseHSDS = valid_hsds)
    if (is.null(valid_hsds)){

        if (!dir.exists(counts_source)){
            return(FALSE)
        } else {
            return(rw_dir_check(counts_source))
        }
    }
    return(valid_hsds)
}
rw_dir_check <- function(dir_name){
    if (file.access(dir_name, mode = 2) != 0){
        stop(paste("!Bad configuration:", dir_name , " is not writable"))
    }
    if (file.access(dir_name, mode = 4) != 0){
        stop(paste("!Bad configuration:", dir_name , " is not readable"))
    }
    return(TRUE)
}
selfCheck <- function(verbose = FALSE) {
    cacheDir = getPhantasusConf("cache_root")
    preloadedDir = getPhantasusConf("preloaded_dir")
    if (!is.null(preloadedDir) && dir.exists(preloadedDir)) {
        preloadedFiles <- list.files(preloadedDir, pattern = "\\.(gct|rda)$")
        message(paste(length(preloadedFiles), 'preloaded datasets are available'))
        if (verbose) {
            message(paste0(preloadedFiles,  collapse = " "))
            message(" ")
        }
    } else {
        message('Preloaded dir is not set')
    }
    use_hsds <- getOption("PhantasusUseHSDS")
    h5counts <- list()
    if (is.null(use_hsds)){
        counts_dir <- getPhantasusConf("cache_folders")$rnaseq_counts
        if (!isCountsPriorityValid(counts_dir) || isCountsMetaOld(counts_dir)){
            message("!! RNA-seq counts was not installed properly.")
            stopPhantasus()
        }
        h5counts <- list.files(counts_dir, recursive = TRUE,
                               pattern = '\\.h5$')
    } else if (use_hsds == TRUE) {
        if (nchar(system.file(package = "phantasusLite")) == 0){
            stopPhantasus()
        }
        h5counts <- phantasusLite::getHSDSFileList(getPhantasusConf("cache_folders")$rnaseq_counts)
    }
    archs4Files <- list.files(file.path(cacheDir, "archs4"),
                              pattern = '\\.h5$')
    if (length(h5counts)) {
        message(paste(length(h5counts), 'counts files are available'))
        if (verbose) {
            message(paste0(h5counts, collapse = " "))
            message(" ")
        }
        if (length((archs4Files))) {
            message("!!! deprecated archs4-based directory architecture exists. It will be ignored and RNA-seq will load only from counts directory.")
        }
    } else {
        if (length(archs4Files)) {
            message("!!! archs4-based directory architecture is obsolete and will dissapear in future release")
            message(paste(length(archs4Files), 'archs4 files are available'))
            if (verbose) {
                message(paste0(archs4Files, collapse = " "))
                message(" ")
            }
        } else {
            message('!!! No counts provided RNA-seq will load without matrices')
        }
    }
    annotDir <- getPhantasusConf("cache_folders")$annot_db
    dbFiles <- list.files(annotDir, pattern = '\\.sqlite$')
    if (length(dbFiles)) {
        message(paste(length(dbFiles), 'annotationDb are available'))
        if (verbose) {
            message(paste0(dbFiles, collapse = " "))
            message(" ")
        }
    } else {
        message('!!! No annotationDb provided')
    }

    fgseaDir <-  getPhantasusConf("cache_folders")$fgsea_pathways
    fgseaFiles <- list.files(fgseaDir, '\\.rds$', full.names = FALSE)
    if (length(fgseaFiles)) {
        message(paste(length(fgseaFiles), 'fgsea tables are available'))
        if (verbose) {
            message(paste0(fgseaFiles, collapse = " "))
            message(" ")
        }
    } else {
        message('!!! No fgsea tables provided')
    }
}

#' check if url  responding as HSDS server
#' TRUE - hsds
#' FALSE - web link but not working
#' NULL - not web link
#' @param url URL to check
#' @keywords internal
isHSDS <- function(url){
    if(!grepl(pattern = "^http(s)?://", x = url)){
        return(NULL)
    }
    req <- httr::GET(url)
    if( req$headers$server == "Highly Scalable Data Service (HSDS)"){
        return(TRUE)
    }
    return(FALSE)
}

safeDownload <- function(url, dir, filename, ...) {
    dest <- file.path(dir, filename)
    if (file.exists(dest)) {
        return()
    }

    tempDest <- tempfile(paste0(filename, ".load"), tmpdir = dir)
    curl::curl_download(url, destfile = tempDest, ...)
    file.rename(tempDest, dest)
}

isValidExperimentID <- function(name) {
    grepl("^((GSE|GDS)[0-9]+(-GPL[0-9]+)?)|(GPL[0-9]+)$", name, ignore.case = TRUE)
}


getGEODir <- function(name, destdir = file.path(tempdir(), "geo")) {
    if (!isValidExperimentID(name)) {
        stop(name, " does not look like a valid GEO Series ID")
    }
    type <- substr(name, 1, 3)
    GEO <- unlist(strsplit(name, "-"))[1]
    stub <- gsub("\\d{1,3}$", "nnn", GEO, perl = TRUE)
    gdsDirPath <- "%s/datasets/%s/%s/soft"
    gseDirPath <- "%s/series/%s/%s/matrix"
    gplDirPath <- "%s/platforms/%s/%s/soft"
    if (type == 'GSE') {
        fullGEODirPath <- file.path(sprintf(gseDirPath, destdir, stub, GEO))
    } else if (type == "GDS") {
        fullGEODirPath <- file.path(sprintf(gdsDirPath, destdir, stub, GEO))
    } else if (type == "GPL") {
        fullGEODirPath <- file.path(sprintf(gplDirPath, destdir, stub, GEO))
    } else {
        stop("Unsupported GEO type: ", type)
    }
    dir.create(fullGEODirPath, showWarnings = FALSE, recursive = TRUE)

    fullGEODirPath
}

getBriefData <- function(name, destdir = tempdir()) {
    GEO <- unlist(strsplit(name, "-"))[1]
    GEOdir <- dirname(getGEODir(GEO, destdir))
    briefFile <- file.path(GEOdir, 'brief')

    if (file.exists(briefFile)) {
        message('Using cached brief file: ', briefFile)
    } else {
        url <- sprintf("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=%s&targ=self&form=text&view=brief", GEO)
        message('Trying ', url)
        resp <- httr::GET(url)
        text <- httr::content(resp, "text", "UTF-8")
        check <- grep('Could not', text)
        if (length(check) || httr::status_code(resp) != 200) {
            message('No such dataset: ', name)
            unlink(GEOdir, recursive = TRUE, force = TRUE)
            stop('Failed to download brief data on: ', GEO, '. No such dataset')
        } else {
            writeLines(text, briefFile)
            message('Stored brief data of ', GEO, ' at ', briefFile)
        }
    }
    parsedList <- parseBriefData(readLines(briefFile))
    if (length(parsedList) == 0) {
        file.remove(briefFile)
        stop('Failed to parse brief data on: ', GEO, '. Empty list')
    }

    return (parsedList)
}

parseBriefData <- function(txt) {
    tmp <- txt[grep("!\\w*?_", txt)]
    tmp <- gsub("!\\w*?_",'', tmp)
    first.eq <- regexpr(' = ', tmp)
    tmp <- cbind(substring(tmp, first = 1, last = first.eq - 1),
                 substring(tmp, first = first.eq + 3))
    tmp <- tmp[tmp[,1] != "",]
    header <- split(tmp[,2],tmp[,1])
    return(header)
}

#' Create meta-data for single counts collection
#'
#' Creates a part of counts collections meta-data
#' @param counts_dir path to directory with count collections
#' @param collection_name name of collection and collection's directory
#' @param verbose logical value which determines a content of  the output.
#' @details Function assumes that \code{collection_name} contains \code{meta.txt} which is valid (in sence of \code{\link{validateCountsCollection}}).
#' For each row in \code{meta.txt} function reads specified \code{sample_id} dataset and writes every sample id to the resulting \code{data.table} with source file name and collection name.
#'
#' @return \code{data.table} with meta-data or nothing if \code{destdir} does not exist or does not contain files.
#' @seealso {\code{\link{validateCountsCollection}},\code{\link{getCountsMetaPart}}}
#' @examples
#' \dontrun{
#'     collDir <- "/path/to/my/collection"
#'     valid_collection = validateCountsCollection(collectionDir = collDir, verbose = TRUE)
#'     if (valid_collection){
#'         metaPart = getCountsMetaPart(destdir = collDir, verbose = TRUE)
#'      }
#'  }
#'
#'
#' @import data.table
#' @keywords internal
getCountsMetaPart <- function(counts_dir, collection_name, verbose){
    destdir <- file.path(counts_dir, collection_name)
    if (!dir.exists(destdir)) {
        return()
    }
    DT_h5_meta <- data.table()
    h5_files <- list.files(destdir, "\\.h5$", full.names = FALSE)
    if (!length(h5_files)) {
        return()
    }
    h5_meta <- fread(file.path(destdir, "meta.txt"), index = "file_name")
    subparts <- list()
    for (input_file in h5_files) {
        if (input_file %in% h5_meta$file_name) {
            full_name <- file.path(destdir, input_file)
            relative_path <- file.path(collection_name, input_file )
            h5f <- H5Fopen(full_name, flags = "H5F_ACC_RDONLY")
            h5_part <- data.table(accession = h5read(h5f, h5_meta[file_name == input_file, ]$sample_id),
                                 file = relative_path,
                                 collection_type = collection_name)
            H5Fclose(h5f)
            subparts <- c(subparts, list(h5_part))
            if (verbose) {
                message("added ", nrow(h5_part), " samples from ", file.path(destdir, input_file))
            }
        } else {
            if (verbose) {
                message("!! ", file.path(destdir, input_file), " is ignored")
            }
        }

    }
    DT_h5_meta <- rbindlist(subparts)
    return(DT_h5_meta)
}

isCountsMetaOld <- function( counts_dir = getPhantasusConf("cache_folders")$rnaseq_counts){
    meta_name <- file.path(counts_dir, "meta.rda")
    h5_files <- list.files(file.path(counts_dir), "\\.h5$", full.names = TRUE, recursive = TRUE)
    if (!file.exists(meta_name) && length(h5_files) == 0) {
        return(FALSE)
    }
    meta_time <- as.numeric(file.mtime(meta_name))
    h5_mtime <- if (length(h5_files) > 0) max(unlist(lapply(h5_files, file.mtime))) else -Inf
    list_dirs <-  list.dirs(counts_dir, full.names = FALSE, recursive = TRUE)
    dirs_mtime <- lapply(file.path(counts_dir, list_dirs[-1]), file.mtime)
    if (length(dirs_mtime) > 0) {
        dir_mtime <- max(unlist(dirs_mtime))
    } else {
        dir_mtime <- -Inf
    }
    if (file.exists(meta_name) && meta_time > h5_mtime && meta_time > dir_mtime) {
        return(FALSE)
    }
    return(TRUE)
}

isCountsPriorityValid <- function(counts_dir =  getPhantasusConf("cache_folders")$rnaseq_counts){
    priority_file <- file.path(counts_dir, "counts_priority.txt")
    if (!file.exists(priority_file)){
        h5_files <- list.files(file.path(counts_dir), "\\.h5$", full.names = TRUE, recursive = TRUE)
        if (length(h5_files) > 0){
            return(FALSE)
        } else {
            return(TRUE)
        }
    }
    list_dirs <-  list.dirs(counts_dir, full.names = FALSE, recursive = TRUE)
    list_dirs <- c(".", list_dirs[-1])
    priority <- fread(priority_file)
    if (!(setequal(priority$directory,list_dirs) && length(unique(priority$priority)) == length(priority$priority))) {
        return (FALSE)
    }
    return(TRUE)
}

#' Update meta-data for counts collections
#'
#' Creates \code{meta.rda} file which contain information about all samples in all collections.
#' Also function checks \code{priority.txt} file. This file is used to manage collections with the same samples.
#' @param counts_dir path to counts cache directory
#' @param verbose logical value which determines a content of  the output.
#' @param force logical value wich lets function replace existing \code{meta.rda} file
#' @details  First of all function checks validity of \code{priority.txt} file. \bold{Every} Collection should have \bold{unique} priority.
#' If \code{priority.txt} is not valid function creates new one, setting priorities for each subdirectory(=collection) equal to order in \code{list.dir} output.
#'
#' Function updates \code{meta.rda} if this file is older than at least one \code{.h5} file in counts files.
#' \code{meta.rda} is \code{data.table} which is a result of union \code{data.table}s produced by \code{\link{getCountsMetaPart}} for each collection
#' @seealso {\code{\link{validateCountsCollection}},\code{\link{updateCountsMeta}}}
#' @import data.table
#' @keywords internal
updateCountsMeta <- function(counts_dir =  getPhantasusConf("cache_folders")$rnaseq_counts, force = FALSE, verbose = FALSE){
    if (!dir.exists(counts_dir)) {
        message(paste0('Counts directory ', counts_dir, " does not extist" ))
        return()
    }
    meta_name <- file.path(counts_dir, "meta.rda")
    h5_files <- list.files(file.path(counts_dir), "\\.h5$", full.names = TRUE, recursive = TRUE)
    list_dirs <-  list.dirs(counts_dir, full.names = FALSE, recursive = TRUE)
    list_dirs <- c(".", list_dirs[-1])
    priority_file <- file.path(counts_dir, "counts_priority.txt")

    need_create <- FALSE
    if (!isCountsPriorityValid(counts_dir)){
        message(paste0("!!! Counts priority file is invalid or missed. Create default one."))
        need_create <- TRUE
    }
    if (need_create) {
        priority <- data.table(directory = list_dirs, priority = seq_along(list_dirs))
        write.table(x = priority, file = priority_file, sep = "\t", eol = "\n", row.names = FALSE, col.names = TRUE, quote = FALSE)
    }
    if (!length(h5_files)) {
      return()
    }
    if (!force && !isCountsMetaOld(counts_dir)) {
        return()
    }
    if (file.exists(meta_name)) {
      unlink(meta_name)
    }
    DT_counts_meta <- data.table(matrix(ncol = 3, nrow = 0, dimnames = list( NULL, c("accession", "file", "collection_type"))))
    for (cur_dir in list_dirs) {
        dir_path <- file.path(counts_dir, cur_dir)
        dir_path <- sub(pattern = "/./?$", replacement = "", x =  dir_path)
        cur_files <- list.files(path = dir_path, pattern = "\\.h5", recursive = FALSE )
        if (length(cur_files) == 0) {
          next
        }
        if (!file.exists(file.path(dir_path, "meta.txt"))) {
            if (startsWith(x = tolower(basename(dir_path)), prefix =  "archs4")) {
              updateARCHS4meta(archDir = dir_path)
            } else if (startsWith(x = tolower(basename(dir_path)), prefix = "dee2")) {
              updateDEE2meta(destDir = dir_path)
            }
        }
        message(paste0('Populating ', cur_dir , ' counts meta' ))
        if (!validateCountsCollection(collectionDir = dir_path, verbose = verbose)) {
            message(paste0("!! files in ", cur_dir , " are ignored because there is not correct meta file in this directory."))
            next
        }
        DT_part <- getCountsMetaPart(counts_dir = counts_dir, collection_name = cur_dir, verbose = verbose)
        if (length(DT_part)) {
            DT_counts_meta <- rbindlist(l = list(DT_counts_meta, DT_part))
        }
        rm(DT_part)
    }
    save(DT_counts_meta, file = meta_name, eval.promises = TRUE)
    rm(DT_counts_meta)
}

#' Check a counts collection
#'
#' Function checks existing  and  structure of \code{meta.txt} file in specified counts folder.Also it checks accessibility of specified datasets in  corresponding \code{.h5} files.
#' @param collectionDir path to directory with collection
#' @param verbose logical value which determines a content of  the output.
#' @details  \code{collectionDir} should contain a bunch of \code{.h5} files
#'  and a single \code{meta.txt}.  \code{meta.txt} is \code{.tsv}-like file where for each \code{.h5} exists a row wit columns:
#' \describe{ \item{file_name}{ name of \code{.h5} file in \code{collectionDir}.}
#'            \item{sample_id}{name of dataset in \code{file_name} which contains sample IDs (sample_geo_accession for example).}
#'            \item{sample_dim}{which dimension of the expression matrix in \code{file_name} corresponds to samples. Should be one of \code{c("rows", "columns")} }
#'            \item{gene_id}{name of dataset in \code{file_name} which contains ids for genes and the "meaning" for that ids( column name in result ES). For correct work this dataset should contain unique values. Example: ENSEMBLID:/meta/genes/ensembl_gene_id}
#'            \item{genes_annot}{Names of datasets and their meanings to extract gene-related metadata from \code{file_name}. Can be empty or \code{gene_id}-like values separated with semicolon(;).}
#'            }
#'
#' @import data.table
#' @import rhdf5
#' @keywords internal
validateCountsCollection <- function(collectionDir, verbose=FALSE){
    if (!file.exists(file.path(collectionDir, "meta.txt"))) {
        if (verbose) {
            message(paste0("metafile does not exist in ",  file.path(collectionDir)))
        }
        return(FALSE)
    }

    h5_meta <- fread(file.path(collectionDir, "meta.txt"), index = "file_name")
    for (input_file  in h5_meta$file_name) {
        full_path <- file.path(collectionDir, input_file)
        cur_meta <- h5_meta[file_name == input_file, ]
        if (nrow(cur_meta) > 1) {
            if (verbose) {
                message(paste0("two or more rows in meta file for ", full_path ))
            }
            return(FALSE)
        }
        if (!file.exists(full_path)){
            message("Skipping absent file ", full_path)
            next
        }
        h5f <- H5Fopen(full_path, flags = "H5F_ACC_RDONLY")

        tryCatch({
            is_sample_valid <- H5Lexists(h5f, name =  cur_meta$sample_id)

            if(!is_sample_valid){
                if (verbose) {
                    message(paste0("can't read sample_id in ", full_path ))
                }
                return(FALSE)
            }
            gene_id <- strsplit(cur_meta$gene_id, split = ":")[[1]]
            if (length(gene_id) != 2){
                if (verbose){
                    message(paste0("wrong gene id format for", full_path))
                }
                return(FALSE)
            }
            gene_ids <- if (H5Lexists(h5f, name = gene_id[2])) {
                h5read(h5f, name = gene_id[2])
            } else NULL

            if (length(gene_ids) == 0) {
                if (verbose) {
                    message(paste("can't read gene_id in ", full_path))
                }
                return(FALSE)
            }
            if (length(gene_ids)  != length(unique(gene_ids))) {
                if (verbose) {
                    message(paste("Non-unique gene ids in file", full_path))
                }
                return(FALSE)
            }
        }, finally = {
            H5Fclose(h5f)
        })
    }
    return(TRUE)
}


checkBinValidity <- function(filePath, valid_from) {
  if (!file.exists(filePath)) {
    return(FALSE)
  }

  if (file.info(filePath)$ctime < valid_from) {
    return(FALSE)
  }

  raw_proto_version <- as.raw(PROTOBUF_LAYOUT_VERSION)
  bin_ref <- protolite::serialize_pb(list(raw_proto_version))
  file_head <- readBin(con = filePath,what = raw(), n = length(bin_ref))

  if (!all(bin_ref == file_head)) {
    return(FALSE)
  }

  return(TRUE)
}

getDesignMatrix <- function(designData){
    designMatrix <- do.call(cbind, designData)
    designRownames <- designMatrix[,"id"]

    designMatrix <- subset(designMatrix, select = -c(id))
    designMatrix <- apply(X = designMatrix, FUN = as.numeric, MARGIN =c(2))
    colnames(designMatrix) [colnames(designMatrix) == "intercept"] <- "(Intercept)"
    rownames(designMatrix) <- designRownames
    if(qr(designMatrix)$rank < ncol(designMatrix)){
        stop("Error: redundancy of model parameters appears. Try to exclude nested factors.")
    }
    return(designMatrix)
}

phantasusVersion <- function() {
  jsonlite::toJSON(as.character(utils::packageVersion("phantasus")))
}

generateReleaseJS <- function(destfile, build="none") {
    lines <- c(sprintf("window.PHANTASUS_VERSION='%s';", as.character(utils::packageVersion("phantasus"))),
      sprintf("window.PHANTASUS_BUILD='%s';", build))
    writeLines(lines, destfile)

}
