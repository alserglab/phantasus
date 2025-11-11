context("Load GEO and its utils")
library(jsonlite)
library(Biobase)
library(data.table)
Sys.setenv(R_USER_CONFIG_DIR = system.file("/testdata/config", package = "phantasus"))
test_that("loadGEO finishes with result", {
    old_conf <- Sys.getenv("R_CONFIG_ACTIVE")
    Sys.setenv(R_CONFIG_ACTIVE = "default")
    x <- loadGEO("GSE27112")
    expect_is(x, "json")
    cacheDir <- getPhantasusConf("cache_root")
    binPath <- file.path(cacheDir, fromJSON(x))
    ess <- protolite::unserialize_pb(readBin(binPath, what="raw", n=100000000))$ess
    expect_equal(length(ess), 2)

    x <- loadGEO("GSE27112-GPL6885")
    expect_is(x, "json")

    binPath <- file.path(cacheDir, fromJSON(x))
    ess <- protolite::unserialize_pb(readBin(binPath, what="raw", n=100000000))$ess

    expect_equal(length(ess), 1)

    expect_is(loadGEO("GSE14308"), "json")
    expect_is(loadGEO("GDS4885"), "json")

    expect_error(loadGEO("WRONGNAME"))
    Sys.setenv(R_CONFIG_ACTIVE = old_conf)

})

test_that("getGDS adds id field for GDS datasets", {
    old_conf <- Sys.getenv("R_CONFIG_ACTIVE")
    Sys.setenv(R_CONFIG_ACTIVE = "default")
    a <- getGDS("GDS4885")[[1]]
    expect_true("id" %in% tolower(fvarLabels(a)))
    Sys.setenv(R_CONFIG_ACTIVE = old_conf)
})

test_that("filterPhenoAnnotations saves colnames", {
    old_conf <- Sys.getenv("R_CONFIG_ACTIVE")
    Sys.setenv(R_CONFIG_ACTIVE = "test_real_geo")
    es <- getES("GSE53986")[[1]]
    expect_true(all(colnames(es) == colnames(exprs(es))))
    Sys.setenv(R_CONFIG_ACTIVE = old_conf)
})

test_that("reparseCachedGSEs works", {
    old_conf <- Sys.getenv("R_CONFIG_ACTIVE")
    Sys.setenv(R_CONFIG_ACTIVE = "test_real_geo")
    getES("GSE14308")
    expect_true("GSE14308" %in% reparseCachedESs(destdir = getPhantasusConf("cache_folders")$geo_path))
    Sys.setenv(R_CONFIG_ACTIVE = old_conf)
})

test_that("checkGPLs counts gpls correctly", {
    old_conf <- Sys.getenv("R_CONFIG_ACTIVE")
    Sys.setenv(R_CONFIG_ACTIVE = "default")
    expect_equal(fromJSON(checkGPLs("GSE14308")), c("GSE14308"))
    expect_equal(fromJSON(checkGPLs("GDS4885")), c("GDS4885"))
    expect_length(fromJSON(checkGPLs("GSE27112")), 2)
    expect_length(fromJSON(checkGPLs("GSE10000")), 2)
    expect_warning(checkGPLs("GSE0"))
    Sys.setenv(R_CONFIG_ACTIVE = old_conf)

})

test_that("checkGPLs works with fully specified name", {
    old_conf <- Sys.getenv("R_CONFIG_ACTIVE")
    Sys.setenv(R_CONFIG_ACTIVE = "default")
    expect_equal(fromJSON(checkGPLs("GSE27112-GPL6885")), c("GSE27112-GPL6885"))
    Sys.setenv(R_CONFIG_ACTIVE = old_conf)
})

# TODO: adapt to new checkGPLs
#test_that("checkGPLs counts existing files correctly without connection", {
    #options(phantasusMirrorPath = "https://alserglab.wustl.edu/files/phantasus")
    #options(phantasusCacheDir = tempfile())
    #expect_length(fromJSON(checkGPLs("GSE27112")), 2)
    #options(phantasusMirrorPath = "https://notworkingdomain")

    #expect_message(checkGPLs("GSE14308"), regexp = "Problems establishing connection")
    #expect_length(fromJSON(checkGPLs("GSE27112")), 2)

    #options(phantasusCacheDir = NULL,
            #phantasusMirrorPath = NULL)
#})

test_that("getGSE works with ARCHS4", {
    ess <- getGSE("GSE99709", destdir=system.file("testdata", package="phantasus"), mirrorPath = "https://ftp.ncbi.nlm.nih.gov")
    expect_gt(nrow(ess[[1]]), 0)
    expect_gt(ncol(ess[[1]]), 0)
})

test_that("InferConditionImpl  works correctly", {
    tests <- fread(system.file("testdata/dts.tsv", package="phantasus"))
    test_ds <- data.table(title=tests$Title, series=tests$Series, accession=tests$Accession, rep=tests$Replicate, inferCondition=tests$InferCondition)
    cond <- split(test_ds$title, test_ds$series)
    inf_cond_test <- split(test_ds$inferCondition, test_ds$series)
    rep_test <- split(test_ds$rep, test_ds$series)
    new_cond <- lapply(cond, inferConditionImpl)
    expect_equal(new_cond$GSE100221, list()) # text in all titles is unique
    expect_equal(new_cond$GSE10380, list())  # long dataset
    expect_equal(new_cond$GSE10382, list())  # number-only titles
    expect_equal(new_cond$GSE10383, list())  # two-color datasets
    expect_equal(new_cond$GSE10385, list())  # the same text and replicate number in all titles
    expect_equal(new_cond$GSE10039, list())  # ambiguous replicate number "High_Mo_seg_pool_Ler_col_F2" "Low_Mo_seg_pool_Ler_col_F2"  "Col-0 3"
    expect_equal(new_cond$GSE101508$condition, inf_cond_test$GSE101508) #"IFNγ+LPS rep2" -> "IFNγ+LPS" + "2"
    expect_equal(new_cond$GSE101508$replicate, as.character(rep_test$GSE101508))
    expect_equal(new_cond$GSE10392$condition, inf_cond_test$GSE10392) # "MPA 1" - > "MPA" + "1"
    expect_equal(new_cond$GSE10392$replicate, as.character(rep_test$GSE10392))
    expect_equal(new_cond$GSE10123$condition, inf_cond_test$GSE10123) # "WT-GFP-lamin A Induction: Day 0 Replicate A" -> "WT-GFP-lamin A Induction: Day 0" + "A"
    expect_equal(new_cond$GSE10123$replicate, as.character(rep_test$GSE10123))


})

test_that("getGPLAnnotation works with errorneous empty annotation files", {
    destdir <- file.path(tempdir(), "cache_bad")
    dir.create(destdir, showWarnings = FALSE)

    GPL <- "GPL17021"
    stub = gsub('\\d{1,3}$','nnn',GPL,perl=TRUE)
    GPLDirPath <- '%s/geo/platforms/%s/%s/annot'
    fullGPLDirPath <- file.path(sprintf(GPLDirPath, destdir, stub, GPL))

    dir.create(fullGPLDirPath, showWarnings = FALSE, recursive = TRUE)
    file.create(file.path(fullGPLDirPath, paste0(GPL, ".annot.gz")))

    gpl <- getGPLAnnotation(GPL, destdir)
    expect_true(!is.null(gpl))
})

test_that("getGSEType works", {
    expect_true(checkGSEType('GSE53986', tempdir()))
    expect_true(checkGSEType('GSE99709', tempdir()))
    expect_true(checkGSEType('GSE33356', tempdir()))
    expect_identical(checkGSEType('GSE33356', tempdir(), combine=identity),
                     c(GPL570=TRUE, GPL6801=FALSE))
    expect_false(checkGSEType('GSE33356-GPL6801', tempdir()))
})

test_that("filterPhenoAnnotations does not split title", {
    old_conf <- Sys.getenv("R_CONFIG_ACTIVE")
    Sys.setenv(R_CONFIG_ACTIVE = "test_real_geo")
    ess <- getGSE("GSE120978")
    expect_true("title" %in% varLabels(ess[[1]]))
    Sys.setenv(R_CONFIG_ACTIVE = old_conf)
})

test_that("filterPhenoAnnotations parses characteristics", {
    load(file = system.file("testdata/GSE120978_unfiltered_pData.rda", package="phantasus"))
    es <- ess[[1]]
    empty_strings <- sample(1:ncol(es), 3, replace=FALSE)
    pData(es)[empty_strings,"characteristics_ch1" ] <- ""
    old_values <-  pData(es)$"characteristics_ch1"[-empty_strings]
    es <- filterPhenoAnnotations(es)
    expect_true(all(c("gender", "strain", "tissue") %in% varLabels(es)))
    expect_true(all(is.na(pData(es)$gender[empty_strings])))
    new_values <- pData(es)$gender[-empty_strings]
    expect_true(all(old_values == paste0("gender: ", new_values)))
})


test_that("checkGPLs returns valid GSE for GSE-GPL when there is only one GPL", {
    expect_equal(fromJSON(checkGPLs("GSE14308-GPL1261")), c("GSE14308"))

})

test_that("weird sample annotations are parse correctly", {
    # multiple entries for "treatment" annotation for the same sample
    es <- getGEO("GSE180475", getGPL = FALSE, AnnotGPL = FALSE)[[1]]
    es1 <- filterPhenoAnnotations(es)
    expect_equal(es1$treatment[1], "KD GFP /// shGFP")

    # empty entries? gitlab issue #83
    es <- getGEO("GSE153557", getGPL = FALSE, AnnotGPL = FALSE)[[1]]
    es1 <- filterPhenoAnnotations(es)
    expect_true(!is.null(es1))

    # empty entries? gitlab issue #83
    es <- getGEO("GSE78913", getGPL = FALSE, AnnotGPL = FALSE)[[2]]
    es1 <- filterPhenoAnnotations(es)
    expect_true(!is.null(es1))
})
