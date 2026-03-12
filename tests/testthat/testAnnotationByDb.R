context("Convert by AnnotationDb")
library(Biobase)
library(data.table)
library(phantasusLite)

# Write a temp config with annot_db as a literal path so that
# config::get() can resolve it without the devtools system.file shim.
local({
    annotDir <- system.file("testdata/annotationdb/", package = "phantasus")
    cfgDir <- file.path(tempdir(), "phantasus-annottest-config", "R", "phantasus")
    dir.create(cfgDir, recursive = TRUE, showWarnings = FALSE)
    writeLines(paste0(
        "default:\n",
        "  host: \"0.0.0.0\"\n",
        "  preloaded_dir: NULL\n",
        "  static_root: \"",
            system.file("www/phantasus.js", package = "phantasus"), "\"\n",
        "  cache_root: \"", tempdir(), "\"\n",
        "  cache_folders:\n",
        "    geo_path: \"", file.path(tempdir(), "geo"), "\"\n",
        "    annot_db: \"", annotDir, "\"\n",
        "    fgsea_pathways: \"", file.path(tempdir(), "fgsea"), "\"\n",
        "    rnaseq_counts: \"", file.path(tempdir(), "counts"), "\"\n"
    ), file.path(cfgDir, "user.conf"))
    Sys.setenv(R_USER_CONFIG_DIR =
                   file.path(tempdir(), "phantasus-annottest-config"))
    Sys.setenv(R_CONFIG_ACTIVE = "default")
})
test_that("AnnotationbyDb works with delete version", {
    test_file <- system.file("testdata/counts_versioned_ids.gct", package="phantasus")
    if (test_file == ""){
        stop("test counts file doesn't exists")
    }
    es <- readGct(test_file)
    dbName <-"sample_mouse_db.sqlite"
    columnName <- "id"
    columnType <- "ENSEMBL"
    keyType <- "SYMBOL"
    otherOptions <- list(deleteDotVersion = FALSE)
    expect_error(convertByAnnotationDB(es, dbName, columnName, columnType, keyType, otherOptions))
    otherOptions <- list(deleteDotVersion = TRUE)
    symbols <- jsonlite::fromJSON(convertByAnnotationDB(es, dbName, columnName, columnType, keyType, otherOptions))
    expect_gt(sum(!is.na(symbols)),0)

})
