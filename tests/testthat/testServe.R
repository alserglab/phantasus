context("servePhantasus")

skip_on_os(c("windows", "mac"))
skip_on_bioc()

# Set up a writable config so areCacheFoldersValid passes.
cfgDir <- file.path(tempdir(), "phantasus-test-config", "R", "phantasus")
dir.create(cfgDir, recursive = TRUE, showWarnings = FALSE)
cacheRoot <- file.path(tempdir(), "phantasus-test-cache")
invisible(lapply(
    c(cacheRoot, file.path(cacheRoot,
                           c("geo", "annotationdb", "fgsea", "counts"))),
    dir.create, recursive = TRUE, showWarnings = FALSE
))
writeLines(paste0(
    "default:\n",
    "  host: \"0.0.0.0\"\n",
    "  preloaded_dir: NULL\n",
    "  static_root: \"",
    system.file("www/phantasus.js", package = "phantasus"), "\"\n",
    "  cache_root: \"", cacheRoot, "\"\n",
    "  cache_folders:\n",
    "    geo_path: \"", file.path(cacheRoot, "geo"), "\"\n",
    "    annot_db: \"", file.path(cacheRoot, "annotationdb"), "\"\n",
    "    fgsea_pathways: \"", file.path(cacheRoot, "fgsea"), "\"\n",
    "    rnaseq_counts: \"", file.path(cacheRoot, "counts"), "\"\n"
), file.path(cfgDir, "user.conf"))
Sys.setenv(R_USER_CONFIG_DIR =
               file.path(tempdir(), "phantasus-test-config"))
Sys.setenv(R_CONFIG_ACTIVE = "default")
suppressMessages(setupPhantasus())

test_that("background=TRUE with multiple ports errors", {
    expect_error(
        servePhantasus(port = 18790:18791, background = TRUE,
                       openInBrowser = FALSE),
        "background"
    )
})
