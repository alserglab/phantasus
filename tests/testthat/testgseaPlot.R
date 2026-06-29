context("GSEA plot")
library(jsonlite)
library(Biobase)

setwd(tempdir()) # put generated files (originally for OpenCPU to serve) to a temp dir

test_that("gseaPlot works", {
    load(file = system.file("testdata/GSE27112-GPL6103.rda", package="phantasus"))

    set.seed(42)
    fData(es)$t <- rnorm(nrow(es))

    f <- fromJSON(gseaPlot(es, rankBy = "t", selectedGenes = sample.int(nrow(es), 10),
             width=6, height=4))
    expect_true(file.exists(f))

    f <- fromJSON(gseaPlot(es, rankBy = "t", selectedGenes = sample.int(nrow(es), 10),
             width=6, height=4, vertical = TRUE))
    expect_true(file.exists(f))

    f <- fromJSON(gseaPlot(es, rankBy = "t", selectedGenes = sample.int(nrow(es), 10),
             width=6, height=4, vertical = TRUE,
             addHeatmap = TRUE, showAnnotation = "time"))
    expect_true(file.exists(f))

    f <- fromJSON(gseaPlot(es, rankBy = "t", selectedGenes = sample.int(nrow(es), 10),
             width=6, height=4, vertical = FALSE,
             addHeatmap = TRUE, showAnnotation = "time"))
    expect_true(file.exists(f))

    f <- fromJSON(gseaPlot(es, rankBy = "t", selectedGenes = sample.int(nrow(es), 10),
                           width=6, height=4, vertical = FALSE,
                           addHeatmap = TRUE))
    expect_true(file.exists(f))
})

test_that("gseaPlot with heatmap works for single column", {
    load(file = system.file("testdata/GSE27112-GPL6103.rda", package="phantasus"))
    es <- es[, 1]
    set.seed(42)
    fData(es)$t <- rnorm(nrow(es))

    f <- fromJSON(gseaPlot(es, rankBy = "t", selectedGenes = sample.int(nrow(es), 10),
                           width=6, height=4, addHeatmap = TRUE))
    expect_true(file.exists(f))
})
