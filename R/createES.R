#' Create ExpressionSet.
#'
#' \code{createES} function produces an ExpressionSet object from given data,
#'     and exports it to global scope.
#'
#' @param data Gene expression matrix.
#'
#' @param pData Matrix with phenotypical data.
#'
#' @param varLabels Names of phenoData columns.
#'
#' @param fData Matrix with feature data.
#'
#' @param fvarLabels Names of featureData columns.
#'
#' @param eData List with experimentData
#'
#' @return produced ExpressionSet object
#'
#' @import Biobase
#' @importFrom methods new
#'
#' @examples
#' \dontrun{
#' data <- matrix(1:15, 5, 3)
#' pData <- c("A", "B", "C")
#' varLabels <- "cat"
#' fData <- c("p", "r", "s", "t", "u")
#' fvarLabels <- "id"
#' eData <- list(name="", lab="", contact="", title="", url="", other=list(), pubMedIds="")
#' createES(data, pData, varLabels, fData, fvarLabels, eData)
#' }
#' @keywords internal
createES <- function(data, pData, varLabels, fData, fvarLabels, eData) {
    if (length(pData) == 0) {
        pData <- matrix(, nrow=ncol(data), ncol=0)
    }
    phenoData <- AnnotatedDataFrame(data.frame(pData, stringsAsFactors = FALSE))
    varLabels(phenoData) <- varLabels

    if (length(fData) == 0) {
        fData <- matrix(, nrow=nrow(data), ncol=0)
    }
    featureData <- AnnotatedDataFrame(data.frame(fData, stringsAsFactors = FALSE))
    varLabels(featureData) <- fvarLabels

    ed <- new ("MIAME",
              name=eData$name,
              lab=eData$lab,
              title=eData$title,
              contact=eData$contact,
              pubMedIds=eData$pubMedIds,
              url=eData$url,
              other=eData$other)

    es <- ExpressionSet(assayData = data,
                        phenoData = phenoData,
                        featureData = featureData,
                        experimentData = ed)

    assign("es", es, envir = parent.frame())
    es
}
