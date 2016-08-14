#' Convert XML node to data frame
#'
#' The function converts "table" XML node to a data frame.
#'
#' @param table XML node.
#' @param findHeaders Whether to guess table headers based on XML tags and
#' attributes.
#' @param rowTag Table row XML tag(s).
#' @param cellTag Table cell XML tag(s).
#' @return A data frame containing the data from the original XML node.
#' @export
tableToDataFrame <- function(table, findHeaders = TRUE, rowTag = "tr",
                             cellTag = c("th", "td")) {
    rows <- getNodes(table, children = rowTag)
    content <- lapply(rows, rowToContent, cellTag = cellTag)
    maxLen <- max(sapply(content, length))
    content <- lapply(content, function(x) {c(x, rep("", maxLen-length(x)))})
    content <- as.data.frame(matrix(unlist(content), ncol = maxLen, byrow = TRUE))
    if (findHeaders) {
        headerCount <- findHeaderCount(table, rowTag = rowTag, cellTag = cellTag)
        content <- setHeaders(content, headerCount = headerCount)
    }
    content
}

rowToContent <- function(row, cellTag = c("th", "td")) {
    cells <- getNodes(row, children = cellTag)
    content <- c()
    for (cell in cells) {
        content <- c(content, cellToContent(cell))
    }
    content
}

cellToContent <- function(cell) {
    content <- getValue(cell)
    span <- getAttrValue(cell, 'colspan')
    if (!is.null(span)) {
        span <- as.numeric(span)
        content <- c(content, rep("", span-1))
    }
    content
}
