#' Set column names
#'
#' The function sets column names in a data frame using data from the first rows.
#'
#' @param data A data frame.
#' @param headerCount How many rows use as headers.
#' @return A data frame with the first \code{headerCount} rows turned into
#' column names.
#' @examples
#' df <- data.frame(letters[1:5], as.character(seq(7, 11)), letters[13:17])
#' dfWithColnames <- setHeaders(df)
#' dfWithColnames <- setHeaders(df, headerCount = 2)
#' @export
setHeaders <- function(data, headerCount = 1) {
    row.names(data) <- NULL
    if (headerCount > 0) {
        colnames(data) <- apply(data, 2, function(x) {
            stringr::str_trim(paste(x[1:headerCount], collapse = " "))
            })
        data <- data[-c(1:headerCount),]
        row.names(data) <- NULL
    }
    data
}

#' Guess header count
#'
#' The function uses XML tags to guess how many table rows form the table headers.
#'
#' @param table A table XML node.
#' @param rowTag Table row XML tag(s).
#' @param cellTag Table cell XML tag(s).
#' @return The number of the first rows that represent the \code{table} header.
#' @export
findHeaderCount <- function(table, rowTag = "tr", cellTag = c("th", "td")) {
    rows <- getNodes(table, children = rowTag)
    rowStyles <- lapply(rows, rowToStyleHash, cellTag = cellTag)
    rowCount <- length(rows)
    headerCount <- 0
    if (rowCount == 2) {
        if (rowStyles[[1]] != rowStyles[[2]]) {
            headerCount <- 1
        }
    }
    if (rowCount > 2) {
        if (rowStyles[[1]] != rowStyles[[2]] &
            identical(rowStyles[2:(rowCount-1)], rowStyles[3:rowCount])) {
            headerCount <- 1
        }
    }
    if (rowCount == 3) {
        if (rowStyles[[1]] == rowStyles[[2]] & rowStyles[[2]] != rowStyles[[3]]) {
            headerCount <- 2
        }
    }
    if (rowCount > 3) {
        if (rowStyles[[1]] == rowStyles[[2]] & rowStyles[[2]] != rowStyles[[3]] &
            identical(rowStyles[3:(rowCount-1)], rowStyles[4:rowCount])) {
            headerCount <- 2
        }
    }
    headerCount
}

rowToStyleHash <- function(row, cellTag = c("th", "td")) {
    children <- getNodes(row, children = cellTag)
    vect <- vector(mode = "character")
    for (ch in children) {
        vect <- c(vect, cellToStyleHash(ch))
    }
    paste(sort(vect), collapse = ",")
}

cellToStyleHash <- function(cell) {
    paste(getName(cell), ":", getAttrNames(cell))
}
