#' Merge data frames
#'
#' The function merges data frames based on column names.
#'
#' @param tables A list of data frames.
#' @return A list of data frames, each of which is one of the original data
#' frames from \code{tables} or a union of the original data frames.
#' @examples
#' mergeTables(tables)
#' @export
mergeTables <- function(tables) {
    tableNames <- table(sapply(tables, getTableHash))
    merged <- list()
    i <- 1
    for (table in tables) {
        tableHash <- getTableHash(table)
        if (colnames(table)[1] == "V1" | tableNames[[tableHash]] < 5) {
            merged[[paste0("table", i)]] <- table
            i <- i + 1
        } else if (tableHash %in% names(merged)) {
            merged[[tableHash]] <- rbind(merged[[tableHash]], table)
            row.names(merged[[tableHash]]) <- NULL
        } else {
            merged[[tableHash]] <- table
        }
    }
    merged
}

getTableHash <- function(table) {
    paste(colnames(table), collapse=",")
}
