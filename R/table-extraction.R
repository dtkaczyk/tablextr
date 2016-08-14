#' Extract tables from web pages.
#'
#' The function allows to extract tabular data from a web page. The extraction
#' function can perform additional cleaning and inference:
#' \itemize{
#'   \item taking care of "span" attributes
#'   \item guessing the table headers based on tags' names and attributes
#'   \item merging tables with the same schema
#' }
#'
#' @param url A web page URL.
#' @param findHeaders Whether to guess table headers based on XML tags and
#' attributes.
#' @param mergeTables Whether to merge tables with the same schema.
#' @param tableXPath XPath expression for extracting table nodes.
#' @param rowTag Table row XML tag(s).
#' @param cellTag Table cell XML tag(s).
#' @return A list of tables extracted from the web page.
#' @examples
#' aircrafts <- extractTables("http://www.flugzeuginfo.net/table_accodes_en.php")
#' cssProperties <- extractTables("http://www.w3schools.com/cssref/")
#' @export
extractTables <- function(url, findHeaders = TRUE, mergeTables = TRUE,
                          tableXPath = "//table", rowTag = "tr",
                          cellTag = c("th", "td")) {
    data <- parse(url)
    tableNodes = getNodes(data, xpath = tableXPath)
    tables <- lapply(tableNodes, tableToDataFrame, findHeaders = findHeaders,
                       rowTag = rowTag, cellTag = cellTag)
    if (mergeTables) {
        tables <- mergeTables(tables)
    }
    tables
}
