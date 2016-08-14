library(tablextr)
context("Table tools")

test1Path <- paste0("file://", system.file("extdata", "test1.html",
                                           package="tablextr"))
test2Path <- paste0("file://", system.file("extdata", "test2.html",
                                           package="tablextr"))

test_that("tableToDataFrame", {
    parsed <- parse(test1Path)
    table <- getNodes(parsed, xpath = "//table")[[1]]
    df <- tableToDataFrame(table)
    expect_equal(c(196, 4), dim(df))
    expect_equal(c("ICAO", "Manufacturer", "Type/Model", "Wake"), colnames(df))
    df <- tableToDataFrame(table, findHeaders = FALSE)
    expect_equal(c(197, 4), dim(df))
    expect_equal(c("V1", "V2", "V3", "V4"), colnames(df))
    parsed <- parse(test2Path)
    tables <- getNodes(parsed, xpath = "//table")
    df <- tableToDataFrame(tables[[1]])
    expect_equal(c(3, 8), dim(df))
    expect_equal(c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8"), colnames(df))
    df <- tableToDataFrame(tables[[2]])
    expect_equal(c(1, 1), dim(df))
    expect_equal(c("V1"), colnames(df))
    df <- tableToDataFrame(tables[[10]])
    expect_equal(c(17, 4), dim(df))
    expect_equal(c("IATA Code", "ICAO Code", "Manufacturer and Aircraft Type / Model",
                   "Wake Category"), colnames(df))
})

test_that("rowToContent", {
    parsed <- parse(test2Path)
    rows <- getNodes(parsed, xpath = "//table/tr")
    expect_equal(c("142", "B462", "BAe 146-200 Pax", "M"),
                 rowToContent(rows[[15]]))
    expect_equal(c("72X", "B721", "Boeing 727-100 Freighter", "M"),
                 rowToContent(rows[[60]]))
})

test_that("cellToContent", {
    parsed <- parse(test2Path)
    cells <- getNodes(parsed, xpath = "//table/tr/td")
    expect_equal("Country Codes", cellToContent(cells[[10]]))
    expect_equal("A310", cellToContent(cells[[104]]))
})
