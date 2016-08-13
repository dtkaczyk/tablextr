library(tablextr)
context("Header extraction")

test_that("setHeaders", {
    df <- data.frame(letters[1:5], as.character(seq(7, 11)), letters[13:17])
    expect_equal(c("a", "7", "m"), colnames(setHeaders(df)))
    expect_equal(c("a b c", "7 8 9", "m n o"), colnames(setHeaders(df, 3)))
})

test_that("findHeaderCount", {
    parsed <- parse(paste0("file://", file.path(getwd(), "test1.html")))
    table <- getNodes(parsed, xpath = "//table")[[1]]
    expect_equal(1, findHeaderCount(table))
    parsed <- parse(paste0("file://", file.path(getwd(), "test2.html")))
    tables <- getNodes(parsed, xpath = "//table")
    expect_equal(0, findHeaderCount(tables[[1]]))
    expect_equal(0, findHeaderCount(tables[[2]]))
    expect_equal(2, findHeaderCount(tables[[10]]))
})

test_that("rowToStyleHash", {
    parsed <- parse(paste0("file://", file.path(getwd(), "test2.html")))
    rows <- getNodes(parsed, xpath = "//table/tr")
    expect_equal("td : colspan", rowToStyleHash(rows[[1]]))
    expect_equal("td : width,td : width", rowToStyleHash(rows[[10]]))
})

test_that("cellToStyleHash", {
    parsed <- parse(paste0("file://", file.path(getwd(), "test2.html")))
    cells <- getNodes(parsed, xpath = "//table/tr/td")
    expect_equal("td : colspan", cellToStyleHash(cells[[1]]))
    expect_equal("td : ", cellToStyleHash(cells[[10]]))
})
