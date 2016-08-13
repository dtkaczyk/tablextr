library(tablextr)
context("XML tools")

test1Path <- paste0("file://", system.file("extdata", "test1.html",
                                           package="tablextr"))

test_that("parse", {
    parsed <- parse(test1Path)
    expect_equal(
        c("HTMLInternalDocument", "HTMLInternalDocument", "XMLInternalDocument",
          "XMLAbstractDocument"),
        class(parsed))
})

test_that("getNodes xpath", {
    parsed <- parse(test1Path)
    tds <- getNodes(parsed, xpath = "//td")
    expect_equal(7548, length(tds))
    divs <- getNodes(parsed, xpath = "//div/div")
    expect_equal(17, length(divs))
})

test_that("getNodes children", {
    parsed <- parse(test1Path)
    head <- getNodes(parsed, xpath = "/html/head")[[1]]
    children <- getNodes(head, children = c("meta", "link"))
    expect_equal(9, length(children))
})

test_that("getValue", {
    parsed <- parse(test1Path)
    title <- getNodes(parsed, xpath = "//head/title")[[1]]
    expect_equal("ICAO Aircraft Codes - flugzeuginfo.net", getValue(title))
})

test_that("getName", {
    parsed <- parse(test1Path)
    title <- getNodes(parsed, xpath = "//head/title")[[1]]
    expect_equal("title", getName(title))
})

test_that("getAttrNames", {
    parsed <- parse(test1Path)
    link <- getNodes(parsed, xpath = "//link")[[1]]
    expect_equal(c("href", "rel", "type"), getAttrNames(link))
})

test_that("getAttrValue", {
    parsed <- parse(test1Path)
    link <- getNodes(parsed, xpath = "//link")[[1]]
    expect_equal("./css/flexible-grids.css", getAttrValue(link, "href"))
    expect_equal("stylesheet", getAttrValue(link, "rel"))
    expect_equal("text/css", getAttrValue(link, "type"))
})
