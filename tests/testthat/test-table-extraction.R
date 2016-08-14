library(tablextr)
context("Table extraction")

test1Path <- paste0("file://", system.file("extdata", "test1.html",
                                           package="tablextr"))
test2Path <- paste0("file://", system.file("extdata", "test2.html",
                                           package="tablextr"))
test3Path <- paste0("file://", system.file("extdata", "test3.html",
                                           package="tablextr"))
test4Path <- paste0("file://", system.file("extdata", "test4.html",
                                           package="tablextr"))

test_that("extractTables 1", {
    tables <- extractTables(test1Path)
    expect_equal(1, length(tables))
    expect_equal(c("ICAO", "Manufacturer", "Type/Model", "Wake"),
                 names(tables[[1]]))
    expect_equal(c(1887, 4), dim(tables[[1]]))
})

test_that("extractTables 2", {
    tables <- extractTables(test2Path)
    expect_equal(8, length(tables))
    expect_equal(
        c("IATA Code", "ICAO Code", "Manufacturer and Aircraft Type / Model",
          "Wake Category"),
        names(tables[[1]]))
    expect_equal(c(326, 4), dim(tables[[1]]))
})

test_that("extractTables 3", {
    tables <- extractTables(test3Path)
    expect_equal(1, length(tables))
    expect_equal(c("Property", "Description", "CSS"), names(tables[[1]]))
    expect_equal(c(230, 3), dim(tables[[1]]))
})

test_that("extractTables 4", {
    tables <- extractTables(test4Path)
    expect_equal(2, length(tables))
    expect_equal(
        c("Rank", "Country (or dependent territory)", "Population", "Date",
          "% of world\npopulation", "Source"),
        names(tables[[1]]))
    expect_equal(c(249, 6), dim(tables[[1]]))
})
