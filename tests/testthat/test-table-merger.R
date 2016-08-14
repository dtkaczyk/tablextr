library(tablextr)
context("Table merging")

test_that("mergeTables", {
    df1 <- data.frame("First" = letters[1:5], "Second" = as.character(seq(7, 11)),
                      "Third" = letters[13:17])
    df2 <- data.frame("First" = letters[15:19], "Second" = as.character(seq(17, 21)),
                      "Third" = letters[13:17])
    df3 <- data.frame(letters[1:2], as.character(seq(7, 8)), letters[13:14])
    df4 <- data.frame("First" = letters[1:3], "Second" = as.character(seq(7, 9)),
                      "Third" = letters[17:19])
    df5 <- data.frame("First" = letters[1:5], "Second" = as.character(seq(7, 11)),
                      "Third" = letters[13:17])
    df6 <- data.frame(letters[1:5], as.character(seq(7, 11)), letters[13:17])
    df7 <- data.frame("First" = letters[1:11], "Second" = as.character(seq(7, 17)),
                      "Third" = letters[13:23])
    merged <- mergeTables(list(df1, df2, df3, df4, df5, df6, df7))
    expect_equal(c("First,Second,Third", "table1", "table2"), names(merged))
    expect_equal(c(29, 3), dim(merged[[1]]))
    expect_equal(df3, merged[[2]])
    expect_equal(df6, merged[[3]])
})

test_that("getTableHash", {
    df <- data.frame(letters[1:5], as.character(seq(7, 11)), letters[13:17])
    expect_equal("letters.1.5.,as.character.seq.7..11..,letters.13.17.",
                 getTableHash(df))
    df <- data.frame("First" = letters[1:5], "Second" = as.character(seq(7, 11)),
                     "Third" = letters[13:17])
    expect_equal("First,Second,Third", getTableHash(df))
})
