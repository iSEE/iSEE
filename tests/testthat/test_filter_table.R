# This tests the various filterDT utilities.
# library(testthat); library(iSEE); source("test_filter_table.R")

test_that("filterDTColumn works for numeric inputs", {
    Y <- runif(100)

    expect_identical(filterDTColumn(Y, "0.5 ... 0.99"), Y >= 0.5 & Y <= 0.99)
    expect_identical(filterDTColumn(Y, "0.1 ... 0.2"), Y >= 0.1 & Y <= 0.2)
    expect_identical(filterDTColumn(Y, "0.5 ... 0.2"), Y >= 0.5 & Y <= 0.1)

    expect_warning(out <- filterDTColumn(Y, "0.5 ... AAA"), "not a valid")
    expect_true(all(out))

    # Handles NA values smoothly. 
    expect_identical(filterDTColumn(c(NA, Y), "0.5 ... 0.99"), c(FALSE, filterDTColumn(Y, "0.5 ... 0.99"))) 
})

test_that("filterDTColumn works for character inputs", {
    stuff <- c("Aaron", "Kevin", "Charlotte", "Federico")

    expect_identical(filterDTColumn(stuff, "a"), c(TRUE, FALSE, TRUE, FALSE))
    expect_identical(filterDTColumn(stuff, "A"), c(TRUE, FALSE, FALSE, FALSE))
    expect_identical(filterDTColumn(stuff, "e"), c(FALSE, TRUE, TRUE, TRUE))
    expect_identical(filterDTColumn(stuff, "a.*o"), c(TRUE, FALSE, TRUE, FALSE))
    expect_identical(filterDTColumn(stuff, "A|K"), c(TRUE, TRUE, FALSE, FALSE))

    # Handles NA values smoothly. 
    expect_identical(filterDTColumn(c(NA, stuff), "A|K"), c(FALSE, filterDTColumn(stuff, "A|K")))
})

test_that("filterDTColumn works for factor inputs", {
    stuff <- factor(c("Aaron", "Kevin", "Charlotte", "Federico"))
    expect_identical(filterDTColumn(stuff, "[\"Aaron\"]"), stuff=="Aaron") 
    expect_identical(filterDTColumn(stuff, "[\"Aaron\", \"Charlotte\"]"), stuff %in% c("Aaron", "Charlotte")) 

    # Continues to work for logicals.
    expect_identical(filterDTColumn(c(TRUE, FALSE), "[\"true\"]"), c(TRUE, FALSE))
    expect_identical(filterDTColumn(c(TRUE, FALSE), "[\"false\"]"), c(FALSE, TRUE))

    # Handles NA values smoothly.
    expect_identical(filterDTColumn(stuff[c(NA, seq_along(stuff))], "[\"Aaron\"]"), c(FALSE, stuff=="Aaron"))
})

df <- data.frame(
    stuff=sample(c("Aaron", "Kevin", "Charlotte", "Federico"), 20, replace=TRUE),
    thing=runif(20),
    blah=sample(LETTERS[1:3], 20, replace=TRUE),
    foo=sample(22:28, 20, replace=TRUE),
    stringsAsFactors=FALSE
)

test_that("global search of filterDT works correctly", {
    expect_identical(
        filterDT(df, global="A", column=""),
        grepl("A", df$stuff) | grepl("A", df$blah)
    )

    expect_identical(
        filterDT(df, global="22", column=""),
        grepl("22", df$thing) | grepl("22", df$foo)
    )

    # Uses the row names.
    expect_identical(
        filterDT(df, global="^5$", column=""),
        rownames(df)=="5"
    )

    # Handles logicals.
    df$whee <- c(TRUE, FALSE)
    expect_identical(
        filterDT(df, global="FALSE", column=""),
        !df$whee
    )

    # Handles factors.
    df$whee <- factor(sample(c("X", "Y", "Z"), 20, replace=TRUE))
    expect_identical(
        filterDT(df, global="X|Y", column=""),
        grepl("X|Y", as.character(df$whee))
    )
})

test_that("column search of filterDT works correctly", {
    complete <- c("o", "0.1 ... 0.5", "B|C", "2 ... 7")

    expect_identical(
        filterDT(df, global="", column=head(complete, 1)),
        cumulative <- grepl("o", df$stuff) 
    )

    expect_identical(
        filterDT(df, global="", column=head(complete, 2)),
        cumulative <- cumulative & (df$thing >= 0.1 & df$thing <= 0.5) 
    )

    expect_identical(
        filterDT(df, global="", column=head(complete, 3)),
        cumulative <- cumulative & grepl("B|C", df$blah)
    )

    expect_identical(
        filterDT(df, global="", column=complete),
        cumulative <- cumulative & (df$foo >= 2 & df$foo <= 7)
    )

    # Adding more stuff on the end has no effect. 
    expect_identical(
        filterDT(df, global="", column=c(complete, "whee")),
        filterDT(df, global="", column=complete)
    )
})

test_that("global and column searches interact correctly", {
    column_only <- filterDT(df, global="", column="o")
    global_only <- filterDT(df, global="A", column="")

    expect_identical(filterDT(df, global="A", column="o"),
        column_only & global_only)
})

test_that("filterDT behaves correctly with NAs", {
    boogered <- df
    boogered$stuff <- NA_character_

    # 'blah' is the only non-NA letter'd column left to regex on.
    expect_identical(
        filterDT(boogered, global="A", column=""),
        grepl("A", boogered$blah)
    )
})
