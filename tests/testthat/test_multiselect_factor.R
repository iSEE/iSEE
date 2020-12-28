# Tests the multiSelectionToFactor function.
# library(testthat); library(iSEE); source("test_multiselect_factor.R")

test_that("multiSelectionToFactor works as expected", {
    out <- multiSelectionToFactor(list(active=c("A", "B"), 
        saved1=c("B", "C"), saved2=c("D", "E", "F")),
        all.names=LETTERS[1:10])

    expect_identical(as.character(out[1]), "active")
    expect_identical(as.character(out[2]), "active,saved1")
    expect_identical(as.character(out[3]), "saved1")
    expect_identical(as.character(out[4:6]), rep("saved2", 3))
    expect_identical(as.character(tail(out, 4)), rep("unselected", 4))
})

test_that("multiSelectionToFactor works with inclusion in >2 sets", {
    out <- multiSelectionToFactor(list(active=c("A"), 
        saved1=c("A", "B"), saved2=c("A", "C")),
        all.names=LETTERS[1:4])

    expect_identical(as.character(out[1]), "active,saved1,saved2")
    expect_identical(as.character(out[2]), "saved1")
    expect_identical(as.character(out[3]), "saved2")
    expect_identical(as.character(out[4]), "unselected")
})

