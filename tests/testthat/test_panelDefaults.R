# This tests the panelDefaults function.
# library(testthat); library(iSEE); source("test_panelDefaults.R")

test_that("panelDefaults works as expected", {
    old <- panelDefaults(FontSize=2)
    expect_identical(old$FontSize, 1)
    expect_identical(getPanelDefault("FontSize"), 2)

    panelDefaults(old)
    expect_identical(getPanelDefault("FontSize"), 1)
    
    panelDefaults(FontSize=3, SelectionAlpha=0.01)
    expect_identical(getPanelDefault("FontSize"), 3)
    expect_identical(getPanelDefault("SelectionAlpha"), 0.01)
    expect_identical(getPanelDefault("Assay"), "logcounts")

    panelDefaults(list(FontSize=4, SelectionAlpha=0.25))
    expect_identical(getPanelDefault("FontSize"), 4)
    expect_identical(getPanelDefault("SelectionAlpha"), 0.25)
    expect_error(getPanelDefault("Assay")) # because it's a total replacement.

    panelDefaults(old)
    expect_identical(getPanelDefault("FontSize"), 1)
    expect_identical(getPanelDefault("SelectionAlpha"), 0.1)
    expect_identical(getPanelDefault("Assay"), "logcounts")

    out <- panelDefaults()
    expect_identical(out$FontSize, 1)
    expect_identical(out$SelectionAlpha, 0.1)
    expect_identical(out, panelDefaults()) # doesn't change the values.

    expect_error(getPanelDefault("random"), "could not find")
    expect_null(getPanelDefault("random", error=FALSE))
})

test_that("panelDefaults works with new arguments", {
    old <- panelDefaults(Other1=1, Other2=2)
    expect_identical(getPanelDefault("Other1"), 1)
    expect_identical(getPanelDefault("Other2"), 2)

    panelDefaults(old)
    expect_null(getPanelDefault("Other1", error=FALSE))
    expect_null(getPanelDefault("Other2", error=FALSE))
})

test_that("panelDefaults responds to iSEEOptions for back-compatibility's sake", {
    expect_warning(iSEEOptions$set(font.size=5))
    expect_identical(getPanelDefault("FontSize"), 5)
    iSEEOptions$restore()
})

