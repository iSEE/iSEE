# library(iSEE); library(testthat); source("test_brush.R")

context("brush")

# .identical_brushes ----

test_that(".identical_brushes works as expected", {
    example <- list(xmin = 8.0882520175732, xmax = 43.722129933505, ymin = -4.2882334274045, ymax = 39.525409385225, 
        coords_css = list(xmin = 283.009614944458, xmax = 416.009614944458, ymin = 75.0096130371094, 
            ymax = 230.009613037109), coords_img = list(xmin = 367.916474130582, xmax = 540.818342036981, 
            ymin = 97.5124933715051, ymax = 299.012485980529), img_css_ratio = list(x = 1.30001404440901, 
            y = 1.29999995231628), mapping = list(x = "X", y = "Y"), domain = list(left = -57.2288623709444, 
            right = 49.1019902822796, bottom = -70.389479254644, top = 53.5190834641125), 
        range = list(left = 50.986301369863, right = 566.922374429224, bottom = 603.013698630137, 
            top = 33.1552511415525), log = list(x = NULL, y = NULL), direction = "xy", 
        brushId = "RedDimPlot1_Brush", outputId = "RedDimPlot1")

    expect_true(iSEE:::.identical_brushes(old_brush=NULL, new_brush=NULL))
    expect_false(iSEE:::.identical_brushes(old_brush=NULL, new_brush=example))
    expect_false(iSEE:::.identical_brushes(old_brush=example, new_brush=NULL))
    expect_true(iSEE:::.identical_brushes(old_brush=example, new_brush=example))

    # Confirming correct failure.
    expect_false(iSEE:::.identical_brushes(list(xmin=1, xmax=2, ymin=10, ymax=20),
        list(xmin=1, xmax=2, ymin=11, ymax=20)))
    expect_false(iSEE:::.identical_brushes(list(xmin=1, xmax=2, ymin=10, ymax=20),
        list(xmin=1, xmax=2, ymin=10, ymax=21)))
    expect_false(iSEE:::.identical_brushes(list(xmin=1, xmax=2, ymin=10, ymax=20),
        list(xmin=0, xmax=2, ymin=10, ymax=20)))
    expect_false(iSEE:::.identical_brushes(list(xmin=1, xmax=2, ymin=10, ymax=20),
        list(xmin=1, xmax=3, ymin=10, ymax=20)))

    # Robust to small deviations.
    example2 <- example
    example2$xmin <- example2$xmin + 1e-8
    expect_true(iSEE:::.identical_brushes(old_brush=example, new_brush=example2))

    expect_true(iSEE:::.identical_brushes(list(xmin=1, xmax=2, ymin=10, ymax=20),
        list(xmin=1, xmax=2.0000001, ymin=10, ymax=20)))
    expect_false(iSEE:::.identical_brushes(list(xmin=1, xmax=2, ymin=10, ymax=20),
        list(xmin=1, xmax=2.0001, ymin=10, ymax=20)))
})

test_that(".transmitted_selection detects whether a brush is active", {
    all_memory <- pObjects$memory

    # No point selection
    all_memory$RedDimPlot1[[iSEE:::.brushData]] <- list()
    out <- iSEE:::.transmitted_selection("ColDataPlot1", "RedDimPlot1", all_memory,
        select_type="Active", select_saved=0L)
    expect_false(out)

    # Active point selection (non-empty brush or lasso)
    all_memory$RedDimPlot1[[iSEE:::.brushData]] <- list(a=1, b=2)
    out <- iSEE:::.transmitted_selection("ColDataPlot1", "RedDimPlot1", all_memory,
        select_type="Active", select_saved=0L)
    expect_true(out)

    # Panel linked to no transmitter (---)
    out <- iSEE:::.transmitted_selection("ColDataPlot1", "---", all_memory,
        select_type="Active", select_saved=0L)
    expect_false(out)

    # missing "select_type" argument requires to "SelectMultiSaved"
    all_memory$RedDimPlot1[[iSEE:::.multiSelectHistory]] <- list(list(a=1, b=2))
    out <- iSEE:::.transmitted_selection("ColDataPlot1", "RedDimPlot1", all_memory,
        select_type="Union", select_saved=0L)
    expect_true(out)

    # "select_type" argument "Saved"
    out <- iSEE:::.transmitted_selection("ColDataPlot1", "RedDimPlot1", all_memory,
        select_type="Saved", select_saved=0L)
    expect_false(out)

    out <- iSEE:::.transmitted_selection("ColDataPlot1", "RedDimPlot1", all_memory,
        select_type="Saved", select_saved=1L)
    expect_true(out)
})
