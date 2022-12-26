# This script tests the code related to the creation of the point transmission infrastructure.
# library(iSEE); library(testthat); source("setup_sce.R"); source("test_code_tracker.R")

context("codeTracker")

# Setting up a chain of plots.
memory <- list(
    ReducedDimensionPlot(ColorByFeatureSource="RowDataTable1", ColorBy="Feature name"),
    ColumnDataPlot(ColumnSelectionSource="ReducedDimensionPlot1"),
    ColumnDataPlot(ColumnSelectionSource="ReducedDimensionPlot1"),
    FeatureAssayPlot(ColumnSelectionSource="ColumnDataPlot1"),
    FeatureAssayPlot(ColumnSelectionSource="FeatureAssayPlot1", YAxisFeatureSource="RowDataTable1"),
    RowDataTable()
)

# .track_it_all / .track_selection_code ----

test_that("code trackers run correctly for plots", {
    pObjects <- mimic_live_app(sce, memory)

    # Executing the code tracker, and checking that all our commands are there.
    out <- iSEE:::.track_it_all(pObjects, se_name="sce", ecm_name="ecm")
    for (panelname in vapply(pObjects$memory, .getFullName, "")) {
        expect_true(any(grepl(panelname, out)))
    }
    expect_true(any(grepl("ggplot", out)))

    # Adding a brush to featAssayPlot1, such that we get it back.
    pObjects$memory$FeatureAssayPlot1[[iSEEslots$brushData]] <- list("this is a mock brush")
    out <- iSEE:::.track_it_all(pObjects, se_name="sce", ecm_name="ecm")
    expect_true(any(grepl("FeatureAssayPlot1.*this is a mock", out)))

    # Adding a brush to redDimPlot, which also gives us the feature assay plot above.
    pObjects$memory$ReducedDimensionPlot1[[iSEEslots$brushData]] <- list("this is a mock brush")
    out <- iSEE:::.track_it_all(pObjects, se_name="sce", ecm_name="ecm")
    expect_true(any(grepl("ReducedDimensionPlot1.*this is a mock", out)))
    expect_true(any(grepl("FeatureAssayPlot1.*this is a mock", out)))
})

# .track_selection_code ----

test_that("code trackers can deparse a lasso", {
    LASSO_CLOSED <- list(
        lasso=NULL,
        closed=TRUE,
        panelvar1=NULL, panelvar2=NULL,
        mapping=list(x="X", y="Y"),
        coord=matrix(c(1, 2, 2, 1, 1, 1, 1, 2, 2, 1), ncol=2))

    memory <- list(ReducedDimensionPlot(BrushData=LASSO_CLOSED))

    # Mimicking a running instance of the app.
    pObjects <- mimic_live_app(sce, memory)

    out <- iSEE:::.track_it_all(pObjects, se_name="sce", ecm_name="ecm")
    expect_true(any(grepl("all_active[['ReducedDimensionPlot1']] <-", out, fixed=TRUE)))

    # Caompare with .deparse_for_viewing directly
    out2 <- iSEE:::.deparse_for_viewing(LASSO_CLOSED)
    expect_true(any(grepl(out2, out, fixed=TRUE)))

})

test_that("code trackers can deparse a selection history", {
    LASSO_CLOSED <- list(
        lasso=NULL,
        closed=TRUE,
        panelvar1=NULL, panelvar2=NULL,
        mapping=list(x="X", y="Y"),
        coord=matrix(c(1, 2, 2, 1, 1, 1, 1, 2, 2, 1), ncol=2))

    memory <- list(ReducedDimensionPlot(SelectionHistory=list(LASSO_CLOSED)))

    # Mimicking a running instance of the app.
    pObjects <- mimic_live_app(sce, memory)

    out <- iSEE:::.track_it_all(pObjects, se_name="sce", ecm_name="ecm")
    expect_true(any(grepl("all_saved[['ReducedDimensionPlot1']] <-", out, fixed=TRUE)))

    # Compare with .deparse_for_viewing directly
    out2 <- iSEE:::.deparse_for_viewing(LASSO_CLOSED)
    expect_true(any(grepl(out2, out, fixed=TRUE)))
})

# .snapshot_graph_linkedpanels ----

test_that(".snapshot_graph_linkedpanels returns NULL after plotting", {
    pObjects <- mimic_live_app(sce, memory)

    # Note: the following command plots as a byproduct but returns nothing
    out <- iSEE:::.snapshot_graph_linkedpanels(pObjects$selection_links,
        vapply(pObjects$memory, .panelColor, ""))

    expect_null(out)
})
