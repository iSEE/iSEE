# This script tests the code related to the creation of the point transmission infrastructure.
# library(iSEE); libary(testthat); source("setup_sce.R"); source("test_tracker.R")

context("codeTracker")

# Setting up a chain of plots.
memory <- list(
    RedDimPlot(ColorByRowTable="RowStatTable1", ColorBy="Feature name", PanelId=1L),
    ColDataPlot(SelectColSource="RedDimPlot1", PanelId=1L),
    ColDataPlot(SelectColSource="RedDimPlot1", PanelId=2L),
    FeatAssayPlot(SelectColSource="ColDataPlot1", PanelId=1L),
    FeatAssayPlot(SelectColSource="FeatAssayPlot1", YAxisRowTable="RowStatTable1", PanelId=2L),
    RowStatTable(PanelId=1L)
)
names(memory) <- vapply(memory, .getEncodedName, "")

# .track_it_all / .track_selection_code ----

test_that("code trackers run correctly for plots", {
    pObjects <- mimic_live_app(sce, memory)

    # Executing the code tracker, and checking that all our commands are there.
    out <- iSEE:::.track_it_all(pObjects, se_name="sce", ecm_name="ecm")
    for (panelname in vapply(memory, .getFullName, "")) {
        expect_true(any(grepl(panelname, out)))
    }
    expect_true(any(grepl("ggplot", out)))

    # Adding a brush to featAssayPlot1, such that we get it back.
    pObjects$memory$FeatAssayPlot1[[iSEE:::.brushData]] <- list("this is a mock brush")
    out <- iSEE:::.track_it_all(pObjects, se_name="sce", ecm_name="ecm")
    expect_true(any(grepl("FeatAssayPlot1.*this is a mock", out)))

    # Adding a brush to redDimPlot, which also gives us the feature assay plot above.
    pObjects$memory$RedDimPlot1[[iSEE:::.brushData]] <- list("this is a mock brush")
    out <- iSEE:::.track_it_all(pObjects, se_name="sce", ecm_name="ecm")
    expect_true(any(grepl("RedDimPlot1.*this is a mock", out)))
    expect_true(any(grepl("FeatAssayPlot1.*this is a mock", out)))
})

# .track_selection_code ----

test_that("code trackers can deparse a lasso", {

    initial_panels <- DataFrame(Name = c(
        "Reduced dimension plot 1"
    ))

    LASSO_CLOSED <- list(
        lasso=NULL,
        closed=TRUE,
        panelvar1=NULL, panelvar2=NULL,
        mapping=list(x="X", y="Y"),
        coord=matrix(c(1, 2, 2, 1, 1, 1, 1, 2, 2, 1), ncol=2))

    redDimArgs[[iSEE:::.lassoData]][[1]] <- LASSO_CLOSED

    memory <- list(
        redDimPlot=redDimArgs,
        colDataPlot=DataFrame(),
        featAssayPlot=DataFrame(),
        rowStatTable=DataFrame(),
        rowDataPlot=DataFrame(),
        sampAssayPlot=DataFrame(),
        colStatTable=DataFrame(),
        customDataPlot=DataFrame(),
        customStatTable=DataFrame(),
        heatMapPlot=DataFrame()
    )
    active_panels <- iSEE:::.setup_initial(initial_panels, memory)
    g <- iSEE:::.spawn_selection_chart(memory)

    # Mimicking a running instance of the app.
    pObjects <- new.env()
    pObjects$memory <- memory
    pObjects$selection_links <- g
    pObjects$coordinates <- list()
    pObjects$commands <- list()

    out <- .track_selection_code(active_panels, pObjects)

    # Caompare with .deparse_for_viewing directly
    out2 <- iSEE:::.deparse_for_viewing(LASSO_CLOSED)
    expect_true(any(grepl(out2, out, fixed=TRUE)))

})

test_that("code trackers can deparse a selection history", {

    initial_panels <- DataFrame(Name = c(
        "Reduced dimension plot 1"
    ))

    LASSO_CLOSED <- list(
        lasso=NULL,
        closed=TRUE,
        panelvar1=NULL, panelvar2=NULL,
        mapping=list(x="X", y="Y"),
        coord=matrix(c(1, 2, 2, 1, 1, 1, 1, 2, 2, 1), ncol=2))

    redDimArgs[[iSEE:::.multiSelectHistory]][[1]] <- list(LASSO_CLOSED)

    memory <- list(
        redDimPlot=redDimArgs,
        colDataPlot=DataFrame(),
        featAssayPlot=DataFrame(),
        rowStatTable=DataFrame(),
        rowDataPlot=DataFrame(),
        sampAssayPlot=DataFrame(),
        colStatTable=DataFrame(),
        customDataPlot=DataFrame(),
        customStatTable=DataFrame(),
        heatMapPlot=DataFrame()
    )
    active_panels <- iSEE:::.setup_initial(initial_panels, memory)
    g <- iSEE:::.spawn_selection_chart(memory)

    # Mimicking a running instance of the app.
    pObjects <- new.env()
    pObjects$memory <- memory
    pObjects$selection_links <- g
    pObjects$coordinates <- list()
    pObjects$commands <- list()

    out <- .track_selection_code(active_panels, pObjects)

    # Caompare with .deparse_for_viewing directly
    out2 <- iSEE:::.deparse_for_viewing(LASSO_CLOSED)
    expect_true(any(grepl(out2, out, fixed=TRUE)))

})

# .snapshot_graph_linkedpanels ----

test_that(".snapshot_graph_linkedpanels returns NULL after plotting", {

    # Adding a table panel for coverage of linked tables
    active_panels <- rbind(active_panels, active_panels[1,])
    active_panels[1, "Type"] <- "rowStatTable"

    # Mimicking a running instance of the app.
    pObjects <- new.env()
    pObjects$memory <- memory
    pObjects$selection_links <- g
    pObjects$coordinates <- list()
    pObjects$commands <- list()
    pObjects$table_links <- .spawn_table_links(memory)

    # Note: the following command plots as a byproduct but returns nothing
    out <- iSEE:::.snapshot_graph_linkedpanels(active_panels, pObjects)

    expect_null(out)
})

# .track_plotting_code ----

test_that(".track_plotting_code detects point selection", {

    DUMMY_COMMAND <- "cat('dummy command')"

    # Mimicking a running instance of the app.
    pObjects <- new.env()
    pObjects$memory <- memory
    pObjects$selection_links <- g
    pObjects$coordinates <- list()
    pObjects$commands <- list(
        colDataPlot1=list(select=DUMMY_COMMAND)
    )

    out <- .track_plotting_code(active_panels, pObjects, select_only = FALSE)

    expect_true(any(out == "# Receiving point selection"))
    expect_identical(sum(grepl(DUMMY_COMMAND, out, fixed=TRUE)), 1L)
})

# .track_heatmap_code ----

test_that(".track_heatmap_code detects point selection and zoom", {

    DUMMY_COMMAND <- "cat('dummy command')"

    # Mimicking a running instance of the app.
    pObjects <- new.env()
    pObjects$memory <- memory
    pObjects$selection_links <- g
    pObjects$coordinates <- list()
    pObjects$commands <- list(
        heatMapPlot1=list(
            select=DUMMY_COMMAND,
            zoom=DUMMY_COMMAND)
    )

    out <- .track_heatmap_code(active_panels, pObjects, select_only = FALSE)

    expect_true(any(out == "# Receiving selection data"))
    expect_true(any(out == "# Zooming in"))
    expect_identical(sum(grepl(DUMMY_COMMAND, out, fixed=TRUE)), 2L)
})
