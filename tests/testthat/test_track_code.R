# This script tests the code related to the creation of the point transmission infrastructure.
# library(iSEE); library(testthat); source("setup_sce.R"); source("test_track_code.R")

# Do NOT move to setup; re-defined here to keep tests self-contained.
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 2)
rowStatArgs <- rowStatTableDefaults(sce, 3)
rowDataArgs <- rowDataPlotDefaults(sce, 1)
featAssayArgs <- featAssayPlotDefaults(sce, 3)
sampAssayArgs <- sampAssayPlotDefaults(sce, 3)
colStatArgs <- colStatTableDefaults(sce, 3)
customDataArgs <- customDataPlotDefaults(sce, 1)
customStatArgs <- customStatTableDefaults(sce, 1)
heatMapArgs <- heatMapPlotDefaults(sce, 2)

# Setting up a chain of plots.
redDimArgs[1,.selectByPlot] <- "---"
colDataArgs[1,.selectByPlot] <- "Reduced dimension plot 1"
colDataArgs[2,.selectByPlot] <- "Reduced dimension plot 1"
featAssayArgs[2,.selectByPlot] <- "Column data plot 1"
heatMapArgs[1,.selectByPlot] <- "Column data plot 1"
featAssayArgs[1,.selectByPlot] <- "Feature assay plot 1"

memory <- list(
    redDimPlot=redDimArgs,
    colDataPlot=colDataArgs,
    featAssayPlot=featAssayArgs,
    rowStatTable=rowStatArgs,
    rowDataPlot=rowDataArgs,
    sampAssayPlot=sampAssayArgs,
    colStatTable=colStatArgs,
    customDataPlot=customDataArgs,
    customStatTable=customStatArgs,
    heatMapPlot=heatMapArgs
)
g <- .spawn_selection_chart(memory)

# Visible panels (in order)
initial_panels <- DataFrame(Name = c(
    "Reduced dimension plot 1",   # 1
    "Column data plot 1",         # 2
    "Column data plot 2",         # 3
    "Feature assay plot 1",  # 4
    "Feature assay plot 2",  # 5
    "Heat map 1"                  # 6
))
active_panels <- .setup_initial(initial_panels, memory)

test_that("reporting order is correctly reported", {
    # Shuffling the order to provide a more robust test of the recovery of the correct order.
    for (mode in 1:3) {
        if (mode==1) {
            cur_active <- active_panels
        } else if (mode==2) {
            cur_active <- active_panels[nrow(active_panels):1,]
        } else {
            cur_active <- active_panels[sample(nrow(active_panels)),]
        }
        report_order <- .get_reporting_order(cur_active, g)
        report_names <- .decode_panel_name(cur_active$Type, cur_active$ID)[report_order]

        # chain is:
        # redDimPlot (1) -> colDataPlot1 (2) -> featAssayPlot2 (5)
        #                                    -> heatMapPlot1  (6)
        # redDimPlot (1) -> colDataPlot2 (3)
        # featAssayPlot1 (4) -> self (4)
        expect_true(
            match("Reduced dimension plot 1", report_names) < match("Column data plot 1", report_names)
        )

        expect_true(
            match("Column data plot 1", report_names) < match("Feature assay plot 2", report_names)
        )

        expect_true(
            match("Column data plot 1", report_names) < match("Heat map 1", report_names)
        )

        expect_true(
            match("Reduced dimension plot 1", report_names) < match("Column data plot 2", report_names)
        )

        expect_identical(nrow(active_panels), length(report_order))
    }
})

test_that("code trackers run correctly", {
    # Adding a custom plot panel for coverage.
    active_panels <- rbind(active_panels, active_panels[1,])
    active_panels[1,"Type"] <- "customDataPlot"

    # Mimicking a running instance of the app.
    pObjects <- new.env()
    pObjects$memory <- memory
    pObjects$selection_links <- g
	pObjects$coordinates <- list()
	pObjects$commands <- list()

	o <- .get_reporting_order(active_panels, g)
    panelnames <- .decode_panel_name(active_panels$Type, active_panels$ID)[o]
    se <- .precompute_UI_info(sce, list(), list())

	for (panelname in panelnames) {
        enc <- .decoded2encoded(panelname)
        spenc <- .split_encoded(enc)

        if (spenc$Type!="customDataPlot") {
            FUN <- switch(spenc$Type,
                redDimPlot=.make_redDimPlot,
                featAssayPlot=.make_featAssayPlot,
                colDataPlot=.make_colDataPlot,
                rowDataPlot=.make_rowDataPlot,
                sampAssayPlot=.make_sampAssayPlot,
                heatMapPlot=.make_heatMapPlot)
            p.out <- FUN(spenc$ID, pObjects$memory, pObjects$coordinates, se, ExperimentColorMap())
        } else {
            p.out <- .make_customDataPlot(spenc$ID, pObjects$memory, pObjects$coordinates, se)
        }

        if ("xy" %in% names(p.out)) {
            pObjects$coordinates[[panelname]] <- p.out$xy[, intersect(.allCoordinatesNames, colnames(p.out$xy))]
        }
        pObjects$commands[[enc]] <- p.out$cmd_list
    }

    # Executing the code tracker, and checking that all our commands are there.
    out <- .track_it_all(active_panels, pObjects, "sce", "ecm", "list()", "list()", "")
    for (panelname in panelnames) {
        expect_true(any(grepl(panelname, out)))
    }
    expect_true(any(grepl("ggplot", out)))

    # Tracking selections only - this ignores all panels as there are no brushes defined.
    out <- .track_selections_only(active_panels, pObjects, "sce", "")
    for (panelname in panelnames) {
        expect_false(any(grepl(panelname, out)))
    }

    # Adding a brush to featAssayPlot1, such that we get it back.
    pObjects$memory$featAssayPlot[1, .brushData] <- list("this is a mock brush")
    out <- .track_selections_only(active_panels, pObjects, "sce", "")
    expect_true(any(grepl("Feature assay plot 1", out)))

    # Adding a brush to redDimPlot, which also gives us the two column data plots.
    pObjects$memory$redDimPlot[1, .brushData] <- list("this is a mock brush")
    out <- .track_selections_only(active_panels, pObjects, "sce", "")
    expect_true(any(grepl("Reduced dimension plot 1", out)))
    expect_true(any(grepl("Column data plot 1", out)))
    expect_true(any(grepl("Column data plot 2", out)))
})

