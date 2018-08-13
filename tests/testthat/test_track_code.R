# This script tests the code related to the creation of the point transmission infrastructure. 
# library(iSEE); library(testthat); source("setup_sce.R"); source("test_track_code.R")

# Do NOT move to setup; re-defined here to keep tests self-contained.
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 2)
featAssayArgs <- featAssayPlotDefaults(sce, 3)
rowStatArgs <- rowStatTableDefaults(sce, 3)
rowDataArgs <- rowDataPlotDefaults(sce, 1)
sampAssayArgs <- sampAssayPlotDefaults(sce, 3)
customDataArgs <- customDataPlotDefaults(sce, 1)
customStatArgs <- customStatTableDefaults(sce, 1)
heatMapArgs <- heatMapPlotDefaults(sce, 2)

# Setting up a chain of plots.
redDimArgs[1,iSEE:::.selectByPlot] <- "---"
colDataArgs[1,iSEE:::.selectByPlot] <- "Reduced dimension plot 1"
colDataArgs[2,iSEE:::.selectByPlot] <- "Reduced dimension plot 1"
featAssayArgs[2,iSEE:::.selectByPlot] <- "Column data plot 1"
heatMapArgs[1,iSEE:::.selectByPlot] <- "Column data plot 1"
featAssayArgs[1,iSEE:::.selectByPlot] <- "Feature assay plot 1" 

memory <- list(
    redDimPlot=redDimArgs,
    featAssayPlot=featAssayArgs,
    sampAssayPlot=sampAssayArgs,
    colDataPlot=colDataArgs,
    rowStatTable=rowStatArgs,
    rowDataPlot=rowDataArgs,
    customDataPlot=customDataArgs,
    customStatTable=customStatArgs,
    heatMapPlot=heatMapArgs
)
g <- iSEE:::.spawn_selection_chart(memory)

# Visible panels (in order)
initial_panels <- DataFrame(Name = c(
    "Reduced dimension plot 1",   # 1
    "Column data plot 1",         # 2
    "Column data plot 2",         # 3
    "Feature assay plot 1",  # 4
    "Feature assay plot 2",  # 5
    "Heat map 1"                  # 6
))
active_panels <- iSEE:::.setup_initial(initial_panels, memory)

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
        report_order <- iSEE:::.get_reporting_order(cur_active, g)
        report_names <- iSEE:::.decode_panel_name(cur_active$Type, cur_active$ID)[report_order]

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

	o <- iSEE:::.get_reporting_order(active_panels, g) 
    panelnames <- iSEE:::.decode_panel_name(active_panels$Type, active_panels$ID)[o]
    se <- iSEE:::.precompute_UI_info(sce, list(), list())

	for (panelname in panelnames) {
        enc <- iSEE:::.decoded2encoded(panelname)
        spenc <- iSEE:::.split_encoded(enc)

        if (spenc$Type!="customDataPlot") {
            FUN <- switch(spenc$Type,
                redDimPlot=iSEE:::.make_redDimPlot,
                featAssayPlot=iSEE:::.make_featAssayPlot,
                colDataPlot=iSEE:::.make_colDataPlot,
                rowDataPlot=iSEE:::.make_rowDataPlot,
                sampAssayPlot=iSEE:::.make_sampAssayPlot,
                heatMapPlot=iSEE:::.make_heatMapPlot)
            p.out <- FUN(spenc$ID, pObjects$memory, pObjects$coordinates, se, ExperimentColorMap())
        } else {
            p.out <- iSEE:::.make_customDataPlot(spenc$ID, pObjects$memory, pObjects$coordinates, se)
        }

        if ("xy" %in% names(p.out)) {
            pObjects$coordinates[[panelname]] <- p.out$xy[, intersect(iSEE:::.allCoordinatesNames, colnames(p.out$xy))]
        }
        pObjects$commands[[enc]] <- p.out$cmd_list	
    }

    # Executing the code tracker, and checking that all our commands are there.
    out <- iSEE:::.track_it_all(active_panels, pObjects, "sce", "ecm", "list()", "list()", "")
    for (panelname in panelnames) {
        expect_true(any(grepl(panelname, out)))
    }
    expect_true(any(grepl("ggplot", out)))

    # Tracking selections only - this ignores all panels as there are no brushes defined.
    out <- iSEE:::.track_selections_only(active_panels, pObjects, "sce", "")
    for (panelname in panelnames) {
        expect_false(any(grepl(panelname, out)))
    }

    # Adding a brush to featAssayPlot1, such that we get it back.
    pObjects$memory$featAssayPlot[1, iSEE:::.brushData] <- list("this is a mock brush")
    out <- iSEE:::.track_selections_only(active_panels, pObjects, "sce", "")
    expect_true(any(grepl("Feature assay plot 1", out)))

    # Adding a brush to redDimPlot, which also gives us the two column data plots.
    pObjects$memory$redDimPlot[1, iSEE:::.brushData] <- list("this is a mock brush")
    out <- iSEE:::.track_selections_only(active_panels, pObjects, "sce", "")
    expect_true(any(grepl("Reduced dimension plot 1", out)))
    expect_true(any(grepl("Column data plot 1", out)))
    expect_true(any(grepl("Column data plot 2", out)))
})
