context("selection_links")

# This script tests the code related to the creation of the point transmission infrastructure.
# library(iSEE); library(testthat); source("setup_sce.R"); source("test_selection_links.R")

# Do NOT move to setup; re-defined here to keep tests self-contained.
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 2)
featAssayArgs <- featAssayPlotDefaults(sce, 3)
rowStatArgs <- rowStatTableDefaults(sce, 3)
rowDataArgs <- rowDataPlotDefaults(sce, 1)
sampAssayArgs <- sampAssayPlotDefaults(sce, 3)
colStatArgs <- colStatTableDefaults(sce, 2)
customDataArgs <- customDataPlotDefaults(sce, 1)
customStatArgs <- customStatTableDefaults(sce, 1)
heatMapArgs <- heatMapPlotDefaults(sce, 2)

# Creating a test graph:
# featAssayPlot1 -> redDimPlot1 -> colDataPlot2
# featAssayPlot1 -> featAssayPlot1

redDimArgs[1,iSEE:::.selectByPlot] <- "Feature assay plot 1"
featAssayArgs[1,iSEE:::.selectByPlot] <- "Feature assay plot 1"
colDataArgs[2,iSEE:::.selectByPlot] <- "Reduced dimension plot 1"

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
g <- iSEE:::.spawn_selection_chart(memory)

test_that("selection link creation works correctly", {
    m <- g[]
    expect_identical(
        sort(rownames(m)),
        sort(c(
            sprintf("redDimPlot%i", seq_len(nrow(redDimArgs))),
            sprintf("colDataPlot%i", seq_len(nrow(colDataArgs))),
            sprintf("featAssayPlot%i", seq_len(nrow(featAssayArgs))),
            sprintf("rowStatTable%i", seq_len(nrow(rowStatArgs))),
            sprintf("rowDataPlot%i", seq_len(nrow(rowDataArgs))),
            sprintf("sampAssayPlot%i", seq_len(nrow(sampAssayArgs))),
            sprintf("colStatTable%i", seq_len(nrow(colStatArgs))),
            sprintf("customDataPlot%i", seq_len(nrow(customDataArgs))),
            sprintf("customStatTable%i", seq_len(nrow(customStatArgs))),
            sprintf("heatMapPlot%i", seq_len(nrow(heatMapArgs))))))
    expect_identical(sum(m), 3)

    expect_true(igraph::are_adjacent(g, "featAssayPlot1", "redDimPlot1"))
    expect_false(igraph::are_adjacent(g, "redDimPlot1", "featAssayPlot1")) # doesn't go the other way!

    expect_true(igraph::are_adjacent(g, "featAssayPlot1", "featAssayPlot1"))
    expect_false(igraph::are_adjacent(g, "redDimPlot1", "redDimPlot1")) # not true of self-loops in general.

    expect_true(igraph::are_adjacent(g, "redDimPlot1", "colDataPlot2"))
    expect_false(igraph::are_adjacent(g, "colDataPlot2", "redDimPlot1")) # doesn't go the other way!

    # Checking that we correctly fail upon cycles.
    redDimArgs[1,iSEE:::.selectByPlot] <- "Feature assay plot 1"
    featAssayArgs[1,iSEE:::.selectByPlot] <- "Column data plot 2"
    colDataArgs[2,iSEE:::.selectByPlot] <- "Reduced dimension plot 1"

    expect_error(
        iSEE:::.spawn_selection_chart(list(
            redDimPlot=redDimArgs,
            colDataPlot=colDataArgs,
            featAssayPlot=featAssayArgs,
            rowStatTable=rowStatArgs,
            rowDataPlot=rowDataArgs,
            sampAssayPlot=sampAssayArgs,
            colStatTable=colStatArgs,
            customDataPlot=customDataArgs,
            customStatTable=customStatArgs,
            heatMapPlot=heatMapArgs)),
        "cyclic point selection dependencies")

    # Checking that it throws up upon being given some garbage.
    redDimArgs[1, iSEE:::.selectByPlot] <- "whee!"
    expect_error(
        iSEE:::.spawn_selection_chart(list(
            redDimPlot=redDimArgs,
            colDataPlot=colDataArgs,
            featAssayPlot=featAssayArgs,
            rowStatTable=rowStatArgs,
            rowDataPlot=rowDataArgs,
            sampAssayPlot=sampAssayArgs,
            colStatTable=colStatArgs,
            customDataPlot=customDataArgs,
            customStatTable=customStatArgs,
            heatMapPlot=heatMapArgs)),
        "not a legal panel name")
})

test_that("selection link updates work correctly", {
    # Deleting edges that are there.
    expect_true(igraph::are_adjacent(g, "featAssayPlot1", "redDimPlot1"))
    expect_equal(sum(g[]), 3)
    g2 <- iSEE:::.choose_new_selection_source(g, "redDimPlot1", "---", "featAssayPlot1")
    expect_false(igraph::are_adjacent(g2, "featAssayPlot1", "redDimPlot1"))
    expect_equal(sum(g2[]), 2)

    # Deleting edges that are not there makes no difference.
    expect_false(igraph::are_adjacent(g, "colDataPlot1", "redDimPlot1"))
    g2 <- iSEE:::.choose_new_selection_source(g, "redDimPlot1", "---", "colDataPlot1")
    expect_equal(g[], g2[])

    # Adding edges without anything being there previously.
    expect_identical(character(0L), names(igraph::adjacent_vertices(g, "colDataPlot1", mode="in")[[1]])) # no parents.
    expect_equal(sum(g[]), 3)
    g2 <- iSEE:::.choose_new_selection_source(g, "colDataPlot1", "featAssayPlot3", "---")
    expect_false(igraph::are_adjacent(g2, "featAssayPlot1", "colDataPlot2"))
    expect_equal(sum(g2[]), 4)

    # Adding links that are already there do nothing.
    g2 <- iSEE:::.choose_new_selection_source(g, "redDimPlot1", "featAssayPlot1", "---")
    expect_equal(g[], g2[])

    # Updating edges from what previously existed.
    expect_true(igraph::are_adjacent(g, "redDimPlot1", "colDataPlot2"))
    expect_false(igraph::are_adjacent(g, "featAssayPlot2", "colDataPlot2"))
    expect_equal(sum(g[]), 3)
    g2 <- iSEE:::.choose_new_selection_source(g, "colDataPlot2", "featAssayPlot2", "redDimPlot1")
    expect_false(igraph::are_adjacent(g2, "redDimPlot1", "colDataPlot2"))
    expect_true(igraph::are_adjacent(g2, "featAssayPlot2", "colDataPlot2"))
    expect_equal(sum(g2[]), 3)

    # Updates to existing edges do nothing.
    g2 <- iSEE:::.choose_new_selection_source(g, "redDimPlot1", "featAssayPlot1", "featAssayPlot1")
    expect_equal(g[], g2[])
})

test_that("selection source destruction works correctly", {
    # Destroying a transmitter (to others and self)
    pObjects <- new.env()
    pObjects$selection_links <- g
    pObjects$memory <- memory

    expect_true(igraph::are_adjacent(pObjects$selection_links, "featAssayPlot1", "redDimPlot1"))
    expect_true(igraph::are_adjacent(pObjects$selection_links, "featAssayPlot1", "featAssayPlot1"))
    expect_equal(sum(pObjects$selection_links[]), 3)
    expect_identical("Feature assay plot 1", pObjects$memory$redDimPlot[1,iSEE:::.selectByPlot])
    expect_identical("Feature assay plot 1", pObjects$memory$featAssayPlot[1,iSEE:::.selectByPlot])

    iSEE:::.destroy_selection_panel(pObjects, "featAssayPlot1")

    expect_false(igraph::are_adjacent(pObjects$selection_links, "featAssayPlot1", "redDimPlot1"))
    expect_false(igraph::are_adjacent(pObjects$selection_links, "featAssayPlot1", "featAssayPlot1"))
    expect_equal(sum(pObjects$selection_links[]), 1)
    expect_identical("---", pObjects$memory$redDimPlot[1,iSEE:::.selectByPlot])
    expect_identical("---", pObjects$memory$featAssayPlot[1,iSEE:::.selectByPlot])

    # Destroying a transmitter/receiver
    pObjects <- new.env()
    pObjects$selection_links <- g
    pObjects$memory <- memory

    expect_true(igraph::are_adjacent(pObjects$selection_links, "featAssayPlot1", "redDimPlot1"))
    expect_true(igraph::are_adjacent(pObjects$selection_links, "redDimPlot1", "colDataPlot2"))
    expect_equal(sum(pObjects$selection_links[]), 3)
    expect_identical("Feature assay plot 1", pObjects$memory$redDimPlot[1,iSEE:::.selectByPlot])
    expect_identical("Reduced dimension plot 1", pObjects$memory$colDataPlot[2,iSEE:::.selectByPlot])

    iSEE:::.destroy_selection_panel(pObjects, "redDimPlot1")

    expect_false(igraph::are_adjacent(pObjects$selection_links, "featAssayPlot1", "redDimPlot1"))
    expect_false(igraph::are_adjacent(pObjects$selection_links, "redDimPlot1", "colDataPlot2"))
    expect_equal(sum(pObjects$selection_links[]), 1)
    expect_identical("---", pObjects$memory$redDimPlot[1,iSEE:::.selectByPlot])
    expect_identical("---", pObjects$memory$colDataPlot[2,iSEE:::.selectByPlot])

    # Destroying a receiver.
    pObjects <- new.env()
    pObjects$selection_links <- g
    pObjects$memory <- memory

    expect_true(igraph::are_adjacent(pObjects$selection_links, "redDimPlot1", "colDataPlot2"))

    iSEE:::.destroy_selection_panel(pObjects, "colDataPlot2")

    expect_false(igraph::are_adjacent(pObjects$selection_links, "redDimPlot1", "colDataPlot2"))
    expect_equal(sum(pObjects$selection_links[]), 2)
    expect_identical("---", pObjects$memory$colDataPlot[2,iSEE:::.selectByPlot])
})

test_that("select dependent identification works correctly", {
    redDimArgs[1,iSEE:::.selectByPlot] <- "---"
    featAssayArgs[1,iSEE:::.selectByPlot] <- "Reduced dimension plot 1"
    featAssayArgs[2,iSEE:::.selectByPlot] <- "Reduced dimension plot 1"
    colDataArgs[1,iSEE:::.selectByPlot] <- "Feature assay plot 1"
    colDataArgs[2,iSEE:::.selectByPlot] <- "Feature assay plot 2"
    featAssayArgs[3,iSEE:::.selectByPlot] <- "Column data plot 1"

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
        heatMapPlot=heatMapArgs)
    g <- iSEE:::.spawn_selection_chart(memory)

    expect_identical(iSEE:::.get_direct_children(g, "redDimPlot1"),
        c("featAssayPlot1", "featAssayPlot2"))
    expect_identical(iSEE:::.get_direct_children(g, "featAssayPlot1"), "colDataPlot1")
    expect_identical(iSEE:::.get_direct_children(g, "colDataPlot1"), "featAssayPlot3")
    expect_identical(iSEE:::.get_direct_children(g, "colDataPlot2"), character(0L))
})

test_that("brush identity function works properly", {
    expect_true(iSEE:::.identical_brushes(list(xmin=1, xmax=2, ymin=10, ymax=20),
        list(xmin=1, xmax=2, ymin=10, ymax=20)))

    # Confirming correct failure.
    expect_false(iSEE:::.identical_brushes(list(xmin=1, xmax=2, ymin=10, ymax=20),
        list(xmin=1, xmax=2, ymin=11, ymax=20)))
    expect_false(iSEE:::.identical_brushes(list(xmin=1, xmax=2, ymin=10, ymax=20),
        list(xmin=1, xmax=2, ymin=10, ymax=21)))
    expect_false(iSEE:::.identical_brushes(list(xmin=1, xmax=2, ymin=10, ymax=20),
        list(xmin=0, xmax=2, ymin=10, ymax=20)))
    expect_false(iSEE:::.identical_brushes(list(xmin=1, xmax=2, ymin=10, ymax=20),
        list(xmin=1, xmax=3, ymin=10, ymax=20)))

    # Avoid indicating that it's different when the error is very small.
    expect_true(iSEE:::.identical_brushes(list(xmin=1, xmax=2, ymin=10, ymax=20),
        list(xmin=1, xmax=2.0000001, ymin=10, ymax=20)))
    expect_false(iSEE:::.identical_brushes(list(xmin=1, xmax=2, ymin=10, ymax=20),
        list(xmin=1, xmax=2.0001, ymin=10, ymax=20)))
})

test_that("evaluation order works properly", {
    # Recall that only transmitting panels are ever reported by this function
    eval_order <- iSEE:::.establish_eval_order(g)
    expect_identical(eval_order, c("featAssayPlot1", "redDimPlot1"))

    # Testing again with added links from separate chains.
    g <- igraph::add_edges(g, c("featAssayPlot2", "featAssayPlot3"))
    g <- igraph::add_edges(g, c("featAssayPlot3", "colDataPlot1"))

    eval_order <- iSEE:::.establish_eval_order(g)
    expect_true("featAssayPlot2" %in% eval_order)
    expect_true(which(eval_order=="featAssayPlot1") < which(eval_order=="redDimPlot1"))
    expect_true(which(eval_order=="featAssayPlot2") < which(eval_order=="featAssayPlot3"))
})

test_that("selections involving custom panels work correctly", {
    # Extending the test graph:
    # featAssayPlot1 -> redDimPlot1 -> colDataPlot2
    #                               -> customDataPlot1
    # featAssayPlot1 -> featAssayPlot1
    # rowDataPlot1 -> customDataPlot1
    # redDimPlot2 -> customStatTable1
    # sampAssayPlot2 -> customStatTable1
    customDataArgs[1,iSEE:::.customColSource] <- "Reduced dimension plot 1"
    customDataArgs[1,iSEE:::.customRowSource] <- "Row data plot 1"
    customStatArgs[1,iSEE:::.customColSource] <- "Reduced dimension plot 2"
    customStatArgs[1,iSEE:::.customRowSource] <- "Sample assay plot 2"

	# Initialization works correctly.
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
	g <- iSEE:::.spawn_selection_chart(memory)

    expect_identical(sort(names(igraph::neighbors(g, "customDataPlot1", mode="in"))), c("redDimPlot1", "rowDataPlot1"))
    expect_identical(names(igraph::neighbors(g, "customDataPlot1", mode="out")), character(0L))
    expect_identical(sort(names(igraph::neighbors(g, "customStatTable1", mode="in"))), c("redDimPlot2", "sampAssayPlot2"))
    expect_identical(names(igraph::neighbors(g, "customStatTable1", mode="out")), character(0L))

    # Detection of children works properly.
    expect_identical(sort(iSEE:::.get_direct_children(g, "redDimPlot1")), c("colDataPlot2", "customDataPlot1"))
    expect_identical(iSEE:::.get_direct_children(g, "customDataPlot1"), character(0L))
    expect_identical(sort(iSEE:::.get_direct_children(g, "sampAssayPlot2")), "customStatTable1")
    expect_identical(iSEE:::.get_direct_children(g, "customStatTable1"), character(0L))

    # Destruction works correctly.
    pObjects <- new.env()
    pObjects$selection_links <- g
    pObjects$memory <- memory

    iSEE:::.destroy_selection_panel(pObjects, "redDimPlot1")
    expect_identical(sort(names(igraph::neighbors(pObjects$selection_links, "customDataPlot1", mode="in"))), "rowDataPlot1")
    expect_identical(pObjects$memory$customDataPlot[[1, iSEE:::.customColSource]], iSEE:::.noSelection)

    iSEE:::.destroy_selection_panel(pObjects, "sampAssayPlot2")
    expect_identical(sort(names(igraph::neighbors(pObjects$selection_links, "customStatTable1", mode="in"))), "redDimPlot2")
    expect_identical(pObjects$memory$customStatTable[[1, iSEE:::.customRowSource]], iSEE:::.noSelection)
})

# .transmitted_selection ----

test_that(".transmitted_selection detects whether a brush is active", {

    select_type <- "Active"

    # No point selection
    memory$redDimPlot[[iSEE:::.brushData]][1] <- list(NULL)
    out <- iSEE:::.transmitted_selection("redDimPlot1", memory, select_type, encoded=TRUE)
    expect_false(out, FALSE)

    # Active point selection (non-empty brush or lasso)
    memory$redDimPlot[[iSEE:::.brushData]][[1]] <- list(a=1, b=2)
    out <- iSEE:::.transmitted_selection("redDimPlot1", memory, select_type, encoded=TRUE)
    expect_true(out)

    # Panel linked to no transmitter (---)
    out <- .transmitted_selection("---", memory, select_type, encoded=TRUE)
    expect_false(out)

    # missing "select_type" argument requires to "SelectMultiSaved"
    memory$colDataPlot[2, "SelectMultiType"] <- iSEE:::.selectMultiUnionTitle
    # Add a saved selection
    memory$redDimPlot[[iSEE:::.multiSelectHistory]][[1]] <- list(list(a=1, b=2))
    out <- .transmitted_selection("Reduced dimension plot 1", memory, mode="colDataPlot", id=2, encoded=FALSE)
    expect_true(out)

    # "select_type" argument "Saved"
    memory$colDataPlot[2, "SelectMultiType"] <- iSEE:::.selectMultiSavedTitle
    memory$colDataPlot[2, iSEE:::.selectMultiSaved] <- 1L
    out <- .transmitted_selection("redDimPlot1", memory, mode="colDataPlot", id=2, encoded=TRUE)
    expect_true(out)

})

# .process_custom_selections ----

LASSO_CLOSED <- list(
    lasso=NULL,
    closed=TRUE,
    panelvar1=NULL, panelvar2=NULL,
    mapping=list(x="X", y="Y"),
    coord=matrix(c(1, 2, 2, 1, 1, 1, 1, 2, 2, 1), ncol=2))

BRUSH_DATA <- list(
    xmin=1, xmax=10, ymin=1, ymax=10,
    direction="xy", mapping=list(x="X", y="Y"),
    brushId="dummy_brush", outputId="dummy_plot"
)

test_that(".process_custom_selections processes incoming selection for custom panels", {

    # Link to another panel that has no selection or history
    memory$customDataPlot[1, iSEE:::.customColSource] <- "Reduced dimension plot 1"
    out <- .process_custom_selections(memory$customDataPlot[1, ], memory ,select_all = TRUE)
    expect_identical(
        out$cmds,
        c("row.names <- list(active=NULL, saved=list());", "col.names <- list(active=NULL,\n    saved=list());" )
    )

    # Create a history of selection in the transmitter panel
    # both a lasso and a brush, for code coverage
    memory$redDimPlot[[iSEE:::.multiSelectHistory]][[1]] <- list(LASSO_CLOSED, BRUSH_DATA)
    out <- .process_custom_selections(memory$customDataPlot[1, ], memory ,select_all = TRUE)
    expect_identical(
        out$cmds,
        c(
            "row.names <- list(active=NULL, saved=list());",
            "col.names <- list(active=NULL,\n    saved=list(rownames(lassoPoints(all_coordinates[['redDimPlot1']], all_select_histories[['redDimPlot1']][[1]])),\n        rownames(shiny::brushedPoints(all_coordinates[['redDimPlot1']], all_select_histories[['redDimPlot1']][[2]]))));" )
    )

})
