# This script tests the code related to the creation of the point transmission infrastructure.
# library(iSEE); library(testthat); source("setup_sce.R"); source("setup_other.R"); source("test_link_graph.R")

context("selection_links")

# Setting up a chain of plots.
memory <- list(
    ReducedDimensionPlot(),
    ReducedDimensionPlot(),
    ColumnDataPlot(SelectColSource="ReducedDimensionPlot1"),
    ColumnDataPlot(SelectColSource="ReducedDimensionPlot1"),
    RowAssayPlot(SelectColSource="ColumnDataPlot1"),
    RowAssayPlot(SelectColSource="RowAssayPlot1"),
    RowDataTable()
)

pObjects <- mimic_live_app(sce, memory)
g <- pObjects$selection_links

test_that("selection link creation works correctly", {
    expect_identical(
        sort(names(igraph::V(g))),
        unname(sort(vapply(pObjects$memory, .getEncodedName, "")))
    )
    
    expect_true(igraph::are_adjacent(g, "ColumnDataPlot1", "RowAssayPlot1")) # doesn't go the other way!
    expect_false(igraph::are_adjacent(g, "RowAssayPlot1", "ColumnDataPlot1"))

    expect_true(igraph::are_adjacent(g, "ReducedDimensionPlot1", "ColumnDataPlot2"))
    expect_false(igraph::are_adjacent(g, "ColumnDataPlot2", "ReducedDimensionPlot1")) # doesn't go the other way!

    # Checking that it is robust to garbage.
    all_memory  <- pObjects$memory
    all_memory$ReducedDimensionPlot1[[iSEE:::.selectColSource]] <- "whee"
    g2 <- iSEE:::.spawn_multi_selection_graph(all_memory)
    expect_identical(g[], g2[])

    # Checking that we correctly fail upon cycles.
    all_memory$ReducedDimensionPlot1[[iSEE:::.selectColSource]] <- "RowAssayPlot2"
    expect_error(iSEE:::.spawn_multi_selection_graph(all_memory), "cannot be cyclic")
})

test_that("selection link updates work correctly", {
    # Deleting edges that are there.
    expect_true(igraph::are_adjacent(g, "ReducedDimensionPlot1", "ColumnDataPlot1"))
    g2 <- iSEE:::.choose_new_parent(g, "ColumnDataPlot1", "---", "ReducedDimensionPlot1", iSEE:::.selectColSource)
    expect_false(igraph::are_adjacent(g2, "ReducedDimensionPlot1", "ColumnDataPlot1"))

    # Deleting edges that are not there makes no difference.
    expect_false(igraph::are_adjacent(g, "ColumnDataPlot1", "ReducedDimensionPlot1"))
    g2 <- iSEE:::.choose_new_parent(g, "ReducedDimensionPlot1", "---", "ColumnDataPlot1", iSEE:::.selectColSource)
    expect_equal(g[], g2[])

    # Adding edges without anything being there previously.
    expect_identical(character(0L), names(igraph::adjacent_vertices(g, "ReducedDimensionPlot1", mode="in")[[1]])) # no parents.
    g2 <- iSEE:::.choose_new_parent(g, "ReducedDimensionPlot1", "ReducedDimensionPlot2", "---", iSEE:::.selectColSource)
    expect_true(igraph::are_adjacent(g2, "ReducedDimensionPlot2", "ReducedDimensionPlot1"))

    # Adding links that are already there do nothing.
    g2 <- iSEE:::.choose_new_parent(g, "RowAssayPlot1", "ColumnDataPlot1", "---", iSEE:::.selectColSource)
    expect_equal(g[], g2[])

    # Updating edges from what previously existed.
    expect_true(igraph::are_adjacent(g, "RowAssayPlot1", "RowAssayPlot2"))
    expect_false(igraph::are_adjacent(g, "ReducedDimensionPlot1", "RowAssayPlot2"))

    g2 <- iSEE:::.choose_new_parent(g, "RowAssayPlot2", "ReducedDimensionPlot1", "RowAssayPlot1", iSEE:::.selectColSource)

    expect_false(igraph::are_adjacent(g2, "RowAssayPlot1", "RowAssayPlot2"))
    expect_true(igraph::are_adjacent(g2, "ReducedDimensionPlot1", "RowAssayPlot2"))

    # Updates to existing edges do nothing.
    g2 <- iSEE:::.choose_new_parent(g, "RowAssayPlot2", "RowAssayPlot1", "RowAssayPlot1", iSEE:::.selectColSource)
    expect_equal(g[], g2[])
})

test_that("select dependent identification works correctly", {
    expect_identical(
        names(iSEE:::.get_direct_children(g, "ReducedDimensionPlot1")),
        c("ColumnDataPlot1", "ColumnDataPlot2"))

    expect_identical(
        names(iSEE:::.get_direct_children(g, "ColumnDataPlot1")), 
        "RowAssayPlot1")

    expect_identical(
        names(iSEE:::.get_direct_children(g, "RowAssayPlot1")), 
        "RowAssayPlot2")

    expect_identical(
        iSEE:::.get_direct_children(g, "ReducedDimensionPlot2"),
        list())
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
    expect_identical(eval_order, c("ReducedDimensionPlot1", "ColumnDataPlot1", "RowAssayPlot1"))
})

test_that("graph adding and deleting responds to fields", {
    # Adding to an existing link augments the available fields.
    id <- igraph::get.edge.ids(g, c("ReducedDimensionPlot1", "ColumnDataPlot1"))
    expect_identical(igraph::E(g)$fields[[id]], iSEE:::.selectColSource)
    
    g2 <- iSEE:::.add_interpanel_link(g, "ColumnDataPlot1", "ReducedDimensionPlot1", "BLAH")
    expect_identical(igraph::E(g2)$fields[[id]], c(iSEE:::.selectColSource, "BLAH"))

    # Removing a field from a link removes the fields.
    g3 <- iSEE:::.delete_interpanel_link(g2, "ColumnDataPlot1", "ReducedDimensionPlot1", "BLAH")
    expect_identical(igraph::E(g3)$fields[[id]], iSEE:::.selectColSource)

    # Removing all fields from a link removes the link.
    g4 <- iSEE:::.delete_interpanel_link(g3, "ColumnDataPlot1", "ReducedDimensionPlot1", iSEE:::.selectColSource)
    expect_identical(igraph::get.edge.ids(g4, c("ReducedDimensionPlot1", "ColumnDataPlot1")), 0)

    # Adding a completely new link works as well.
    id <- igraph::get.edge.ids(g, c("ReducedDimensionPlot1", "ReducedDimensionPlot2"))
    expect_identical(id, 0)

    g5 <- iSEE:::.add_interpanel_link(g, "ReducedDimensionPlot2", "ReducedDimensionPlot1", "YAY")

    id <- igraph::get.edge.ids(g5, c("ReducedDimensionPlot1", "ReducedDimensionPlot2"))
    expect_identical(igraph::E(g5)$fields[[id]], "YAY")

    # Adding or removing links involving non-existent parents has no effect.
    expect_identical(iSEE:::.add_interpanel_link(g, "ColumnDataPlot1", "---", "BLAH")[], g[])
    expect_identical(iSEE:::.delete_interpanel_link(g, "ColumnDataPlot1", "---", "BLAH")[], g[])
})

test_that("aesthetics links construction works as expected", {
    memory <- list(
        ReducedDimensionPlot(ColorByRowTable="RowDataTable1"),
        RowDataTable(),
        RowAssayPlot(YAxisRowTable="RowDataPlot1"),
        RowDataPlot(),
        ColumnAssayPlot(XAxisColTable="ColumnDataTable1"),
        ColumnDataTable()
    )

    pObjects <- mimic_live_app(sce, memory)
    g <- pObjects$aesthetics_links

    # All of the correct slots corresponding to each *Table slot are registered:
    ids <- igraph::get.edge.ids(g, c("RowDataTable1", "ReducedDimensionPlot1"))
    expect_identical(igraph::E(g)$fields[[ids]], "ColorByFeatName")

    ids <- igraph::get.edge.ids(g, c("RowDataPlot1", "RowAssayPlot1"))
    expect_identical(igraph::E(g)$fields[[ids]], "YAxisFeatName")

    ids <- igraph::get.edge.ids(g, c("ColumnDataTable1", "ColumnAssayPlot1"))
    expect_identical(igraph::E(g)$fields[[ids]], "XAxisSampName")
})
