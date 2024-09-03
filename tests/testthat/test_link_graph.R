# This script tests the code related to the creation of the point transmission infrastructure.
# library(iSEE); library(testthat); source("setup_sce.R"); source("setup_mimic_live_app.R"); source("test_link_graph.R")

context("selection_links")

# Setting up a chain of plots.
memory <- list(
    ReducedDimensionPlot(),
    ReducedDimensionPlot(),
    ColumnDataPlot(ColumnSelectionSource="ReducedDimensionPlot1"),
    ColumnDataPlot(ColumnSelectionSource="ReducedDimensionPlot1"),
    FeatureAssayPlot(ColumnSelectionSource="ColumnDataPlot1"),
    FeatureAssayPlot(ColumnSelectionSource="FeatureAssayPlot1"),
    RowDataTable()
)

pObjects <- mimic_live_app(sce, memory)
g <- pObjects$selection_links

test_that("selection link creation works correctly", {
    expect_identical(
        sort(names(igraph::V(g))),
        unname(sort(vapply(pObjects$memory, .getEncodedName, "")))
    )

    expect_true(igraph::are_adjacent(g, "ColumnDataPlot1", "FeatureAssayPlot1")) # doesn't go the other way!
    expect_false(igraph::are_adjacent(g, "FeatureAssayPlot1", "ColumnDataPlot1"))

    expect_true(igraph::are_adjacent(g, "ReducedDimensionPlot1", "ColumnDataPlot2"))
    expect_false(igraph::are_adjacent(g, "ColumnDataPlot2", "ReducedDimensionPlot1")) # doesn't go the other way!

    # Checking that it is robust to garbage.
    all_memory  <- pObjects$memory
    all_memory$ReducedDimensionPlot1[[iSEE:::.selectColumnSource]] <- "whee"
    g2 <- iSEE:::.spawn_multi_selection_graph(all_memory)
    expect_identical(g[], g2[])

    # Checking that we correctly fail upon cycles.
    all_memory$ReducedDimensionPlot1[[iSEE:::.selectColumnSource]] <- "FeatureAssayPlot2"
    expect_error(iSEE:::.spawn_multi_selection_graph(all_memory), "cannot be cyclic")
})

test_that("selection link updates work correctly", {
    # Deleting edges that are there.
    expect_true(igraph::are_adjacent(g, "ReducedDimensionPlot1", "ColumnDataPlot1"))
    g2 <- iSEE:::.choose_new_parent(g, "ColumnDataPlot1", "---", "ReducedDimensionPlot1", iSEE:::.selectColumnSource)
    expect_false(igraph::are_adjacent(g2, "ReducedDimensionPlot1", "ColumnDataPlot1"))

    # Deleting edges that are not there makes no difference.
    expect_false(igraph::are_adjacent(g, "ColumnDataPlot1", "ReducedDimensionPlot1"))
    g2 <- iSEE:::.choose_new_parent(g, "ReducedDimensionPlot1", "---", "ColumnDataPlot1", iSEE:::.selectColumnSource)
    expect_equal(g[], g2[])

    # Adding edges without anything being there previously.
    expect_identical(character(0L), names(igraph::adjacent_vertices(g, "ReducedDimensionPlot1", mode="in")[[1]])) # no parents.
    g2 <- iSEE:::.choose_new_parent(g, "ReducedDimensionPlot1", "ReducedDimensionPlot2", "---", iSEE:::.selectColumnSource)
    expect_true(igraph::are_adjacent(g2, "ReducedDimensionPlot2", "ReducedDimensionPlot1"))

    # Adding links that are already there do nothing.
    g2 <- iSEE:::.choose_new_parent(g, "FeatureAssayPlot1", "ColumnDataPlot1", "---", iSEE:::.selectColumnSource)
    expect_equal(g[], g2[])

    # Updating edges from what previously existed.
    expect_true(igraph::are_adjacent(g, "FeatureAssayPlot1", "FeatureAssayPlot2"))
    expect_false(igraph::are_adjacent(g, "ReducedDimensionPlot1", "FeatureAssayPlot2"))

    g2 <- iSEE:::.choose_new_parent(g, "FeatureAssayPlot2", "ReducedDimensionPlot1", "FeatureAssayPlot1", iSEE:::.selectColumnSource)

    expect_false(igraph::are_adjacent(g2, "FeatureAssayPlot1", "FeatureAssayPlot2"))
    expect_true(igraph::are_adjacent(g2, "ReducedDimensionPlot1", "FeatureAssayPlot2"))

    # Updates to existing edges do nothing.
    g2 <- iSEE:::.choose_new_parent(g, "FeatureAssayPlot2", "FeatureAssayPlot1", "FeatureAssayPlot1", iSEE:::.selectColumnSource)
    expect_equal(g[], g2[])
})

test_that("select dependent identification works correctly", {
    expect_identical(
        names(iSEE:::.get_direct_children(g, "ReducedDimensionPlot1")),
        c("ColumnDataPlot1", "ColumnDataPlot2"))

    expect_identical(
        names(iSEE:::.get_direct_children(g, "ColumnDataPlot1")),
        "FeatureAssayPlot1")

    expect_identical(
        names(iSEE:::.get_direct_children(g, "FeatureAssayPlot1")),
        "FeatureAssayPlot2")

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
    expect_identical(eval_order, c("ReducedDimensionPlot1", "ColumnDataPlot1", "FeatureAssayPlot1"))

    # Again, only transmitting panels are ever reported by this function
    eval_order <- iSEE:::.has_child(g)
    expect_identical(sort(eval_order), sort(c("ReducedDimensionPlot1", "ColumnDataPlot1", "FeatureAssayPlot1")))
})

test_that("graph adding and deleting responds to fields", {
    # Adding to an existing link augments the available fields.
    id <- igraph::get.edge.ids(g, c("ReducedDimensionPlot1", "ColumnDataPlot1"))
    expect_identical(igraph::E(g)$fields[[id]], iSEE:::.selectColumnSource)

    g2 <- iSEE:::.add_interpanel_link(g, "ColumnDataPlot1", "ReducedDimensionPlot1", "BLAH")
    expect_identical(igraph::E(g2)$fields[[id]], c(iSEE:::.selectColumnSource, "BLAH"))

    # Removing a field from a link removes the fields.
    g3 <- iSEE:::.delete_interpanel_link(g2, "ColumnDataPlot1", "ReducedDimensionPlot1", "BLAH")
    expect_identical(igraph::E(g3)$fields[[id]], iSEE:::.selectColumnSource)

    # Removing all fields from a link removes the link.
    g4 <- iSEE:::.delete_interpanel_link(g3, "ColumnDataPlot1", "ReducedDimensionPlot1", iSEE:::.selectColumnSource)
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
        ReducedDimensionPlot(ColorByFeatureSource="RowDataTable1"),
        RowDataTable(),
        FeatureAssayPlot(YAxisFeatureSource="RowDataPlot1"),
        RowDataPlot(),
        SampleAssayPlot(XAxisSampleSource="ColumnDataTable1"),
        ColumnDataTable()
    )

    pObjects <- mimic_live_app(sce, memory)
    g <- pObjects$aesthetics_links

    # All of the correct slots corresponding to each *Table slot are registered:
    ids <- igraph::get.edge.ids(g, c("RowDataTable1", "ReducedDimensionPlot1"))
    expect_identical(igraph::E(g)$fields[[ids]], "ColorByFeatureName")

    ids <- igraph::get.edge.ids(g, c("RowDataPlot1", "FeatureAssayPlot1"))
    expect_identical(igraph::E(g)$fields[[ids]], "YAxisFeatureName")

    ids <- igraph::get.edge.ids(g, c("ColumnDataTable1", "SampleAssayPlot1"))
    expect_identical(igraph::E(g)$fields[[ids]], "XAxisSampleName")
})

test_that("dynamic source construction works as expected", {
    memory <- list(
        ReducedDimensionPlot(PanelId=1L, ColumnSelectionDynamicSource=TRUE),
        FeatureAssayPlot(PanelId=1L), # no dynamic selection, as a control.
        FeatureAssayPlot(PanelId=2L, ColumnSelectionDynamicSource=TRUE),
        SampleAssayPlot(PanelId=1L, RowSelectionDynamicSource=TRUE)
    )

    out <- iSEE:::.spawn_dynamic_multi_selection_list(memory)
    expect_identical(names(out$row), "SampleAssayPlot1")
    expect_identical(names(out$column), c("ReducedDimensionPlot1", "FeatureAssayPlot2"))

    # Now for the single selections:
    memory <- list(
        ReducedDimensionPlot(PanelId=1L, ColorByFeatureDynamicSource=TRUE),
        FeatureAssayPlot(PanelId=1L),
        FeatureAssayPlot(PanelId=2L, YAxisFeatureDynamicSource=TRUE, XAxisFeatureDynamicSource=TRUE),
        SampleAssayPlot(PanelId=1L, ColorBySampleDynamicSource=TRUE)
    )

    out <- iSEE:::.spawn_dynamic_single_selection_list(memory)
    expect_identical(names(out$feature), c("ReducedDimensionPlot1", "FeatureAssayPlot2"))
    expect_identical(out$feature$ReducedDimensionPlot1, "ColorByFeatureSource")
    expect_identical(out$feature$FeatureAssayPlot2, c("XAxisFeatureSource", "YAxisFeatureSource"))
    expect_identical(names(out$sample), "SampleAssayPlot1")
    expect_identical(out$sample$SampleAssayPlot1, "ColorBySampleSource")
})

test_that("dynamic source editing works as expected", {
    memory <- list(
        ReducedDimensionPlot(PanelId=1L, ColorByFeatureDynamicSource=TRUE),
        FeatureAssayPlot(PanelId=1L),
        FeatureAssayPlot(PanelId=2L, YAxisFeatureDynamicSource=TRUE, XAxisFeatureDynamicSource=TRUE),
        SampleAssayPlot(PanelId=1L, ColorBySampleDynamicSource=TRUE)
    )

    out <- iSEE:::.spawn_dynamic_single_selection_list(memory)

    # Works for new panels.
    out2 <- iSEE:::.add_panel_to_dynamic_sources(out, "ReducedDimensionPlot2", "feature", "ColorByFeatureSource")
    expect_identical(out2$feature$ReducedDimensionPlot2, "ColorByFeatureSource")
    out3 <- iSEE:::.delete_panel_from_dynamic_sources(out2, "ReducedDimensionPlot2", "feature", "ColorByFeatureSource")
    expect_identical(out3, out)

    # Works for existing panels with new fields.
    out2 <- iSEE:::.delete_panel_from_dynamic_sources(out2, "FeatureAssayPlot2", "feature", "XAxisFeatureSource")
    expect_false("XAxisFeatureSource" %in% out2$feature$FeatureAssayPlot2)
    out3 <- iSEE:::.add_panel_to_dynamic_sources(out, "FeatureAssayPlot2", "feature", "XAxisFeatureSource")
    expect_identical(out3, out)

    # No effect of adding.
    out2 <- iSEE:::.add_panel_to_dynamic_sources(out, "ReducedDimensionPlot1", "feature", "ColorByFeatureSource")
    expect_identical(out, out2)
})
