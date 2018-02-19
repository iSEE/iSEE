
# Do NOT move to setup; re-defined here to keep tests self-contained.
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 2)
featExprArgs <- featExprPlotDefaults(sce, 3)
rowStatArgs <- rowStatTableDefaults(sce, 3)
rowDataArgs <- rowDataPlotDefaults(sce, 1)

# Creating a test graph:
redDimArgs[1,iSEE:::.brushByPlot] <- "Feature expression plot 1"
featExprArgs[1,iSEE:::.brushByPlot] <- "Feature expression plot 1"
colDataArgs[2,iSEE:::.brushByPlot] <- "Reduced dimension plot 1"
memory <- list(redDimPlot=redDimArgs, featExprPlot=featExprArgs, colDataPlot=colDataArgs, rowStatTable=rowStatArgs, rowDataPlot=rowDataArgs) 
g <- iSEE:::.spawn_brush_chart(memory)

test_that("brush link creation works correctly", {
    m <- g[]
    expect_identical(sort(rownames(m)), sort(c(sprintf("redDimPlot%i", seq_len(nrow(redDimArgs))),
                                               sprintf("featExprPlot%i", seq_len(nrow(featExprArgs))),
                                               sprintf("colDataPlot%i", seq_len(nrow(colDataArgs))),
                                               sprintf("rowStatTable%i", seq_len(nrow(rowStatArgs))),
                                               sprintf("rowDataPlot%i", seq_len(nrow(rowDataArgs))))))
    expect_identical(sum(m), 3)

    expect_true(igraph::are_adjacent(g, "featExprPlot1", "redDimPlot1"))
    expect_false(igraph::are_adjacent(g, "redDimPlot1", "featExprPlot1")) # doesn't go the other way!

    expect_true(igraph::are_adjacent(g, "featExprPlot1", "featExprPlot1"))
    expect_false(igraph::are_adjacent(g, "redDimPlot1", "redDimPlot1")) # not true of self-loops in general.

    expect_true(igraph::are_adjacent(g, "redDimPlot1", "colDataPlot2"))
    expect_false(igraph::are_adjacent(g, "colDataPlot2", "redDimPlot1")) # doesn't go the other way!

    # Checking that we correctly fail upon cycles.
    redDimArgs[1,iSEE:::.brushByPlot] <- "Feature expression plot 1"
    featExprArgs[1,iSEE:::.brushByPlot] <- "Column data plot 2"
    colDataArgs[2,iSEE:::.brushByPlot] <- "Reduced dimension plot 1"

    expect_error(iSEE:::.spawn_brush_chart(list(redDimPlot=redDimArgs, featExprPlot=featExprArgs, colDataPlot=colDataArgs, 
                                                rowStatTable=rowStatArgs, rowDataPlot=rowDataArgs)),
                 "cyclic brushing dependencies")

    # Checking that it throws up upon being given some garbage.
    redDimArgs[1,iSEE:::.brushByPlot] <- "whee!"
    expect_error(iSEE:::.spawn_brush_chart(list(redDimPlot=redDimArgs, featExprPlot=featExprArgs, colDataPlot=colDataArgs,
                                                rowStatTable=rowStatArgs, rowDataPlot=rowDataArgs)),
                 "not a legal panel name")
})

test_that("brush link updates work correctly", {
    # Deleting edges that are there.
    expect_true(igraph::are_adjacent(g, "featExprPlot1", "redDimPlot1"))
    expect_equal(sum(g[]), 3)
    g2 <- iSEE:::.choose_new_brush_source(g, "redDimPlot1", "---", "featExprPlot1")
    expect_false(igraph::are_adjacent(g2, "featExprPlot1", "redDimPlot1"))
    expect_equal(sum(g2[]), 2)

    # Deleting edges that are not there makes no difference.
    expect_false(igraph::are_adjacent(g, "colDataPlot1", "redDimPlot1"))
    g2 <- iSEE:::.choose_new_brush_source(g, "redDimPlot1", "---", "colDataPlot1")
    expect_equal(g[], g2[])

    # Adding edges without anything being there previously. 
    expect_identical(character(0), names(igraph::adjacent_vertices(g, "colDataPlot1", mode = "in")[[1]])) # no parents.
    expect_equal(sum(g[]), 3)
    g2 <- iSEE:::.choose_new_brush_source(g, "colDataPlot1", "featExprPlot3", "---")
    expect_false(igraph::are_adjacent(g2, "featExprPlot1", "colDataPlot2"))
    expect_equal(sum(g2[]), 4)

    # Adding links that are already there do nothing.    
    g2 <- iSEE:::.choose_new_brush_source(g, "redDimPlot1", "featExprPlot1", "---")
    expect_equal(g[], g2[])

    # Updating edges from what previously existed.
    expect_true(igraph::are_adjacent(g, "redDimPlot1", "colDataPlot2"))
    expect_false(igraph::are_adjacent(g, "featExprPlot2", "colDataPlot2"))
    expect_equal(sum(g[]), 3)
    g2 <- iSEE:::.choose_new_brush_source(g, "colDataPlot2", "featExprPlot2", "redDimPlot1")
    expect_false(igraph::are_adjacent(g2, "redDimPlot1", "colDataPlot2"))
    expect_true(igraph::are_adjacent(g2, "featExprPlot2", "colDataPlot2"))
    expect_equal(sum(g2[]), 3)

    # Updates to existing edges do nothing.
    g2 <- iSEE:::.choose_new_brush_source(g, "redDimPlot1", "featExprPlot1", "featExprPlot1")
    expect_equal(g[], g2[])
})

test_that("brush source destruction works correctly", {
    # Destroying a transmitter (to others and self)
    pObjects <- new.env()
    pObjects$brush_links <- g
    pObjects$memory <- memory

    expect_true(igraph::are_adjacent(pObjects$brush_links, "featExprPlot1", "redDimPlot1"))
    expect_true(igraph::are_adjacent(pObjects$brush_links, "featExprPlot1", "featExprPlot1"))
    expect_equal(sum(pObjects$brush_links[]), 3)
    expect_identical("Feature expression plot 1", pObjects$memory$redDimPlot[1,iSEE:::.brushByPlot])
    expect_identical("Feature expression plot 1", pObjects$memory$featExprPlot[1,iSEE:::.brushByPlot])

    iSEE:::.destroy_brush_source(pObjects, "featExprPlot1")

    expect_false(igraph::are_adjacent(pObjects$brush_links, "featExprPlot1", "redDimPlot1"))
    expect_false(igraph::are_adjacent(pObjects$brush_links, "featExprPlot1", "featExprPlot1"))
    expect_equal(sum(pObjects$brush_links[]), 1)
    expect_identical("---", pObjects$memory$redDimPlot[1,iSEE:::.brushByPlot])
    expect_identical("---", pObjects$memory$featExprPlot[1,iSEE:::.brushByPlot])

    # Destroying a transmitter/receiver
    pObjects <- new.env()
    pObjects$brush_links <- g
    pObjects$memory <- memory

    expect_true(igraph::are_adjacent(pObjects$brush_links, "featExprPlot1", "redDimPlot1"))
    expect_true(igraph::are_adjacent(pObjects$brush_links, "redDimPlot1", "colDataPlot2"))
    expect_equal(sum(pObjects$brush_links[]), 3)
    expect_identical("Feature expression plot 1", pObjects$memory$redDimPlot[1,iSEE:::.brushByPlot])
    expect_identical("Reduced dimension plot 1", pObjects$memory$colDataPlot[2,iSEE:::.brushByPlot])

    iSEE:::.destroy_brush_source(pObjects, "redDimPlot1")

    expect_false(igraph::are_adjacent(pObjects$brush_links, "featExprPlot1", "redDimPlot1"))
    expect_false(igraph::are_adjacent(pObjects$brush_links, "redDimPlot1", "colDataPlot2"))
    expect_equal(sum(pObjects$brush_links[]), 1)
    expect_identical("---", pObjects$memory$redDimPlot[1,iSEE:::.brushByPlot]) 
    expect_identical("---", pObjects$memory$colDataPlot[2,iSEE:::.brushByPlot])

    # Destroying a receiver.
    pObjects <- new.env()
    pObjects$brush_links <- g
    pObjects$memory <- memory

    expect_true(igraph::are_adjacent(pObjects$brush_links, "redDimPlot1", "colDataPlot2"))

    iSEE:::.destroy_brush_source(pObjects, "colDataPlot2")

    expect_false(igraph::are_adjacent(pObjects$brush_links, "redDimPlot1", "colDataPlot2"))
    expect_equal(sum(pObjects$brush_links[]), 2)
    expect_identical("---", pObjects$memory$colDataPlot[2,iSEE:::.brushByPlot])
})

test_that("brush dependent identification works correctly", {
    # Setting up a hierarchy.          
    redDimArgs[1,iSEE:::.brushByPlot] <- "---"
    featExprArgs[1,iSEE:::.brushByPlot] <- "Reduced dimension plot 1"
    featExprArgs[2,iSEE:::.brushByPlot] <- "Reduced dimension plot 1"
    colDataArgs[1,iSEE:::.brushByPlot] <- "Feature expression plot 1"
    colDataArgs[2,iSEE:::.brushByPlot] <- "Feature expression plot 2"
    featExprArgs[3,iSEE:::.brushByPlot] <- "Column data plot 1"

    # No restriction in the children.
    memory <- list(redDimPlot=redDimArgs, featExprPlot=featExprArgs, colDataPlot=colDataArgs,
                   rowStatTable=rowStatArgs, rowDataPlot=rowDataArgs)
    g <- iSEE:::.spawn_brush_chart(memory)
    expect_identical(iSEE:::.get_brush_dependents(g, "redDimPlot1", memory),
                     c("featExprPlot1", "featExprPlot2"))
    
    # Restriction in one of the children, and not the other.
    featExprArgs[1,iSEE:::.brushEffect] <- iSEE:::.brushRestrictTitle
    memory <- list(redDimPlot=redDimArgs, featExprPlot=featExprArgs, colDataPlot=colDataArgs,
                   rowStatTable=rowStatArgs, rowDataPlot=rowDataArgs)
    g <- iSEE:::.spawn_brush_chart(memory)
    expect_identical(iSEE:::.get_brush_dependents(g, "redDimPlot1", memory),
                     c("featExprPlot1", "featExprPlot2", "colDataPlot1"))

    # Restriction in the grandchildren.
    colDataArgs[1,iSEE:::.brushEffect] <- iSEE:::.brushRestrictTitle
    memory <- list(redDimPlot=redDimArgs, featExprPlot=featExprArgs, colDataPlot=colDataArgs,
                   rowStatTable=rowStatArgs, rowDataPlot=rowDataArgs)
    g <- iSEE:::.spawn_brush_chart(memory)
    expect_identical(iSEE:::.get_brush_dependents(g, "redDimPlot1", memory),
                     c("featExprPlot1", "featExprPlot2", "colDataPlot1", "featExprPlot3"))

    # Breaking the chain if we turn off restriction in the child.
    featExprArgs[1,iSEE:::.brushEffect] <- iSEE:::.brushColorTitle
    memory <- list(redDimPlot=redDimArgs, featExprPlot=featExprArgs, colDataPlot=colDataArgs, 
                   rowStatTable=rowStatArgs, rowDataPlot=rowDataArgs)
    g <- iSEE:::.spawn_brush_chart(memory)
    expect_identical(iSEE:::.get_brush_dependents(g, "redDimPlot1", memory),
                     c("featExprPlot1", "featExprPlot2"))
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
