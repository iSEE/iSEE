
# Do NOT move to setup; re-defined here to keep tests self-contained.
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 2)
featAssayArgs <- featAssayPlotDefaults(sce, 3)
rowStatArgs <- rowStatTableDefaults(sce, 3)
rowDataArgs <- rowDataPlotDefaults(sce, 1)
customColArgs <- customColPlotDefaults(sce, 1)
heatMapArgs <- heatMapPlotDefaults(sce, 2)

# Creating a test graph:
# featAssayPlot1 -> redDimPlot1 -> colDataPlot2
# featAssayPlot1 -> featAssayPlot1

redDimArgs[1,iSEE:::.selectByPlot] <- "Feature assay plot 1"
featAssayArgs[1,iSEE:::.selectByPlot] <- "Feature assay plot 1"
colDataArgs[2,iSEE:::.selectByPlot] <- "Reduced dimension plot 1"

memory <- list(
    redDimPlot=redDimArgs, 
    featAssayPlot=featAssayArgs, 
    colDataPlot=colDataArgs,
    rowStatTable=rowStatArgs, 
    rowDataPlot=rowDataArgs, 
    customColPlot=customColArgs,
    heatMapPlot=heatMapArgs
)
g <- iSEE:::.spawn_selection_chart(memory)

test_that("selection link creation works correctly", {
    m <- g[]
    expect_identical(sort(rownames(m)), sort(c(sprintf("redDimPlot%i", seq_len(nrow(redDimArgs))),
                                               sprintf("featAssayPlot%i", seq_len(nrow(featAssayArgs))),
                                               sprintf("colDataPlot%i", seq_len(nrow(colDataArgs))),
                                               sprintf("rowStatTable%i", seq_len(nrow(rowStatArgs))),
                                               sprintf("rowDataPlot%i", seq_len(nrow(rowDataArgs))),
                                               sprintf("customColPlot%i", seq_len(nrow(customColArgs))),
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

    expect_error(iSEE:::.spawn_selection_chart(list(redDimPlot=redDimArgs, 
                                                    featAssayPlot=featAssayArgs, 
                                                    colDataPlot=colDataArgs, 
                                                    rowStatTable=rowStatArgs, 
                                                    rowDataPlot=rowDataArgs,
                                                    customColPlot=customColArgs,
                                                    heatMapPlot=heatMapArgs)),
                 "cyclic point selection dependencies")

    # Checking that it throws up upon being given some garbage.
    redDimArgs[1,iSEE:::.selectByPlot] <- "whee!"
    expect_error(iSEE:::.spawn_selection_chart(list(redDimPlot=redDimArgs, 
                                                    featAssayPlot=featAssayArgs, 
                                                    colDataPlot=colDataArgs,
                                                    rowStatTable=rowStatArgs, 
                                                    rowDataPlot=rowDataArgs,
                                                    customColPlot=customColArgs,
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
    expect_identical(character(0), names(igraph::adjacent_vertices(g, "colDataPlot1", mode = "in")[[1]])) # no parents.
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
    # Setting up a hierarchy.          
    redDimArgs[1,iSEE:::.selectByPlot] <- "---"
    featAssayArgs[1,iSEE:::.selectByPlot] <- "Reduced dimension plot 1"
    featAssayArgs[2,iSEE:::.selectByPlot] <- "Reduced dimension plot 1"
    colDataArgs[1,iSEE:::.selectByPlot] <- "Feature assay plot 1"
    colDataArgs[2,iSEE:::.selectByPlot] <- "Feature assay plot 2"
    featAssayArgs[3,iSEE:::.selectByPlot] <- "Column data plot 1"

    # No restriction in the children.
    memory <- list(redDimPlot=redDimArgs, 
                   featAssayPlot=featAssayArgs, 
                   colDataPlot=colDataArgs,
                   rowStatTable=rowStatArgs, 
                   rowDataPlot=rowDataArgs, 
                   customColPlot=customColArgs,
                   heatMapPlot=heatMapArgs)
    g <- iSEE:::.spawn_selection_chart(memory)
    expect_identical(iSEE:::.get_selection_dependents(g, "redDimPlot1", memory),
                     c("featAssayPlot1", "featAssayPlot2"))
    
    # Restriction in one of the children, and not the other.
    featAssayArgs[1,iSEE:::.selectEffect] <- iSEE:::.selectRestrictTitle
    memory <- list(redDimPlot=redDimArgs, 
                   featAssayPlot=featAssayArgs, 
                   colDataPlot=colDataArgs,
                   rowStatTable=rowStatArgs, 
                   rowDataPlot=rowDataArgs, 
                   customColPlot=customColArgs,
                   heatMapPlot=heatMapArgs)
    g <- iSEE:::.spawn_selection_chart(memory)
    expect_identical(iSEE:::.get_selection_dependents(g, "redDimPlot1", memory),
                     c("featAssayPlot1", "featAssayPlot2", "colDataPlot1"))

    # Restriction in the grandchildren.
    colDataArgs[1,iSEE:::.selectEffect] <- iSEE:::.selectRestrictTitle
    memory <- list(redDimPlot=redDimArgs, 
                   featAssayPlot=featAssayArgs, 
                   colDataPlot=colDataArgs,
                   rowStatTable=rowStatArgs, 
                   rowDataPlot=rowDataArgs, 
                   customColPlot=customColArgs,
                   heatMapPlot=heatMapArgs)
    g <- iSEE:::.spawn_selection_chart(memory)
    expect_identical(iSEE:::.get_selection_dependents(g, "redDimPlot1", memory),
                     c("featAssayPlot1", "featAssayPlot2", "colDataPlot1", "featAssayPlot3"))

    # Breaking the chain if we turn off restriction in the child.
    featAssayArgs[1,iSEE:::.selectEffect] <- iSEE:::.selectColorTitle
    memory <- list(redDimPlot=redDimArgs, 
                   featAssayPlot=featAssayArgs, 
                   colDataPlot=colDataArgs, 
                   rowStatTable=rowStatArgs, 
                   rowDataPlot=rowDataArgs, 
                   customColPlot=customColArgs,
                   heatMapPlot=heatMapArgs)
    g <- iSEE:::.spawn_selection_chart(memory)
    expect_identical(iSEE:::.get_selection_dependents(g, "redDimPlot1", memory),
                     c("featAssayPlot1", "featAssayPlot2"))
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

test_that("reporting order is correctly reported", {
  
  # Setting up a chain 
  redDimArgs[1,iSEE:::.selectByPlot] <- "---"
  colDataArgs[1,iSEE:::.selectByPlot] <- "Reduced dimension plot 1"
  colDataArgs[2,iSEE:::.selectByPlot] <- "Reduced dimension plot 1"
  featAssayArgs[2,iSEE:::.selectByPlot] <- "Column data plot 1"
  # fork: second from the same plot
  heatMapArgs[1,iSEE:::.selectByPlot] <- "Column data plot 1"
  # self
  featAssayArgs[1,iSEE:::.selectByPlot] <- "Feature assay plot 1" # self
  
  memory <- list(
    redDimPlot=redDimArgs, 
    featAssayPlot=featAssayArgs, 
    colDataPlot=colDataArgs,
    rowStatTable=rowStatArgs, 
    rowDataPlot=rowDataArgs, 
    customColPlot=customColArgs,
    heatMapPlot=heatMapArgs
  )
  g <- iSEE:::.spawn_selection_chart(memory)
  # plot(g)
  
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
  
  report_order <- iSEE:::.get_reporting_order(active_panels, g)
  # active_panels[report_order,]
  report_names <- iSEE:::.decode_panel_name(active_panels$Type, active_panels$ID)
  
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
  
})
