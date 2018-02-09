.spawn_brush_chart <- function(memory) 
# Creates a graph containing all of the possible vertices, and 
# sets up the initial relationships between them.    
{
  node_names <- list()
  edges <- list()

  for (mode in c("redDimPlot", "colDataPlot", "featExprPlot", "rowDataPlot")) { 
    N <- nrow(memory[[mode]])
    cur_panels <- sprintf("%s%i", mode, seq_len(N))
    node_names[[mode]] <- cur_panels

    cur_edges <- vector("list",N)
    for (i in seq_len(N)) {
      cur_parent <- memory[[mode]][i, .brushByPlot]
      if (cur_parent!="") {
        cur_edges[[i]] <- c(.decoded2encoded(cur_parent), cur_panels[i])
      }
    }
    edges[[mode]] <- cur_edges
  }

  all_edges <- unlist(edges)
  node_names <- unlist(node_names)  
  g <- make_graph(as.character(all_edges), isolates=setdiff(node_names, all_edges), directed=TRUE)
  if (!is_dag(simplify(g))) {
    stop("cyclic brushing dependencies in 'initialPanels'")
  }
  return(g)
}

.choose_new_brush_source <- function(graph, panel, new_parent, old_parent) 
# Replaces the edge in the plot, if the choice of plot to receive from
# in the current panel changes.
{
  if (old_parent!="" && are_adjacent(graph, old_parent, panel)) {
    graph[from=old_parent,to=panel] <- 0
  }
  if (new_parent!="" && !are_adjacent(graph, new_parent, panel)) {
    graph <- add_edges(graph, c(new_parent, panel))
  }
  return(graph)
}

.destroy_brush_source <- function(pObjects, panel)
# Destroys the all edges to and from the current brushing source
# upon its removal from the UI. Also updates the memory to eliminate
# discarded plots as the default choice. Relies on pass-by-reference
# semantics with 'pObjects' as an environment.
{
    graph <- pObjects$brush_links

    # Resetting memory.
    all_kids <- names(adjacent_vertices(graph, panel, mode="out")[[1]])
    enc <- .split_encoded(all_kids)

    for (i in seq_along(all_kids)) {
        type <- enc$Type[i]
        ID <- enc$ID[i]
        pObjects$memory[[type]][ID, .brushByPlot] <- ""
    }

    # Destroying self memory of any transmitting brush, as there
    # is no guarantee that the transmitter will be alive when this
    # plot is added back to the UI. This ensures memory is valid,
    # in line with '.sanitize_memory()' in misc.R.
    self <- .split_encoded(panel)
    pObjects$memory[[self$Type]][self$ID, .brushByPlot] <- ""

    # Destroying the edges.
    pObjects$brush_links <- graph - incident(graph, panel, mode="all")
    return(invisible(NULL))
}

.get_brush_dependents <- function(graph, panel, memory)
# Identifies the children that need to be updated when 'panel' updates its brush.
# This includes grandchildren if restrict is TRUE in any of the children
# as the brushing will change the selected subset of points... and so on,
# throughout the graph until all leaves terminate (or hit non-restrict nodes).
{
    children <- names(adjacent_vertices(graph, panel, mode="out")[[1]])
    children <- setdiff(children, panel) # self-updates are handled elsewhere.
    
    old_children <- children
    while (length(children)) {
        enc <- .split_encoded(children)
        types <- enc$Type
        ids <- enc$ID

        new_children <- character(0)
        for (i in seq_along(children)) {
            if (memory[[types[i]]][ids[i],.brushEffect]==.brushRestrictTitle) {
                new_children <- c(new_children, names(adjacent_vertices(graph, children[i], mode="out")[[1]]))
            }
        }
     
        old_children <- c(old_children, new_children)
        children <- new_children
    }
    return(old_children)
}
