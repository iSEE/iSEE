.spawn_brush_chart <- function(memory) 
# Creates a graph containing all of the possible vertices.
# Edges are only constructed upon actual rendering of the plot. 
{
  node_names <- list()
  for (mode in c("redDim", "colData", "geneExpr")) { 
      node_names[[mode]] <- sprintf("%sPlot%i", mode, seq_len(nrow(memory[[mode]])))
  }
  node_names <- unlist(node_names)  
  make_graph(character(0), isolates=node_names, directed=TRUE)
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

.destroy_brush_source <- function(graph, panel)
# Destroys the all edges to and from the current brushing source
# upon its removal from the UI.
{
  graph - incident(graph, panel, mode="all")
}

.get_brush_dependents <- function(graph, panel, memory)
# Identifies the children that need to be updated when a panel updates.
# This includes grandchildren if restrict is TRUE in any of the children
# as the brushing will change the selected subset of points... and so on,
# throughout the graph until all leaves terminate (or hit non-restrict nodes).
{
    children <- names(adjacent_vertices(graph, panel, mode="out")[[1]])
    children <- setdiff(children, panel) # self-updates are handled elsewhere.
    
    old_children <- children
    while (length(children)) {
        types <- sub("Plot[0-9]+$", "", children)
        ids <- as.integer(sub("^[a-zA-Z]*Plot", "", children))

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
