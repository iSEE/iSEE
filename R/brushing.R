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
      graph[from=old_parent,to=panel]<-0
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
