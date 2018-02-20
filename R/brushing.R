#' Creates the initial brush chart
#'
#' Creates a graph containing all of the possible vertices, and sets up the initial relationships between them.
#'
#' @param memory A list of DataFrames, where each DataFrame corresponds to a panel type and contains the initial settings for each individual panel of that type.
#'
#' @return A graph object from the \pkg{igraph} package,
#' where each vertex represents a panel and directed edges indicate brush transmission from one panel to another.
#'
#' @details
#' This function will construct a directed acyclic graph involving all relevant panels in \code{memory}.
#' Each vertex corresponds to one individual panel, and is present regardless of whether the panel is currently active.
#' Each edge is directed and represents the transfer of brush information from one panel (the transmitter) to another (the receiver).
#'
#' In the graph, all vertices are named using the internal encoding, i.e., \code{"redDimPlot1"} rather than the decoded \code{"Reduced dimension plot 1"}.
#' However, in the memory, all panels use the decoded names as this is what is visible on the UI.
#' Hence the need to swap between the two using \code{\link{.decoded2encoded}}.
#'
#' Note that this function will not determine if edges are valid, i.e., between two active panels.
#' This is the responsibility of other functions, namely \code{\link{.sanitize_memory}}.
#'
#' It is also worth mentioning here that the concept of a \dQuote{brush} includes selection via both the standard Shiny brush as well as our custom lasso method.
#' Unless we explicitly refer to a Shiny brush, any mentions of brushing are assumed to refer to both lasso and Shiny brushing.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_spawn_brush_chart
#' @seealso
#' \code{\link{.choose_new_brush_source}},
#' \code{\link{.destroy_brush_source}},
#' \code{\link{iSEE}}
#'
#' @importFrom igraph make_graph simplify is_dag
.spawn_brush_chart <- function(memory) {
    node_names <- list()
    edges <- list()

    for (mode in c("redDimPlot", "colDataPlot", "featExprPlot", "rowDataPlot", "rowStatTable")) {
        N <- nrow(memory[[mode]])
        cur_panels <- sprintf("%s%i", mode, seq_len(N))
        node_names[[mode]] <- cur_panels

        cur_edges <- vector("list",N)
        for (i in seq_len(N)) {
            cur_parent <- memory[[mode]][i, .brushByPlot]
            if (cur_parent!=.noSelection) {
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

#' Change the brushing source
#'
#' Replaces the edge in the graph if the choice of plot to receive the brush from changes in the current panel.
#'
#' @param graph A graph object with encoded panel names as the vertices, see \code{\link{.spawn_brush_chart}}.
#' @param panel A string containing the encoded name of the current receiving panel.
#' @param new_parent A string containing the encoded name of the new transmitting panel.
#' @param old_parent A string containing the encoded name of the old transmitting panel.
#'
#' @return A graph object with the old edge deleted (possibly) and replaced by a new edge (possibly).
#' 
#' @details
#' This function will delete the edge from \code{old_parent} to \code{panel}, and add the edge from \code{new_parent} to \code{panel}.
#' This reflects a UI-mediated change in the brushing transmitter from which a panel receives its selection.
#'
#' If \code{old_parent="---"}, no edge will be deleted.
#' If \code{new_parent="---"}, no edge will be added.
#' Similarly, no deletion will occur if the edge is not present, and no addition will occur if the edge is already there.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_choose_new_brush_source
#' @seealso \code{\link{.spawn_brush_chart}}
#'
#' @importFrom igraph are_adjacent add_edges
.choose_new_brush_source <- function(graph, panel, new_parent, old_parent)
{
  if (old_parent!=.noSelection && are_adjacent(graph, old_parent, panel)) {
    graph[from=old_parent,to=panel] <- 0
  }
  if (new_parent!=.noSelection && !are_adjacent(graph, new_parent, panel)) {
    graph <- add_edges(graph, c(new_parent, panel))
  }
  return(graph)
}

#' Destroy a brush transmitter
#'
#' Destroys all edges to and from the current brushing source upon its removal from the UI. 
#' Also updates the memory to eliminate discarded plots as the default choice. 
#' 
#' @param pObjects An environment containing \code{brush_links}, a graph produced by \code{.\link{spawn_brush_links}};
#' and \code{memory}, a list of DataFrames containing parameters for each panel of each type.
#' @param panel A string containing the encoded name of the panel to be deleted.
#'
#' @details
#' This function relies on pass-by-reference semantics with \code{pObjects} as an environment.
#' Thus, it can implicitly update \code{pObjects$brush_links} upon removal of edges to/from \code{panel}.
#' 
#' It also replaces all references to \code{panel} in \code{pObjects$memory} with \code{"---"}, i.e., no selection.
#' This is necessary as there is no guarantee that the transmitter will be alive when this plot is added back to the UI. 
#' Removal of all brush links ensures that the memory is valid, in line with \code{\link{.sanitize_memory}}.
#'
#' @return \code{NULL}, invisibly.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_destroy_brush_source
#' @seealso
#' \code{\link{.spawn_brush_chart}},
#' \code{\link{.sanitize_memory}}
#' \code{\link{iSEE}}
#'
#' @importFrom igraph adjacent_vertices incident
.destroy_brush_source <- function(pObjects, panel) {
    graph <- pObjects$brush_links

    # Resetting memory.
    all_kids <- names(adjacent_vertices(graph, panel, mode="out")[[1]])
    enc <- .split_encoded(all_kids)

    for (i in seq_along(all_kids)) {
        type <- enc$Type[i]
        ID <- enc$ID[i]
        pObjects$memory[[type]][ID, .brushByPlot] <- .noSelection
    }

    # Destroying self memory of any transmitting brush.
    self <- .split_encoded(panel)
    pObjects$memory[[self$Type]][self$ID, .brushByPlot] <- .noSelection

    # Destroying the edges.
    pObjects$brush_links <- graph - incident(graph, panel, mode="all")
    return(invisible(NULL))
}

#' Identifies a panel's brushing receivers 
#' 
#' Identifies the receiving panels that need to be updated when a transmitting panel updates its brush.
#'
#' @param graph A graph object with encoded panel names as the vertices, see \code{\link{.spawn_brush_chart}}.
#' @param panel A string containing the encoded name of the current transmitting panel.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#'
#' @return A character vector of encoded names for all panels that need to be updated.
#'
#' @details
#' Upon changes to a brush in a transmitting panel \code{panel}, all other panels that receive the brush need to be updated.
#' This function identifies the set of child panels that require updates.
#' 
#' Note that this includes indirect descendents if the brushing mode is \code{"Restrict"} in any of the child panels.
#' This is because the brushing will change the selected subset of points in the children, which will then change the grandchildren, and so on.
#' We recurse throughout the graph until all paths terminate or encounter panels in which the mode is not \code{"Restrict"}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_get_brush_dependents
#' @seealso
#' \code{\link{.spawn_brush_chart}}
#' \code{\link{iSEE}}
#' 
#' @importFrom igraph adjacent_vertices
.get_brush_dependents <- function(graph, panel, memory)
{
    children <- names(adjacent_vertices(graph, panel, mode="out")[[1]])
    children <- setdiff(children, panel) # self-updates are handled elsewhere.

    old_children <- children
    while (length(children)) {
        enc <- .split_encoded(children)
        types <- enc$Type
        ids <- enc$ID

        new_children <- character(0)
        for (i in which(types!="rowStatTable")) { # as tables don't have a brush effect, or even transmit.
            if (memory[[types[i]]][ids[i],.brushEffect]==.brushRestrictTitle) {
                new_children <- c(new_children, names(adjacent_vertices(graph, children[i], mode="out")[[1]]))
            }
        }

        old_children <- c(old_children, new_children)
        children <- new_children
    }
    return(old_children)
}

#' Test if Shiny brushes are identical
#' 
#' Check whether brush coordinates have actually changed between two Shiny brush objects.
#'
#' @param old_brush A Shiny brush object with the elements \code{xmin}, \code{xmax}, \code{ymin} and \code{ymax}.
#' @param new_brush Another  Shiny brush object for the same plot.
#'
#' @details
#' This function checks whether there are any significant differences in the rectangular regions defined by two Shiny brushes.
#' If there is no change, there is no need to waste time updating the plot.
#'
#' The tolerance is defined as one millionth of the x- and y-axis range for \code{xmin}/\code{xmax} and \code{ymin}/\code{ymax}, respectively.
#' We do not use \code{all.equal(old_brush, new_brush)}, as the plot domain can sometimes change without affecting the actual brush coordinates.
#'
#' @return A logical scalar indicating whether the brushes are identical.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_identical_brushes
#' @seealso
#' \code{\link{iSEE}}
.identical_brushes <- function(old_brush, new_brush) {
    old_null <- is.null(old_brush)
    new_null <- is.null(new_brush)
    if (old_null || new_null) {
        return(old_null==new_null)
    }

    xspan <- old_brush$xmax - old_brush$xmin
    tol <- xspan * 1e-6
    if (abs(old_brush$xmin - new_brush$xmin) > tol
        || abs(old_brush$xmax - new_brush$xmax) > tol) {
      return(FALSE)
    }

    yspan <- old_brush$ymax - old_brush$ymin
    tol <- yspan * 1e-6
    if (abs(old_brush$ymin - new_brush$ymin) > tol
        || abs(old_brush$ymax - new_brush$ymax) > tol) {
      return(FALSE)
    }

    return(TRUE)
}

#' Checks if a transmitting panel has a Shiny brush or lasso
#' 
#' A convenience function that encodes the transmitter name, and checks whether a Shiny brush or lasso currently exists in the memory of the transmitting plot.
#'
#' @param transmitter String containing the decoded name of a transmitting panel.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#' 
#' @return A list of of two elements - \code{brush}, a logical scalar indicating whether a brush/lasso exists in \code{transmitter};
#' and \code{encoded}, the encoded name of \code{transmitter}.
#'
#' @details 
#' This is largely a convenience function that avoids the need to write out all of the lines inside it.
#' Protection against non-selections for \code{transmitter} are particularly inconvenient.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_transmitted_brush
#' @seealso
#' \code{\link{.any_point_selection}},
#' \code{\link{iSEE}}
.transmitted_brush <- function(transmitter, memory)
{
    brush <- FALSE
    encoded <- .noSelection
    if (transmitter!=.noSelection) {
        enc <- .encode_panel_name(transmitter)
        encoded <- paste0(enc$Type, enc$ID)
        if (.any_point_selection(enc$Type, enc$ID, memory)) {
            brush <- TRUE
        }
    }
    return(list(brush=brush, encoded=encoded))
}

#' Checks if any points are selected
#'
#' Checks if any points are selected via a brush or closed lasso in a transmitting plot.
#'
#' @param mode String specifying the (encoded) panel type.
#' @param i Integer specifying the ID of the panel of the specified type.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#'
#' @return A logical scalar specifying whether the specified panel contains a brush or a closed lasso.
#' @author Aaron Lun
#' @rdname INTERNAL_any_point_selection
#' @seealso
#' \code{\link{.transmitted_brush}},
#' \code{\link{iSEE}}
.any_point_selection <- function(mode, i, memory) {
    if (!is.null(memory[[mode]][,.brushData][[i]])) {
        return(TRUE)       
    } 
    lasso <- memory[[mode]][,.lassoData][[i]]
    if (!is.null(lasso) && attr(lasso, "closed")) {
        return(TRUE)
    }
    return(FALSE)
}
