.global_row_node <- "iSEE_internal_global_rows"
.global_col_node <- "iSEE_internal_global_columns"

#' Spawn transmitter graphs
#'
#' Create graphs for the links between panels due to multiple or single selections.
#'
#' @param all_memory A named list of \linkS4class{Panel} objects representing the current state of the application.
#'
#' @return A \link{graph} object containing one node per panel and one directed edge from parent to child.
#' Each edge has a \code{fields} attribute specifying the slot in the child to which the transmission is directed.
#' 
#' @author Aaron Lun
#'
#' @details
#' As the name suggests, \code{.spawn_multi_selection_graph} captures the transmission of multiple selections on the rows/columns.
#' 
#' Similarly, \code{.spawn_single_selection_graph} captures the transmission of single selections.
#' The receiving slots are panel-dependent, hence the need to use \code{\link{.singleSelectionSlots}} here.
#'
#' These functions are used during app initialization as well as to reconstruct the graph after panel reorganization.
#'
#' @rdname INTERNAL_spawn_graph
#' @importFrom igraph make_graph is_dag
.spawn_multi_selection_graph <- function(all_memory) {
    graph <- make_graph(edges=character(0), 
        isolates=c(names(all_memory), .global_row_node, .global_col_node))

    params <- list(
        row=list(by=.selectRowSource, global=.selectRowDynamic, node=.global_row_node),
        column=list(by=.selectColSource, global=.selectColDynamic, node=.global_col_node)
    )

    for (x in names(all_memory)) {
        instance <- all_memory[[x]]

        for (f in params) {
            by <- instance[[f$by]]
            global <- instance[[f$global]]

            if (global) {
                graph <- .add_interpanel_link(graph, x, f$node, field=f$by)
            } else if (by %in% names(all_memory)) {
                graph <- .add_interpanel_link(graph, x, by, field=f$by)
            }
        }
    }

    if (!is_dag(graph)) {
        stop("multiple selection dependencies cannot be cyclic")
    }

    graph
}

#' @rdname INTERNAL_spawn_graph
#' @importFrom igraph make_graph
.spawn_single_selection_graph <- function(all_memory) {
    graph <- make_graph(edges=character(0), isolates=names(all_memory))

    for (x in names(all_memory)) {
        instance <- all_memory[[x]]
        fields <- .singleSelectionSlots(instance)
        for (f in fields) {
            src <- f$source
            if (instance[[src]] %in% names(all_memory)) {
                graph <- .add_interpanel_link(graph, x, instance[[src]], field=f$parameter)
            }
        }
    }

    graph
}

#' Add or remove links in the graph
#'
#' Add or remote inter-panel links that reflect single or multiple selections.
#'
#' @param graph A \link{graph} object containing one node per panel in the app.
#' @param panel_name String containing the name of the receiving panel.
#' @param parent_name String containing the name of the transmitting (parent) panel.
#' @param field String containing the name of the slot that is modified by the transmission.
#'
#' @return A \link{graph} where the requested link is added or removed.
#'
#' @details
#' Adding a link will either create an edge with the \code{fields} attribute set to \code{field}.
#' or add \code{field} to the \code{fields} attribute of an existing edge.
#'
#' Deleting a link will remove \code{field} from the \code{fields} attribute of an existing edge,
#' or delete the edge entirely if the \code{fields} attribute is subsequently empty.
#'
#' This function is a no-op if \code{parent_name="---"} or is not in \code{graph}.
#' The latter protection is necessary as this function is called during the process of updating the memory after panel deletion;
#' in that brief window, there may be references to panels that no longer exist.
#' 
#' @author Aaron Lun
#'
#' @rdname INTERNAL_interpanel_link
#' @importFrom igraph add_edges get.edge.ids E E<- V
#' @importFrom stats setNames
.add_interpanel_link <- function(graph, panel_name, parent_name, field) {
    if (parent_name %in% names(V(graph))) { # implicitly protects against noSelection *and* recently deleted panels.
        idx <- get.edge.ids(graph, c(parent_name, panel_name))
        if (idx==0L) {
            graph <- add_edges(graph, c(parent_name, panel_name), attr=list(fields=list(field)))
        } else {
            E(graph)$fields[[idx]] <- union(E(graph)$fields[[idx]], field)
        }
    }
    graph
}

#' @rdname INTERNAL_interpanel_link
#' @importFrom igraph E<- E delete_edges
.delete_interpanel_link <- function(graph, panel_name, parent_name, field) {
    if (parent_name %in% names(V(graph))) { # implicitly protects against noSelection *and* recently deleted panels.
        idx <- get.edge.ids(graph, c(parent_name, panel_name))
        if (idx!=0L) {
            fields <- E(graph)$fields[[idx]]
            remaining <- setdiff(fields, field)

            if (length(remaining)) {
                E(graph)$fields[[idx]] <- remaining
            } else {
                graph <- delete_edges(graph, idx)
            }
        }
    }
    graph
}

#' Identifies a panel's point selection receivers
#'
#' Identifies the receiving panels that need to be updated when a transmitting panel updates its multiple/single selection.
#'
#' @param graph A \link{graph} object with panel names as the vertices, see \code{\link{.spawn_multi_selection_graph}}.
#' @param panel_name A string containing the encoded name of the current transmitting panel.
#'
#' @return A character vector of names for all panels that need to be updated.
#'
#' @details
#' Upon changes to a transmitting panel \code{panel}, all other panels that receive selection information from \code{panel_name} may need to be updated.
#' This function identifies the set of children of \code{panel} that potentially require updates.
#' Whether they \emph{actually} need updates depends on the nature of the changes in \code{panel}, which will be context-specific.
#'
#' Note that this only refers to direct children.
#' If the selection effect is \code{"Restrict"} in any of the child panels, their children (i.e., the grandchildren of \code{panel}) would also need updating, and so on.
#' This is achieved via a set of observers to reactive values in \code{\link{iSEE}} to enable this recursion, see \code{\link{.create_child_propagation_observer}}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_get_direct_children
#' @importFrom igraph adjacent_vertices get.edge.ids
.get_direct_children <- function(graph, panel_name) {
    children <- names(adjacent_vertices(graph, panel_name, mode="out")[[1]])
    children <- setdiff(children, panel_name) # self-updates are handled elsewhere.

    if (!length(children)) {
        return(list())
    }

    ids <- get.edge.ids(graph, as.vector(rbind(panel_name, children)))
    output <- E(graph)$fields[ids]
    names(output) <- children
    output    
}

#' Change the selection source
#'
#' Replaces the edge in the graph if the choice of transmitting panel changes in the current panel.
#'
#' @param graph A graph object with encoded panel names as the vertices, see \code{\link{.spawn_multi_selection_graph}}.
#' @param panel_name A string containing the encoded name of the current receiving panel.
#' @param new_parent_name A string containing the encoded name of the new transmitting panel.
#' @param old_parent_name A string containing the encoded name of the old transmitting panel.
#' @param field String containing the name of the affected field, see \code{\link{.add_interpanel_link}}.
#'
#' @return A graph object with the old edge deleted (possibly) and replaced by a new edge (possibly).
#'
#' @details
#' This function will delete the edge from \code{old_parent_name} to \code{panel_name}, and add the edge from \code{new_parent_name} to \code{panel_name}.
#' This reflects a UI-mediated change in the transmitter panel from which the current panel receives its selection.
#'
#' If \code{old_parent_name="---"}, no edge will be deleted.
#' If \code{new_parent_name="---"}, no edge will be added.
#' Similarly, no deletion will occur if the edge is not present, and no addition will occur if the edge is already there.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_choose_new_selection_source
.choose_new_parent <- function(graph, panel_name, new_parent_name, old_parent_name, field) {
    graph <- .delete_interpanel_link(graph, panel_name, old_parent_name, field)
    .add_interpanel_link(graph, panel_name, new_parent_name, field)
}

#' Establish the evaluation order
#'
#' Establish the order in which panels are to be evaluated during app initialization,
#' to ensure that panels transmitting a multiple selection have valid \code{pObjects$contents} for downstream use.
#'
#' @param graph A graph object containing links between panels, produced by \code{\link{.spawn_multi_selection_graph}}.
#'
#' @details
#' This function identifies any initial connections between panels (e.g., specified in the panel arguments) for point selection.
#' It then orders the connected panels such that any transmitters are placed in front of their receivers.
#'
#' The idea is to \dQuote{evaluate} the plots at the start of the app, to obtain the coordinates for transmitting to other panels.
#' Otherwise, errors will be encountered whereby a panel tries to select from a set of coordinates that do not yet exist.
#'
#' Note that only transmitting panels are ever reported by this function.
#' It is not necessary to evaluate receiving-only panels.
#'
#' @return A character vector of names for transmitting panels in their evaluation order.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_establish_eval_order
#' @seealso
#' \code{\link{.spawn_multi_selection_graph}}
#' @importFrom igraph delete.vertices V topo_sort degree
.establish_eval_order <- function(graph) {
    iso <- V(graph)[degree(graph, mode="out") == 0]
    graph <- delete.vertices(graph, iso)
    names(topo_sort(graph, mode="out"))
}

#' Spawn the dynamic multiple selection source list
#'
#' Create a list of all panels that can respond to a multiple selection as a dynamic source.
#'
#' @param all_memory A named list of \linkS4class{Panel} objects representing the current state of the application.
#'
#' @return A named list containing two character vectors \code{"row"} or \code{"column"},
#' each specifying all panels responding to dynamic sources of row or column selections.
#' 
#' @details
#' The idea is to provide a quick reference that can be used in \code{\link{.requestActiveSelectionUpdate}} and friends,
#' to trigger resetting of the links between panels that are involved in the a dynamic source scheme.
#'
#' The panels that might serve as dynamic sources are always a subset of those that can respond to dynamic sources.
#' This is imposed by the desire to avoid circularity problems - see \code{?\link{.requestActiveSelectionUpdate}} for details.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_spawn_dynamic_multi_selection_list
.spawn_dynamic_multi_selection_list <- function(all_memory) {
    all_rows <- all_cols <- character(0)

    for (x in all_memory) {
        panel_name <- .getEncodedName(x)
        if (x[[.selectRowDynamic]]) {
            all_rows <- c(all_rows, panel_name)            
        } 
        if (x[[.selectColDynamic]]) {
            all_cols <- c(all_cols, panel_name)            
        }
    }

    list(row=all_rows, column=all_cols)
}
