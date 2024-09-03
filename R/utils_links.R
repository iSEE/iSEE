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
    graph <- make_graph(edges=character(0), isolates=names(all_memory))

    for (x in names(all_memory)) {
        instance <- all_memory[[x]]

        for (f in c(.selectRowSource, .selectColumnSource)) {
            parent <- slot(instance, f)
            if (parent %in% names(all_memory)) {
                graph <- .add_interpanel_link(graph, x, parent, field=f)
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
            parent <- slot(instance, f$source)
            if (parent %in% names(all_memory)) {
                graph <- .add_interpanel_link(graph, x, parent, field=f$parameter)
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
#' to ensure that panels transmitting a single/multiple selection have valid \code{pObjects$contents} for downstream use.
#'
#' @param graph A graph object containing links between panels,
#' produced by \code{\link{.spawn_multi_selection_graph}} or friends.
#'
#' @details
#' These functions identify any initial connections between panels (e.g., specified in the panel arguments) for point selection.
#' The idea is to \dQuote{evaluate} the plots at the start of the app, to obtain the coordinates for transmitting to other panels.
#' Otherwise, errors will be encountered whereby a panel tries to select from a set of coordinates that do not yet exist.
#'
#' \code{.establish_eval_order} is intended for use in the multiple selection graph;
#' the connected panels are returned in an order such that any transmitters are placed in front of their receivers.
#' This is necessary to accommodate complex DAGs where one panel transmits to another, which transmits to another, and so on.
#'
#' \code{.has_child} is intended for use in the single selection graph and will simply report all transmitting panels.
#' There is no need to order dependencies as all single selections exert their effects through a named parameter that is watched by an observer; we can thus rely on the observers to propagate all necessary effects.
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
#' @importFrom igraph delete_vertices V topo_sort degree
.establish_eval_order <- function(graph) {
    iso <- V(graph)[degree(graph, mode="out") == 0]
    graph <- delete_vertices(graph, iso)
    names(topo_sort(graph, mode="out"))
}

#' @rdname INTERNAL_establish_eval_order
#' @importFrom igraph V degree
.has_child <- function(graph) {
    names(V(graph)[degree(graph, mode="out") != 0])
}

#' Spawn the dynamic selection source lists
#'
#' Create a list of all panels that can respond to a dynamic source of multiple or single selections.
#'
#' @param all_memory A named list of \linkS4class{Panel} objects representing the current state of the application.
#'
#' @return A list of two named lists of character vectors.
#' Each internal vector corresponds to a panel and contains all fields responding to a dynamic row/column source in that panel.
#' 
#' For multiple selections, only one field will be present for each panel (i.e., the row/column selection source),
#' but for single selections, multiple fields may be present depending on the contents of \code{\link{.singleSelectionSlots}}.
#'
#' For multiple selections, the two internal lists are named \code{"row"} and \code{"column"}.
#' For single selections, they are instead named \code{"feature"} and \code{"sample"}, 
#' consistent with the nomenclature in the rest of the package.
#' 
#' @details
#' The idea is to provide a quick reference that can be used in \code{\link{.requestActiveSelectionUpdate}} and friends,
#' to trigger resetting of the links between panels that are involved in a dynamic multiple selection source scheme.
#' A similar principle applies to single selections via the observer in \code{\link{.create_dimname_propagation_observer}}.
#'
#' Note that the panels that might serve as dynamic sources for multiple selections
#' are always a subset of those that can respond to dynamic sources.
#' This is imposed by the desire to avoid circularity problems - 
#' see \code{?\link{.requestActiveSelectionUpdate}} for details.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_spawn_dynamic_selection_list
.spawn_dynamic_multi_selection_list <- function(all_memory) {
    multi_rows <- multi_cols <- list() 

    for (x in all_memory) {
        panel_name <- .getEncodedName(x)
        if (slot(x, .selectRowDynamic)) {
            multi_rows[[panel_name]] <- .selectRowSource
        } 
        if (slot(x, .selectColumnDynamic)) {
            multi_cols[[panel_name]] <- .selectColumnSource
        }
    }

    list(row=multi_rows, column=multi_cols)
}

#' @author Aaron Lun
#' @rdname INTERNAL_spawn_dynamic_selection_list
.spawn_dynamic_single_selection_list <- function(all_memory) {
    single_feat <- single_samp <- list() 

    for (x in all_memory) {
        all_singles <- .singleSelectionSlots(x)
        cur_feat <- cur_samp <- character(0)

        for (s in all_singles) {
            if (is.null(s$dimension)) {
                next # nocov
            } 
            if (is.null(s$dynamic) || !x[[s$dynamic]]) {
                next
            }

            if (s$dimension=="feature") {
                cur_feat <- c(cur_feat, s$source)
            } else {
                cur_samp <- c(cur_samp, s$source)
            }
        }

        panel_name <- .getEncodedName(x)
        if (length(cur_feat)) {
            single_feat[[panel_name]] <- cur_feat
        }
        if (length(cur_samp)) {
            single_samp[[panel_name]] <- cur_samp
        }
    }

    list(feature=single_feat, sample=single_samp)
}

#' Modify the dynamic source listing
#'
#' Add or delete entries from the dynamic source listing.
#'
#' @param listing A list of lists, typically the output of \code{\link{.spawn_dynamic_multi_selection_list}} or friends.
#' @param panel_name String containing the name of the current panel.
#' @param source_type String specifying whether we are working with \code{"row"} or \code{"column"} selections.
#' (For single-selections, this will be \code{"feature"} or \code{"sample"}.)
#' @param field String specifying the field responding to the dynamic source.
#'
#' @return 
#' For \code{.add_panel_to_dynamic_sources}, \code{listing} is returned with \code{field} added to the character vector for \code{panel_name}; if no such vector existed, a new entry named after \code{panel_name} is added.
#'
#' For \code{.delete_panel_from_dynamic_sources}, \code{listing} is returned with \code{field} removed from the character vector for \code{panel_name}; if this results in a zero-length vector, the entire entry for that panel is removed.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_add_panel_to_dynamic_sources
.add_panel_to_dynamic_sources <- function(listing, panel_name, source_type, field) {
    existing <- listing[[source_type]][[panel_name]] 
    listing[[source_type]][[panel_name]] <- union(existing, field)
    listing
}

#' @rdname INTERNAL_add_panel_to_dynamic_sources
.delete_panel_from_dynamic_sources <- function(listing, panel_name, source_type, field) {
    existing <- listing[[source_type]][[panel_name]] 
    existing <- setdiff(existing, field)
    if (!length(existing)) {
        existing <- NULL
    }
    listing[[source_type]][[panel_name]] <- existing
    listing
}
