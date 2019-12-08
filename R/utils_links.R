#' @importFrom igraph V add_vertices 
.add_panel_vertex <- function(graph, panel_name) {
    if (!panel_name %in% names(V(graph))) {
        graph <- add_vertices(graph, 1L, name=panel_name)
    } else {
        graph
    }
}

#' @importFrom igraph add_edges get.edge.ids E E<-
#' @importFrom stats setNames
.add_interpanel_link <- function(graph, panel_name, parent_name, field) {
    if (parent_name!=.noSelection) {
        idx <- get.edge.ids(graph, c(parent_name, panel_name))
        if (idx==0L) {
            graph <- add_edges(graph, c(parent_name, panel_name), attr=list(field=list(field)))
        } else {
            E(graph)$fields[[idx]] <- union(E(graph)$fields[[idx]], field)
        }
    }
    graph
}

#' @importFrom igraph E<- E delete_edges
.delete_interpanel_link <- function(graph, panel_name, parent_name, field) {
    if (parent_name!=.noSelection) {
        idx <- get.edge.ids(graph, c(parent_name, panel_name))
        if (idx!=0L) {
            fields <- E(graph)$fields[[idx]]
            remaining <- setdiff(fields, field)

            if (length(remaining)) {
                E(graph)$fields[[idx]] <- remaining
            } else {
                graph <- delete_edges(graph, c(parent_name, panel_name))
            }
        }
    }
    graph
}


#' Identifies a panel's point selection receivers
#'
#' Identifies the receiving panels that need to be updated when a transmitting panel updates its point selection.
#'
#' @param graph A graph object with encoded panel names as the vertices, see \code{\link{.spawn_selection_chart}}.
#' @param panel A string containing the encoded name of the current transmitting panel.
#'
#' @return A character vector of encoded names for all panels that need to be updated.
#'
#' @details
#' Upon changes to a transmitting panel \code{panel}, all other panels that receive the point selection information from \code{panel} may need to be updated.
#' This function identifies the set of children of \code{panel} that potentially require updates.
#' Whether they \emph{actually} need updates depends on the nature of the changes in \code{panel}, which will be context-specific.
#'
#' Note that this only refers to direct children.
#' If the selection effect is \code{"Restrict"} in any of the child panels, their children (i.e., the grandchildren of \code{panel}) would also need updating, and so on.
#' This is achieved via a set of observers to reactive values in \code{\link{iSEE}} that allow such recursion to occur naturally.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_get_direct_children
#' @seealso
#' \code{\link{.spawn_selection_chart}}
#' \code{\link{iSEE}}
#'
#' @importFrom igraph adjacent_vertices get.edge.ids
.get_direct_children <- function(graph, panel_name, names_only=TRUE) {
    children <- names(adjacent_vertices(graph, panel_name, mode="out")[[1]])
    children <- setdiff(children, panel_name) # self-updates are handled elsewhere.
    if (names_only) {
        return(children)        
    }

    if (!length(children)) {
        return(list())
    }

    ids <- get.edge.ids(graph, as.vector(rbind(panel_name, children)))
    output <- E(graph)$fields[ids]
    names(output) <- children
    output    
}

#' Destroy a selection transmitter or receiver
#'
#' Destroys all edges to and from the current panel upon its removal from the UI, to break all transfers of point selection information.
#' Also updates the memory to eliminate discarded plots as the default choice.
#'
#' @param pObjects An environment containing \code{selection_links}, a graph produced by \code{\link{.spawn_selection_chart}};
#' and \code{memory}, a list of DataFrames containing parameters for each panel of each type.
#' @param panel A string containing the encoded name of the panel to be deleted.
#'
#' @details
#' This function relies on pass-by-reference semantics with \code{pObjects} as an environment.
#' Thus, it can implicitly update \code{pObjects$selection_links} upon removal of edges to/from \code{panel}.
#'
#' The transmitting panel in \code{pObjects$memory} for the current panel is replaced with \code{"---"}, i.e., no selection.
#' This is necessary as there is no guarantee that the transmitter will be alive when this panel is added back to the UI.
#' Removal of all selection links ensures that the memory is valid, in line with the philosophy in \code{\link{.sanitize_memory}}.
#'
#' The function also replaces all references to \code{panel} in \code{pObjects$memory} with \code{"---"}.
#' Again, this is necessary as there is no guarantee that the receiving panel will be alive when this panel is added back to the UI.
#'
#' @return \code{NULL}, invisibly.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_destroy_selection_source
#' @seealso
#' \code{\link{.spawn_selection_chart}},
#' \code{\link{.sanitize_memory}}
#' \code{\link{iSEE}}
#'
#' @importFrom igraph adjacent_vertices incident
.destroy_transmitter <- function(graph, panel_name, pObjects) {
    # Resetting memory.
    all_kids <- .get_children(graph, panel_name)
    for (kid in all_kids) {
        pObjects$memory[[kid]][[.selectByPlot]] <- .noSelection
    }

    # Technically the population changes because all points are destroyed with the panel.
    .safe_reactive_bump(rObjects, paste0(panel_name, "_repopulated"))

    # Destroying the edges.
    pObjects$selection_links <- graph - incident(graph, panel, mode="all")
    invisible(NULL)
}

#' Change the selection source
#'
#' Replaces the edge in the graph if the choice of transmitting plot (i.e., to receive point selection information from) changes in the current panel.
#'
#' @param graph A graph object with encoded panel names as the vertices, see \code{\link{.spawn_selection_chart}}.
#' @param panel A string containing the encoded name of the current receiving panel.
#' @param new_parent A string containing the encoded name of the new transmitting panel.
#' @param old_parent A string containing the encoded name of the old transmitting panel.
#'
#' @return A graph object with the old edge deleted (possibly) and replaced by a new edge (possibly).
#'
#' @details
#' This function will delete the edge from \code{old_parent} to \code{panel}, and add the edge from \code{new_parent} to \code{panel}.
#' This reflects a UI-mediated change in the transmitter panel from which the current panel receives its selection.
#'
#' If \code{old_parent="---"}, no edge will be deleted.
#' If \code{new_parent="---"}, no edge will be added.
#' Similarly, no deletion will occur if the edge is not present, and no addition will occur if the edge is already there.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_choose_new_selection_source
#' @seealso \code{\link{.spawn_selection_chart}}
#'
.choose_new_parent <- function(graph, panel_name, new_parent_name, old_parent_name, field) {
    graph <- .delete_interpanel_link(graph, panel_name, old_parent_name, field)
    .add_interpanel_link(graph, panel_name, new_parent_name, field)
}

#' Checks if there is a relevant selection 
#' 
#' Checks whether there is a Shiny brush or lasso selection from a transmitter, in the active selection or in the saved selection history,
#' that is relevant to the current panel.
#'
#' @param transmitter String containing the name of the transmitting panel.
#' By default, an encoded panel name is expected.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#' @param select_type String specifying whether the current panel is receiving the \code{"Active"}, \code{"Union"} or \code{"Saved"} selections.
#' @param select_saved Integer specifying which saved selection is received by the current panel when \code{select_type="Saved"}.
#' @param mode String specifying the (encoded) panel type of the current panel. 
#' @param id Integer scalar specifying the ID of the current panel of the specified type.
#' @param encoded Logical scalar specifying whether \code{transmitter} is an encoded panel name.
#' 
#' @return A logical scalar specifying whether there is a relevant selection.
#'
#' @details 
#' This will look for saved or active selections (or both) depending on the value of \code{select_type}.
#' An active selection will not be relevant when \code{select_type="Saved"}, and vice versa.
#' 
#' \code{mode} and \code{id} will be used to retrieve the select type and saved index from \code{memory}, 
#' only if \code{select_type} and \code{select_saved} are not specified.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_transmitted_selection
#' @seealso
#' \code{\link{.any_active_selection}},
#' \code{\link{.any_saved_selection}},
#' \code{\link{iSEE}}
.transmitted_selection <- function(panel_name, parent_name, memory, select_type=NULL, select_saved=NULL) {
    if (parent_name==.noSelection) {
        return(FALSE)
    }
    panel <- memory[[panel_name]]
    transmitter <- memory[[parent_name]]

    changed <- FALSE
    if (is.null(select_type)) {
        select_type <- panel[[.selectMultiType]]
    }
    if (is.null(select_saved)) {
        select_saved <- panel[[.selectMultiSaved]]
    }

    if (select_type==.selectMultiActiveTitle || select_type==.selectMultiUnionTitle) { 
        if (.any_active_selection(transmitter)) {
            changed <- TRUE
        }

        if (select_type==.selectMultiUnionTitle) {
            if (.any_saved_selection(transmitter)) {
                changed <- TRUE
            }
        }
    } else {
        # In principle, we don't have to check the transmitter options here, because 
        # the saved index should always be valid. In practice, the saved index might
        # not be valid if this function is called after the transmitter is changed
        # but before the .selectMultiSaved selectize is updated. However, if it was
        # non-zero and invalid, then the update would cause it to be zero, which 
        # would set changed=TRUE anyway.
        if (select_saved!=0L) {
            changed <- TRUE
        }
    }

    changed
}

