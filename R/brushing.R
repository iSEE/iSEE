#' Creates the initial point selection chart
#'
#' Creates a graph containing all of the possible vertices (i.e., panels), and sets up the initial relationships between them.
#'
#' @param memory A list of DataFrames, where each DataFrame corresponds to a panel type and contains the initial settings for each individual panel of that type.
#'
#' @return A graph object from the \pkg{igraph} package,
#' where each vertex represents a panel and directed edges indicate transmission of point selection data from one panel to another.
#'
#' @details
#' This function will construct a directed acyclic graph involving all relevant panels in \code{memory}.
#' Each vertex corresponds to one individual panel, and is present regardless of whether the panel is currently active.
#' Each edge is directed and represents the transfer of point selection information from one panel (the transmitter) to another (the receiver).
#'
#' In the graph, all vertices are named using the internal encoding, i.e., \code{"redDimPlot1"} rather than the decoded \code{"Reduced dimension plot 1"}.
#' However, in the memory, all panels use the decoded names as this is what is visible on the UI.
#' Hence the need to swap between the two using \code{\link{.decoded2encoded}}.
#'
#' Note that this function will not determine if edges are valid, i.e., between two active panels.
#' This is the responsibility of other functions, namely \code{\link{.sanitize_memory}}.
#'
#' It is also worth mentioning here that the point selection includes both the standard Shiny brush as well as our custom lasso method.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_spawn_selection_chart
#' @seealso
#' \code{\link{.choose_new_selection_source}},
#' \code{\link{.destroy_selection_panel}},
#' \code{\link{iSEE}}
#'
#' @importFrom igraph make_graph simplify is_dag
.spawn_selection_chart <- function(memory) {
    node_names <- list()
    edges <- list()

    for (mode in all_panel_types) {
        N <- nrow(memory[[mode]])
        cur_panels <- sprintf("%s%i", mode, seq_len(N))
        node_names[[mode]] <- cur_panels

        fields <- .find_selectby_field(mode)
        collected_edges <- vector("list", length(fields))
        for (i in seq_along(fields)) {
            src <- fields[i]

            cur_edges <- vector("list", N)
            for (id in seq_len(N)) {
                cur_parent <- memory[[mode]][id, src]
                if (cur_parent!=.noSelection) {
                    cur_edges[[id]] <- c(.decoded2encoded(cur_parent), cur_panels[id])
                }
            }
            collected_edges[[i]] <- cur_edges
        }

        edges[[mode]] <- unlist(collected_edges) 
    }

    # Creating a graph.
    all_edges <- unlist(edges)
    node_names <- unlist(node_names)
    g <- make_graph(as.character(all_edges), isolates=setdiff(node_names, all_edges), directed=TRUE)
    if (!is_dag(simplify(g))) {
        stop("cyclic point selection dependencies in 'initialPanels'")
    }
    return(g)
}

#' Find the transmitting field
#'
#' Finds the field in memory containing the identity of the transmitting plot for a given panel type.
#'
#' @param type String specifying the encoded panel type.
#'
#' @return A character vector containing the names of memory fields containing the identities of the transmitting plot(s).
#'
#' @details
#' This function is necessary as custom panels have different fields for their upstream transmitting plots compared to all other panels.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_find_selectby_field
.find_selectby_field <- function(type) {
    if (type %in% custom_panel_types) { 
        fields <- c(.customRowSource, .customColSource)
    } else {
        fields <- .selectByPlot
    }
    return(fields)
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
#' @importFrom igraph are_adjacent add_edges
.choose_new_selection_source <- function(graph, panel, new_parent, old_parent)
{
  if (old_parent!=.noSelection && are_adjacent(graph, old_parent, panel)) {
    graph[from=old_parent,to=panel] <- 0
  }
  if (new_parent!=.noSelection && !are_adjacent(graph, new_parent, panel)) {
    graph <- add_edges(graph, c(new_parent, panel))
  }
  return(graph)
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
.destroy_selection_panel <- function(pObjects, panel) {
    graph <- pObjects$selection_links

    # Resetting memory.
    all_kids <- names(adjacent_vertices(graph, panel, mode="out")[[1]])
    enc <- .split_encoded(all_kids)

    for (i in seq_along(all_kids)) {
        type <- enc$Type[i]
        id <- enc$ID[i]
        for (f in .find_selectby_field(type)) {
            pObjects$memory[[type]][id, f] <- .noSelection
        }
    }

    # Destroying self memory of any transmitting panel.
    self <- .split_encoded(panel)
    for (f in .find_selectby_field(self$Type)) {
        pObjects$memory[[self$Type]][self$ID, f] <- .noSelection
    }

    # Destroying the edges.
    pObjects$selection_links <- graph - incident(graph, panel, mode="all")
    return(invisible(NULL))
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
#' @importFrom igraph adjacent_vertices
.get_direct_children <- function(graph, panel)
{
    children <- names(adjacent_vertices(graph, panel, mode="out")[[1]])
    setdiff(children, panel) # self-updates are handled elsewhere.
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
.transmitted_selection <- function(transmitter, memory, select_type, select_saved, mode, id, encoded=TRUE)
{
    if (transmitter==.noSelection) {
        return(FALSE)
    }

    changed <- FALSE
    if (missing(select_type)) {
        select_type <- memory[[mode]][id, .selectMultiType]
    }
    if (missing(select_saved)) {
        select_saved <- memory[[mode]][id, .selectMultiSaved]
    }

    if (select_type==.selectMultiActiveTitle || select_type==.selectMultiUnionTitle) { 
        if (encoded) {
            enc <- .split_encoded(transmitter)
        } else {
            enc <- .encode_panel_name(transmitter)
        }

        if (.any_active_selection(enc$Type, enc$ID, memory)) {
            changed <- TRUE
        }

        if (select_type==.selectMultiUnionTitle) {
            if (.any_saved_selection(enc$Type, enc$ID, memory)) {
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

#' Checks if any points are selected
#'
#' Checks if any points are actively selected via a Shiny brush or closed lasso in a transmitting plot,
#' or if there are any saved selections in the memory of the transmitting plot.
#'
#' @param mode String specifying the (encoded) panel type for the current (transmitting) panel.
#' @param id Integer scalar specifying the ID of the current panel of the specified type.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#'
#' @return A logical scalar specifying whether the specified panel contains an active or saved Shiny brush or a closed lasso.
#' @author Aaron Lun
#' @rdname INTERNAL_any_point_selection
#' @seealso
#' \code{\link{.transmitted_selection}},
#' \code{\link{iSEE}}
.any_active_selection <- function(mode, id, memory) {
    if (!is.null(memory[[mode]][,.brushData][[id]])) {
        return(TRUE)       
    } 
    lasso <- memory[[mode]][,.lassoData][[id]]
    if (!is.null(lasso) && lasso$closed) {
        return(TRUE)
    }
    return(FALSE)
}

#' @rdname INTERNAL_any_point_selection
.any_saved_selection <- function(mode, id, memory) {
    length(memory[[mode]][,.multiSelectHistory][[id]]) > 0L
}
