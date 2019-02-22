#' Create table links
#'
#' Creates a record of the links between row statistics tables and the plots that receive input from them.
#'
#' @param memory A list of DataFrames, where each DataFrame corresponds to a panel type and contains the initial settings for each individual panel of that type.
#'
#' @return A list where each entry is itself a list, named with the encoded name of a row statistics table.
#' Each table-specific list contains:
#' \itemize{
#' \item \code{"xaxis"}, a character vector containing encoded names of (feature/sample assay) plots that receive input from this table for x-axis specification.
#' \item \code{"yaxis"}, a character vector containing encoded names of (feature/sample assay) plots that receive input from this table for y-axis specification.
#' \item \code{"color"}, a character vector containing encoded names of various plots that receive input from this table for colour specification.
#' }
#'
#' @details
#' Note that this function will not determine if the links are valid, i.e., between an active table and an active plot.
#' This is the responsibility of other functions, namely \code{\link{.sanitize_memory}}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_spawn_table_links
#' @seealso
#' \code{\link{.modify_table_links}},
#' \code{\link{.destroy_table}}
.spawn_table_links <- function(memory) {
    all_linked_tabs <- c(sprintf("rowStatTable%i", seq_len(nrow(memory$rowStatTable))), 
        sprintf("colStatTable%i", seq_len(nrow(memory$colStatTable))))
    table_links <- rep(list(list(color=character(0), xaxis=character(0), yaxis=character(0))), length(all_linked_tabs))
    names(table_links) <- all_linked_tabs

    # Adding the links for the colors.
    for (mode in point_plot_types) {
        N <- nrow(memory[[mode]])
        cur_panels <- sprintf("%s%i", mode, seq_len(N))

        for (id in seq_len(N)) {
            for (tab_type in c(.colorByRowTable, .colorByColTable)) { 
                cur_tab <- memory[[mode]][id, tab_type]
                if (cur_tab!=.noSelection) {
                    tab_name <- .decoded2encoded(cur_tab)
                    table_links[[tab_name]]$color <- c(table_links[[tab_name]]$color, cur_panels[id])
                }
            }
        }
    }

    # Adding links for x- and y-axes.
    for (mode in c("featAssayPlot", "sampAssayPlot")) {
        N <- nrow(memory[[mode]])
        cur_panels <- sprintf("%s%i", mode, seq_len(N))
        if (mode=="featAssayPlot") {
            xtabfield <- .featAssayXAxisRowTable
            ytabfield <- .featAssayYAxisRowTable
        } else {
            xtabfield <- .sampAssayXAxisColTable
            ytabfield <- .sampAssayYAxisColTable
        }
        
        for (id in seq_len(N)) {
            param_choices <- memory[[mode]][id,]

            X_tab <- param_choices[[xtabfield]]
            if (X_tab!=.noSelection) {
                tab_name <- .decoded2encoded(X_tab)
                table_links[[tab_name]]$xaxis <- c(table_links[[tab_name]]$xaxis, cur_panels[id])
            }
    
            Y_tab <- param_choices[[ytabfield]]
            if (Y_tab!=.noSelection) {
                tab_name <- .decoded2encoded(Y_tab)
                table_links[[tab_name]]$yaxis <- c(table_links[[tab_name]]$yaxis, cur_panels[id])
            }
        }
    }

    return(table_links)
}

#' Clear links for a destroyed table
#'
#' Clear all links to a row statistics table that has been destroyed.
#'
#' @param pObjects An environment containing \code{table_links}, a graph produced by \code{\link{.spawn_table_links}};
#' and \code{memory}, a list of DataFrames containing parameters for each panel of each type.
#' @param tab String containing the encoded name of the row statistics table to be destroyed.
#'
#' @return \code{NULL}, invisibly.
#'
#' @details
#' This function modifies \code{pObjects$table_links} to empty the character vectors containing the encoded names of the receiving plots.
#' It relies on pass-by-reference behaviour of \code{pObjects} to work properly.
#'
#' The function also modifies the \code{pObjects$memory} to remove all references to \code{tab} in the relevant fields of the receiving plots,
#' replacing it with an empty string (i.e., no selection).
#' This ensures that the memory is always valid, in line with \code{\link{.sanitize_memory}}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_destroy_table
#' @seealso
#' \code{\link{.spawn_table_links}}
.destroy_table <- function(pObjects, tab) {
    links <- pObjects$table_links
    all_kids <- links[[tab]]

    # Deciding which fields to cancel.
    if (.split_encoded(tab)$Type=="rowStatTable") {
        col_field <- .colorByRowTable
        assay_panel_type <- "featAssayPlot"
        x_field <- .featAssayXAxisRowTable
        y_field <- .featAssayYAxisRowTable
    } else {
        col_field <- .colorByColTable
        assay_panel_type <- "sampAssayPlot"
        x_field <- .sampAssayXAxisColTable
        y_field <- .sampAssayYAxisColTable
    }

    # Updating the memory of all linked plots.
    col_kids <- all_kids$color
    enc <- .split_encoded(col_kids)
    for (i in seq_along(col_kids)) {
        kid <- col_kids[i]
        type <- enc$Type[i]
        pObjects$memory[[type]][kid, col_field] <- .noSelection
    }

    for (panel in all_kids$yaxis) {
        pObjects$memory[[assay_panel_type]][panel, y_field] <- .noSelection
    }
    for (panel in all_kids$xaxis) {
        pObjects$memory[[assay_panel_type]][panel, x_field] <- .noSelection
    }

    # Erasing the links.
    links[[tab]] <- list(color=character(0), xaxis=character(0), yaxis=character(0))
    pObjects$table_links <- links
    return(invisible(NULL))
}

#' Change the table source
#'
#' Switch to a different row statistics table for a panel that receives input from a linked table.
#'
#' @param links A list of lists specifying the dependent plots for each table, see \code{\link{.spawn_table_links}}.
#' @param dest A string containing the encoded name of the current receiving panel.
#' @param newtab A string containing the \emph{decoded} name of the new linked table.
#' @param oldtab A string containing the \emph{decoded} name of the old linked panel.
#' @param mode A string specifying the mode in which a panel is receiving input, i.e., for color or x/y-axis.
#'
#' @return A modified \code{links} where \code{dest} is moved from the dependents of \code{oldtab} to those of \code{newtab}.
#'
#' @details
#' This function will remove \code{dest} from the vector for \code{mode} in \code{oldtab}, provided that \code{oldtab} is not an empty string.
#' It will then add \code{dest} to the vector for \code{mode} in \code{newtab}, provided that \code{newtab} is not an empty string.
#' This reflects a UI-mediated change in the choice of row statistics table from which \code{dest} receives input.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_modify_table_links
#' @seealso
#' \code{\link{.spawn_table_links}},
#' \code{\link{.setup_table_observer}}
.modify_table_links <- function(links, dest, newtab, oldtab, mode='color') {
    if (oldtab!=.noSelection) {
        oldtab <- .decoded2encoded(oldtab)
        links[[oldtab]][[mode]] <- setdiff(links[[oldtab]][[mode]], dest)
    }
    if (newtab!=.noSelection) {
        newtab <- .decoded2encoded(newtab)
        links[[newtab]][[mode]] <- union(links[[newtab]][[mode]], dest)
    }
    return(links)
}

#' Set up a table observer
#'
#' Set up the actions for an observer for a parameter choice in a plot panel that may involve a linked table.
#'
#' @param mode String specifying the encoded panel type of the current (receiving) plot.
#' @param id Integer scalar specifying the index of the current panel of the specified type.
#' @param pObjects An environment containing \code{table_links}, a graph produced by \code{\link{.spawn_table_links}};
#' and \code{memory}, a list of DataFrames containing parameters for each panel of each type.
#' @param rObjects A reactive list containing incrementable counters for all panels,
#' @param input A Shiny list of inputs, generated by the server.
#' @param session A \code{session} object from a Shiny server.
#' @param by_field String specifying the name of the field containing the data source for the current panel.
#' @param title String specifying the title used for table-based data sources, to match to the value of \code{by_field} in \code{memory} for this plot.
#' @param select_field String specifying the name of the field containing the selected row for this panel.
#' @param tab_field String specifying the name of the field containing the decoded name of the table input for this panel.
#' @param select_choices Vector of consecutive integers indexing all rows, named with the row names.
#' @param param String specifying the type of table link to the current plot, i.e., color or x/y-axis.
#'
#' @return A logical scalar indicating whether the current (receiving) plot needs to be regenerated.
#'
#' @details
#' This function has a number of side-effects, relying on the pass-by-reference behaviour of \code{pObjects}, \code{rObjects} and \code{session} to perform its role.
#' \itemize{
#' \item New values of the fields \code{by_field} and \code{tab_field} in \code{input} overwrite values in \code{pObjects$memory} for the current panel.
#' \item \code{pObjects$table_links} is modified for the new linked table in \code{input[[tab_field]]}.
#' Note that table links are \emph{not} destroyed if \code{input[[by_field]]} is different to \code{title},
#' as this could result in failure to clear the memory of the current panel in \code{\link{.delete_table_links}}.
#' \item Link panel counters in \code{rObjects} are incremented if the new linked table differs from the old link table and \code{input[[by_field]]==title};
#' or if \code{input[[by_field]]} differs from that in memory and either of them is equal to \code{title}.
#' Counters are only updated for the current panel as well as the old/new tables, and only when
#' \item The selectize UI element corresponding to \code{select_field} is updated with the current selection in the linked table, if a new linked table was chosen.
#' Note that this will trigger another call to the observer that contains this function.
#' }
#'
#' The flag to regenerate the current plot is set if the \code{by_field} and \code{tab_field} fields in \code{input} are non-\code{NULL};
#' and the \code{by_field} entry differs from the parameter currently stored in memory.
#'
#' Note that \code{by_field} and \code{title} are ignored if \code{param="yaxis"}, as the y-axis of feature/sample assay plots have no other choice of variable.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_setup_table_observer
#' @seealso
#' \code{\link{.modify_table_links}},
#' \code{\link{iSEE}}
#'
#' @importFrom shiny updateSelectizeInput
#' @importFrom methods as
.setup_table_observer <- function(mode, id, pObjects, rObjects, input, session,
                                  by_field, title, select_field, tab_field,
                                  select_choices, param='color')
{
    plot_name <- paste0(mode, id)
    prefix <- paste0(plot_name, "_")

    # Checking that all the incoming arguments are non-NULL.
    if (param!='yaxis') {
        choice <- input[[paste0(prefix, by_field)]]
    } else {
        choice <- title <- ""
    }
    tab <- input[[paste0(prefix, tab_field)]]
    if (is.null(choice) || is.null(tab)) {
        return(FALSE)
    }

    # Obtaining the old parameter choices, enforcing type and updating memory.
    # The new values should persist due to environment's pass-by-reference.
    if (param!='yaxis') {
        old_choice <- pObjects$memory[[mode]][id, by_field]
        choice <- as(choice, typeof(old_choice))
        pObjects$memory[[mode]][id, by_field] <- choice
    } else {
        old_choice <- choice
    }
    old_tab <- pObjects$memory[[mode]][id, tab_field]
    tab <- as(tab, typeof(old_tab))
    pObjects$memory[[mode]][id, tab_field] <- tab

    if (old_tab!=tab) {
        # Editing the table_links, if we're switching the table choice.
        pObjects$table_links <- .modify_table_links(pObjects$table_links, plot_name, tab, old_tab, mode=param)

        # Updating the selection, based on the currently selected row.
        if (tab!=.noSelection) {
            enc <- .encode_panel_name(tab)
            new_selected <- pObjects$memory[[enc$Type]][enc$ID, .rowStatSelected]
            old_selected <- pObjects$memory[[mode]][id, select_field]
            if (new_selected != old_selected && !is.null(session)) { # we use session=NULL only for unit testing the rest of the function.
                updateSelectizeInput(session, paste0(prefix, select_field), label = NULL, choices = select_choices, server = TRUE, selected = new_selected) # nocov
            }
        }
    }

    # Updating the link UI elements, but only if there was a change to table identities or linking.
    if ((choice==title)!=(old_choice==title) || (choice==title && tab!=old_tab)) {
        tab_names <- .decoded2encoded(setdiff(union(old_tab, tab), .noSelection))
        for (relinked in c(plot_name, tab_names)) {
            relink_field <- paste0(relinked, "_", .panelLinkInfo)
            rObjects[[relink_field]] <- .increment_counter(isolate(rObjects[[relink_field]]))
        }
    }

    # Not replotting if the row choice has not changed. Note that the identical-ness of 'tab'
    # doesn't matter here, as the row name determines the plot.
    return(!identical(old_choice, choice))
}

#' Delete table links for a destroyed panel
#'
#' Delete all references to any linked row statistics tables when a receiving panel is destroyed.
#'
#' @param mode String specifying the encoded panel type of the current (receiving) panel to be destroyed.
#' @param id Integer scalar specifying the index of the current panel of the specified type.
#' @param pObjects An environment containing \code{table_links}, a graph produced by \code{\link{.spawn_table_links}};
#' and \code{memory}, a list of DataFrames containing parameters for each panel of each type.
#'
#' @return \code{NULL}, invisibly.
#'
#' @details
#' This function will delete the encoded name of the receiving panel from the relevant vectors of its linked row tables in \code{pObjects$table_links}.
#' It will also replace all of its references to any linked row tables in memory with an empty string.
#'
#' Removal of references is necessary as the memories of inactive panels are not updated, for reasons of efficiency.
#' Thus, there is no guarantee that the linked tables are still active in the UI when the destroyed panel is reactivated.
#' This would result in an invalid memory state if the memory refers to inactive tables.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_delete_table_links
#' @seealso
#' \code{\link{.modify_table_links}},
#' \code{\link{iSEE}}
.delete_table_links <- function(mode, id, pObjects) {
    tmp_link <- pObjects$table_links
    tmp_mem <- pObjects$memory[[mode]]
    plot_name <- paste0(mode, id)

    if (mode %in% row_point_plot_types) {
        parameters <- list(c(.colorByColTable, "color"))
        if (mode=="sampAssayPlot") {
            parameters <- c(parameters, list(c(.sampAssayXAxisColTable, "xaxis"), c(.sampAssayYAxisColTable, "yaxis")))
        }
    } else {
        parameters <- list(c(.colorByRowTable, "color"))
        if (mode=="featAssayPlot") {
            parameters <- c(parameters, list(c(.featAssayXAxisRowTable, "xaxis"), c(.featAssayYAxisRowTable, "yaxis")))
        }
    }

    for (param in parameters) { 
        oldtab <- tmp_mem[id, param[1]]
        if (oldtab!=.noSelection) {
            tmp_link <- .modify_table_links(tmp_link, plot_name, .noSelection, oldtab, mode = param[2])
            tmp_mem[id, param[1]] <- .noSelection
        }
    }

    pObjects$memory[[mode]] <- tmp_mem
    pObjects$table_links <- tmp_link
    return(invisible(NULL))
}



