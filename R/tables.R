#' Create table links
#'
#' Creates a record of the links between row statistics tables and the plots that receive input from them.
#'
#' @param memory A list of DataFrames, where each DataFrame corresponds to a panel type and contains the initial settings for each individual panel of that type.
#'
#' @return A list where each entry is itself a list, named with the encoded name of a row statistics table.
#' Each table-specific list contains:
#' \itemize{
#' \item \code{"xaxis"}, a character vector containing encoded names of (feature expression) plots that receive input from this table for x-axis specification.
#' \item \code{"yaxis"}, a character vector containing encoded names of (feature expression) plots that receive input from this table for y-axis specification.
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
    Ntabs <- nrow(memory$rowStatTable)
    table_links <- rep(list(list(color=character(0), xaxis=character(0), yaxis=character(0))), Ntabs)
    names(table_links) <- sprintf("rowStatTable%i", seq_len(Ntabs))
  
    # Adding the links for the colors.
    for (mode in c("redDimPlot", "colDataPlot", "featExprPlot", "rowDataPlot")) { 
        N <- nrow(memory[[mode]])
        cur_panels <- sprintf("%s%i", mode, seq_len(N))
    
        for (id in seq_len(N)) {
            cur_tab <- memory[[mode]][id, .colorByRowTable]
            if (cur_tab!=.noSelection) {
                tab_name <- decoded2encoded(cur_tab)
                table_links[[tab_name]]$color <- c(table_links[[tab_name]]$color, cur_panels[id])
            }
        }
    }
  
    # Adding links for x- and y-axes.
    N <- nrow(memory$featExprPlot)
    cur_panels <- sprintf("featExprPlot%i", seq_len(N))
    for (id in seq_len(N)) {
        param_choices <- memory[["featExprPlot"]][id,]

        X_tab <- param_choices[[.featExprXAxisRowTable]]
        if (X_tab!=.noSelection) {
            tab_name <- decoded2encoded(X_tab)
            table_links[[tab_name]]$xaxis <- c(table_links[[tab_name]]$xaxis, cur_panels[id])
        }
        
        Y_tab <- param_choices[[.featExprYAxisRowTable]]
        if (Y_tab!=.noSelection) {
            tab_name <- .decoded2encoded(Y_tab)            
            table_links[[tab_name]]$yaxis <- c(table_links[[tab_name]]$yaxis, cur_panels[id])
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

    # Updating the memory of all linked plots.
    col_kids <- all_kids$color
    enc <- .split_encoded(col_kids)
    for (i in seq_along(col_kids)) { 
        kid <- col_kids[i]
        type <- enc$Type[i]
        pObjects$memory[[type]][kid, .colorByRowTable] <- .noSelection
    }

    for (x in all_kids$yaxis) {
        pObjects$memory$featExprPlot[x, .featExprYAxisRowTable] <- .noSelection
    }
    for (x in all_kids$xaxis) {
        pObjects$memory$featExprPlot[x, .featExprXAxisRowTable] <- .noSelection
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
#' @param by_field String specifying the field to check for whether the input is using a row table input of any kind.
#' @param title String specifying the title of a feature name input, to match to the value of \code{by_field} in \code{memory} for this plot.
#' @param feat_field String specifying the field to check for the feature to examine. 
#' @param tab_field String specifying the field to check for the identify of the row table input.
#' @param feat_choices Vector of consecutive integers indexing all rows, named with the feature names.
#' @param param String specifying the type of table link to the current plot, i.e., color or x/y-axis.
#'
#' @return A logical scalar indicating whether the current (receiving) plot needs to be regenerated.
#'
#' @details
#' This function has a number of side-effects, relying on the pass-by-reference behaviour of \code{pObjects}, \code{rObjects} and \code{session} to perform its role.
#' \itemize{
#' \item New values of the fields \code{by_field}, \code{tab_field} and \code{feat_field} in \code{input} overwrite values in \code{pObjects$memory} for the current panel.
#' \item \code{pObjects$table_links} is modified for the new linked table in \code{input[[tab_field]]}.
#' Note that table links are \emph{not} destroyed if \code{input[[by_field]]} is different to \code{title},
#' as this could result in failure to clear the memory of the current panel in \code{\link{.delete_table_links}}.
#' \item Link panel counters in \code{rObjects} are incremented if the new linked table differs from the old link table, 
#' or if \code{input[[by_field]]} differs from that in memory and either of them is equal to \code{title}.
#' Counters are only updated for the current panel as well as the old/new tables, and only when 
#' \item The selectize UI element corresponding to \code{feat_field} is updated with the current selection in the linked table, if a new linked table was chosen.
#' Note that this will trigger another call to the observer that contains this function.
#' }
#'
#' The flag to regenerate the current plot is set if the \code{by_field} and \code{tab_field} fields in \code{input} are non-\code{NULL};
#' the \code{feat_field} entry is neither \code{NULL} nor an empty string; 
#' and the \code{by_field} entry differs from the parameter currently stored in memory,
#' or the value of \code{by_field} is equal to \code{title} \emph{and} the input and memory values of \code{feat_field} are different.
#'
#' Note that \code{by_field} and \code{title} are ignored if \code{param="yaxis"}, as the y-axis of feature expression plots have no other choice of variable.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_setup_table_observer
#' @seealso
#' \code{\link{.modify_table_links}},
#' \code{\link{iSEE}}
#'
#' @importFrom shiny updateSelectizeInput
.setup_table_observer <- function(mode, id, pObjects, rObjects, input, session, 
                                  by_field, title, feat_field, tab_field, 
                                  feat_choices, param='color') 
{
    plot_name <- paste0(mode, id)
    prefix <- paste0(plot_name, "_")

    # Checking that all the incoming arguments are non-NULL.
    if (param!='yaxis') { 
        choice <- input[[paste0(prefix, by_field)]]
    } else {
        choice <- title <- ""
    }
    feature <- input[[paste0(prefix, feat_field)]]
    tab <- input[[paste0(prefix, tab_field)]]
    if (is.null(choice) || is.null(tab) || is.null(feature) || feature=="") {
        return(FALSE)
    }

    # Obtaining the old parameter choices and enforcing type.
    if (param!='yaxis') { 
        old_choice <- pObjects$memory[[mode]][id, by_field]
    } else {
        old_choice <- ""
    }
    old_feature <- pObjects$memory[[mode]][id, feat_field]
    old_tab <- pObjects$memory[[mode]][id, tab_field]
    choice <- as(choice, typeof(old_choice))
    feature <- as(feature, typeof(old_feature))
    tab <- as(tab, typeof(old_tab))

    # Updating stored parameters. These should persist due to environment's pass-by-reference.
    if (param!='yaxis') { 
        pObjects$memory[[mode]][id, by_field] <- choice
    }
    pObjects$memory[[mode]][id, feat_field] <- feature
    pObjects$memory[[mode]][id, tab_field] <- tab

    if (old_tab!=tab) {
        # Editing the table_links, if we're switching the table choice. 
        pObjects$table_links <- .modify_table_links(pObjects$table_links, plot_name, tab, old_tab, mode=param)

        # Updating the feature selection, based on the currently selected row.
        if (tab!=.noSelection) { 
            enc_id <- .encode_panel_name(tab)$ID
            tab_chosen <- pObjects$memory$rowStatTable[enc_id,.rowStatSelected]
            if (tab_chosen!=feature && !is.null(session)) { # session=NULL used for testing the rest of the function.
                updateSelectizeInput(session, paste0(prefix, feat_field), label = NULL, 
                                     choices = feat_choices, server = TRUE, selected = tab_chosen)
            }
        }
    }

    # Updating the link UI elements, but only if there was a change to table identities or linking.
    if ((choice==title)!=(old_choice==title) || tab!=old_tab) {
        tab_names <- .decoded2encoded(setdiff(union(old_tab, tab), .noSelection))
        for (relinked in c(plot_name, tab_names)) {
            relink_field <- paste0(relinked, "_", .panelLinkInfo)
            rObjects[[relink_field]] <- .increment_counter(isolate(rObjects[[relink_field]]))
        }
    }

    # Not replotting if none of the variables have changed.
    # Note that the identical-ness of 'tab' doesn't matter, as long as 'feature' is the same.
    # Of course, feature only has an effect when choice==title.
    reset <- !identical(old_choice, choice) || (choice==title && !identical(old_feature, feature))
    return(reset)
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
  
    for (param in list(c(.colorByRowTable, "color"),
                       c(.featExprXAxisRowTable, "xaxis"),
                       c(.featExprYAxisRowTable, "yaxis"))) {
  
        oldtab <- tmp_mem[id, param[1]]
        if (oldtab!=.noSelection) {
            tmp_link <- .modify_table_links(tmp_link, plot_name, .noSelection, oldtab, mode = param[2])
            tmp_mem[id, param[1]] <- .noSelection
        }
    
        if (mode!="featExprPlot") {
            break
        }
    }
  
    pObjects$memory[[mode]] <- tmp_mem
    pObjects$table_links <- tmp_link
    return(invisible(NULL))
}
