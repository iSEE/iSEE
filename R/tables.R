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
    
        for (i in seq_len(N)) {
            tab_name <- .check_for_tab(mode, i, memory, .colorByField, .colorByRowTableTitle, .colorByRowTable)
            if (!is.null(tab_name)) {
                table_links[[tab_name]]$color <- c(table_links[[tab_name]]$color, cur_panels[i])
            }
        }
    }
  
    # Adding links for x- and y-axes.
    N <- nrow(memory$featExprPlot)
    cur_panels <- sprintf("featExprPlot%i", seq_len(N))
    for (i in seq_len(N)) {
        tab_name <- .check_for_tab("featExprPlot", i, memory, .featExprXAxis, .featExprXAxisRowTableTitle, .featExprXAxisRowTable)
        if (!is.null(tab_name)) {
            table_links[[tab_name]]$xaxis <- c(table_links[[tab_name]]$xaxis, cur_panels[i])
        }
        
        tab_name <- .check_for_tab("featExprPlot", i, memory, .featExprYAxis, .featExprYAxisRowTableTitle, .featExprYAxisRowTable)
        if (!is.null(tab_name)) {
            table_links[[tab_name]]$yaxis <- c(table_links[[tab_name]]$yaxis, cur_panels[i])
        }
    }
  
    return(table_links)  
}


#' Does a linked table exist?
#'
#' Checks whether a linked row statistics table exists for a particular field in a receiving plot.
#'
#' @param mode String specifying the encoded panel type of the receiving plot.
#' @param i Integer scalar specifying the index of a panel of the specified type, for the receiving plot.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#' @param by_field String specifying the field to check for whether the input is using a row table input of any kind.
#' @param ref_title String specifying the title of a row table input, to match to the value of \code{by_field} in \code{memory} for this plot.
#' @param table_field String specifying the field to check for the identify of the row table input.
#'
#' @return A string containing the encoded name of the row statistics table from which the specified receiving plot takes input.
#' However, if the current plot is not receiving from any row table, or no row table is specified, \code{NULL} is returned instead.
#'
#' @details
#' Note that the row table is stored in memory using a decoded name.
#' This is converted to an encoded name for internal use by functions that call \code{.check_for_tab}.
#' 
#' @author Aaron Lun
#' @rdname INTERNAL_check_for_tab
#' @seealso
#' \code{\link{.spawn_table_links}}
.check_for_tab <- function(mode, i, memory, by_field, ref.title, table_field) {
    if (mode_memory[i, by_field]!=ref.title) {
        return(NULL)
    }
    cur_tab <- mode_memory[i, table_field]
    if (cur_tab=="") {
        return(NULL) 
    }
    return(.decoded2encoded(cur_tab))
}

#' Clear links for a destroyed table
#'
#' Clear all links to a row statistics table that has been destroyed.
#' 
#' @param pObjects An environment containing \code{table_links}, a graph produced by \code{.\link{spawn_table_links}};
#' and \code{memory}, a list of DataFrames containing parameters for each panel of each type.
#' @param tab String containing the encoded name of the row statistics table to be destroyed.
#' 
#' @return \code{NULL}, invisibly.
#'
#' @details
#' This function modifies \code{pObjects$table_links} to empty the character vectors containing the encoded names of the receiving plots.
#' It also modifies the \code{pObjects$memory} to remove all references to \code{tab} in the relevant fields of the receiving plots, 
#' replacing it with an empty string (i.e., no selection).
#'
#' Note that this function relies on pass-by-reference behaviour of \code{pObjects} to work properly.
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
        pObjects$memory[[type]][kid, .colorByRowTable] <- ""
    }

    for (x in all_kids$yaxis) {
        pObjects$memory$featExprPlot[x, .featExprYAxisRowTable] <- ""
    }
    for (x in all_kids$xaxis) {
        pObjects$memory$featExprPlot[x, .featExprXAxisRowTable] <- ""
    }

    # Erasing the links.
    links[[tab]] <- list(color=character(0), xaxis=character(0), yaxis=character(0))
    pObjects$table_links <- links
    return(invisible(NULL))
}

.modify_table_links <- function(links, dest, newtab, oldtab, mode='color') 
# This adds a table link between 'newtab' and 'dest', while removing 
# a link between 'oldtab' and 'dest'.
{
    if (oldtab!="") {
        oldtab <- .decoded2encoded(oldtab)
        links[[oldtab]][[mode]] <- setdiff(links[[oldtab]][[mode]], dest)
    }
    if (newtab!="") {
        newtab <- .decoded2encoded(newtab)
        links[[newtab]][[mode]] <- union(links[[newtab]][[mode]], dest)
    }
    return(links)
}

.setup_table_observer <- function(mode, i, input, pObjects, by_field, tab_title, tab_field, param='color')
# Convenience function to update table links and memory when 'input' changes.
# This can be for colours or for x/y-axis settings.
{
    choice <- input[[paste0(mode, i, "_", by_field)]]
    tab <- input[[paste0(mode, i, "_", tab_field)]]
    reset <- FALSE

    if (!is.null(choice) && !is.null(tab)) {
        # Editing the table_links, if we're switching to/from the table choice. 
        old <- pObjects$memory[[mode]][i, tab_field]
        plot_name <- paste0(mode, i)
        if (choice==tab_title) {
            pObjects$table_links <- .modify_table_links(pObjects$table_links, plot_name, tab, old, mode=param)
        } else {
            pObjects$table_links <- .modify_table_links(pObjects$table_links, plot_name, "", old, mode=param)
        }

        # Triggering replotting, but only if both of the input values are initialized.
        # We don't have an 'ignoreInit' that we can rely on here.
        reset <- TRUE
    }

    # Updating stored parameters. These should persist due to environment's pass-by-reference.
    if (!is.null(choice)) {
        pObjects$memory[[mode]][i, by_field] <- choice
    }
    if (!is.null(tab)) {
        pObjects$memory[[mode]][i, tab_field] <- tab
    }
    return(reset)
}

.find_linked_gene <- function(link, input)
# Convenience function to identify the selected gene from the linked table.
{
  if (link=="") {
    return(NULL)
  }
  tab.id <- .encode_panel_name(link)$ID
  linked.tab <- paste0("rowStatTable", tab.id, .int_rowStatSelected)
  input[[linked.tab]]
}

.delete_table_links <- function(mode, i, pObjects) 
# Destroys the table links when a PLOT is being deleted,
# and updates its memory to clear out all table references.
{
  tmp_link <- pObjects$table_links 
  tmp_mem <- pObjects$memory[[mode]]
  plot_name <- paste0(mode, i)

  for (param in list(c(.colorByField, .colorByRowTableTitle, .colorByRowTable, "color"),
                     c(.featExprXAxis, .featExprXAxisRowTableTitle, .featExprXAxisRowTable, "xaxis"),
                     c(.featExprYAxis, .featExprYAxisRowTableTitle, .featExprYAxisRowTable, "yaxis"))) {

    if (tmp_mem[i, param[1]]==param[2]) {
      oldtab <- tmp_mem[i, param[3]]
      if (oldtab!="") {
        tmp_link <- .modify_table_links(tmp_link, plot_name, "", oldtab, mode = param[4])
        tmp_mem[i, param[3]] <- ""
      }
    }

    if (mode!="featExprPlot") {
      break
    }
  }

  pObjects$memory[[mode]] <- tmp_mem
  pObjects$table_links <- tmp_link
  return(invisible(NULL))
}
