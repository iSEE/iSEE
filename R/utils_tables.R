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

.generate_table_filter <- function(panel, varname="tab") {
    filters <- NULL

    search <- panel[[.TableSearch]]
    if (search!="") {
        filters <- c(filters,
            sprintf("Reduce('|', lapply(%s, FUN=grepl, pattern=%s))", 
                varname, deparse(search)))
    }

    searchcols <- panel[[.TableColSearch]]
    involved <- which(searchcols!="")
    if (length(involved)) {
        filters <- c(filters, sprintf("iSEE::filterDTColumn(%s[[%i]], %s)", 
            varname, involved, vapply(searchcols[involved], deparse, "")))
    }

    if (!is.null(filters)) {
        filters <- paste(filters, collapse="\n    & ")
    }
    filters
}


