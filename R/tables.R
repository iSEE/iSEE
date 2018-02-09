.spawn_table_links <- function(memory) 
# Creates links between tables and dependent plots, based on the colour
# of all plots or x/y-axis choices for the gene expression plots.
{
  Ntabs <- nrow(memory$rowStatTable)
  table_links <- rep(list(list(color=character(0), xaxis=character(0), yaxis=character(0))), Ntabs)
  names(table_links) <- sprintf("rowStatTable%i", seq_len(Ntabs))

  # Adding the links for the colors.
  for (mode in c("redDimPlot", "colDataPlot", "featExprPlot", "rowDataPlot")) { 
    N <- nrow(memory[[mode]])
    cur_panels <- sprintf("%s%i", mode, seq_len(N))

    for (i in seq_len(N)) {
      tab_name <- .check_for_tab(i, memory[[mode]], .colorByField, .colorByRowTableTitle, .colorByRowTable)
      if (!is.null(tab_name)) {
        table_links[[tab_name]]$color <- c(table_links[[tab_name]]$color, cur_panels[i])
      }
    }
  }

  # Adding links for x- and y-axes.
  N <- nrow(memory$featExprPlot)
  cur_panels <- sprintf("featExprPlot%i", seq_len(N))
  for (i in seq_len(N)) {
    tab_name <- .check_for_tab(i, memory$featExprPlot, .featExprXAxis, .featExprXAxisRowTableTitle, .featExprXAxisRowTable)
    if (!is.null(tab_name)) {
      table_links[[tab_name]]$xaxis <- c(table_links[[tab_name]]$xaxis, cur_panels[i])
    }

    tab_name <- .check_for_tab(i, memory$featExprPlot, .featExprYAxis, .featExprYAxisRowTableTitle, .featExprYAxisRowTable)
    if (!is.null(tab_name)) {
      table_links[[tab_name]]$yaxis <- c(table_links[[tab_name]]$yaxis, cur_panels[i])
    }
  }

  return(table_links)  
}

.check_for_tab <- function(i, mode_memory, by.field, ref.title, table.field) {
  if (mode_memory[i, by.field]!=ref.title) {
    return(NULL)
  }
  cur_tab <- mode_memory[i, table.field]
  if (cur_tab=="") {
    return(NULL) 
  }
  return(.decoded2encoded(cur_tab))
}

.destroy_table <- function(pObjects, tab) 
# This resets all links for the current table, and updates
# the memory of all dependent plots to not have this table.
# We assume pObjects is an environment for pass-by-reference.
{
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
