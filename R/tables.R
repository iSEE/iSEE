.spawn_table_links <- function(memory) 
# Creates links between tables and dependent plots.
{
  Ntabs <- nrow(memory$geneStat)
  table_links <- rep(list(list(color=list(), xaxis=list(), yaxis=list())), Ntabs)
  names(table_links) <- sprintf("geneStatTable%i", seq_len(Ntabs))

  # Adding the links for the colors.
  for (mode in c("redDim", "colData", "geneExpr")) { 
    N <- nrow(memory[[mode]])
    cur_panels <- sprintf("%sPlot%i", mode, seq_len(N))

    for (i in seq_len(N)) {
      tab_name <- .check_for_tab(i, memory[[mode]], .colorByField, .colorByGeneTableTitle, .colorByGeneTable)
      if (!is.null(tab_name)) {
        table_links[[tab_name]]$color <- c(table_links[[tab_name]]$color, cur_panels[i])
      }
    }
  }

  # Adding links for x- and y-axes.
  N <- nrow(memory$geneExpr)
  cur_panels <- sprintf("geneExprPlot%i", seq_len(N))
  for (i in seq_len(N)) {
    tab_name <- .check_for_tab(i, memory$geneExpr, .geneExprXAxis, .geneExprXAxisGeneTableTitle, .geneExprXAxisGeneTable)
    if (!is.null(tab_name)) {
      table_links[[tab_name]]$color <- c(table_links[[tab_name]]$xaxis, cur_panels[i])
    }

    tab_name <- .check_for_tab(i, memory$geneExpr, .geneExprYAxis, .geneExprYAxisGeneTableTitle, .geneExprYAxisGeneTable)
    if (!is.null(tab_name)) {
      table_links[[tab_name]]$color <- c(table_links[[tab_name]]$yaxis, cur_panels[i])
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
        pObjects$memory[[type]][kid, .colorByGeneTable] <- ""
    }

    for (x in all_kids$yaxis) {
        pObjects$memory$geneExpr[x, .geneExprYAxisGeneTable] <- ""
    }
    for (x in all_kids$xaxis) {
        pObjects$memory$geneExpr[x, .geneExprXAxisGeneTable] <- ""
    }

    # Erasing the links.
    links[[tab]] <- list(color=list(), yaxis=list(), xaxis=list())
    pObjects$table_links <- links
    return(invisible(NULL))
}

.modify_table_links <- function(links, dest, newtab, oldtab, mode='color') 
# This adds a table link between 'tab' and 'dest'.    
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
