# This defines the function required to mimic a particular state of the application,
# given all elements in memory.

mimic_live_app <- function(se, all_memory) {
    metadata(se)$colormap <- ExperimentColorMap()
    for (m in seq_along(all_memory)) {
        se <- .cacheCommonInfo(all_memory[[m]], se)
    }
    for (m in seq_along(all_memory)) {
        all_memory[[m]] <- .refineParameters(all_memory[[m]], se)
    }

    pObjects <- new.env()
    pObjects$memory <- all_memory
    pObjects$selection_links <- iSEE:::.spawn_multi_selection_graph(memory)
    pObjects$coordinates <- list()
    pObjects$commands <- list()
    pObjects$varname <- list()
    pObjects$contents <- list()
    rObjects <- list()

    ordering <- names(igraph::topo_sort(pObjects$selection_links, mode="out"))
    for (o in ordering) {
        stuff <- iSEE::.retrieveOutput(o, se, pObjects, rObjects)
        pObjects$varname[[o]] <- if (is(all_memory[[o]], "Table")) "tab" else "plot.data"
    }

    pObjects
}
