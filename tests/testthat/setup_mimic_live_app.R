# This defines the function required to mimic a particular state of the application,
# given all elements in memory.

mimic_live_app <- function(se, all_memory) {
    se <- iSEE:::.prepare_SE(se, ExperimentColorMap(), all_memory)
    init_out <- iSEE:::.setup_initial_state(se, all_memory)
    res_out <- iSEE:::.define_reservoir(se, list(), init_out$memory, init_out$counter)
    pObjects <- iSEE:::.create_persistent_objects(init_out$memory, res_out$reservoir, res_out$counter)

    rObjects <- list()
    ordering <- names(igraph::topo_sort(pObjects$selection_links, mode="out"))
    for (o in ordering) {
        stuff <- iSEE::.retrieveOutput(o, se, pObjects, rObjects)
        pObjects$varname[[o]] <- if (is(all_memory[[o]], "Table")) "tab" else "plot.data"
    }

    pObjects
}
