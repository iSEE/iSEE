#' @importFrom shiny observe updateSelectizeInput
.define_table_observers <- function(panel_name, input, session, pObjects, rObjects) {
    # Note that '.int' variables already have underscores, so these are not necessary.
    select_field <- paste0(panel_name, .int_statTableSelected)
    observeEvent(input[[select_field]], {
        chosen <- input[[select_field]]
        if (length(chosen)==0L) {
            return(NULL)
        }

        chosen <- rownames(pObjects$coordinates[[panel_name]])[chosen]
        previous <- pObjects$memory[[panel_name]][[.TableSelected]]
        if (chosen==previous) {
            return(NULL)
        }
        pObjects$memory[[panel_name]][[.TableSelected]] <- chosen

        .safe_reactive_bump(rObjects, paste0(panel_name, "_", .propagateDimnames))
    })

    search_field <- paste0(panel_name, .int_statTableSearch)
    act_field <- paste0(panel_name, "_reactivated")
    observeEvent(input[[search_field]], {
        pObjects$memory[[panel_name]][[.statTableSearch]] <- input[[search_field]]
        .safe_reactive_bump(rObjects, act_field)
    })

    colsearch_field <- paste0(panel_name, .int_statTableColSearch)
    observeEvent(input[[colsearch_field]], {
        search <- input[[colsearch_field]]

        # Usually getting rid of the secret column added to filter the table.
        if (ncol(pObjects$coordinates[[panel_name]]) < length(search)) {
            search <- head(search, -1)
        }

        pObjects$memory[[panel_name]][[.statTableColSearch]] <- search
        .safe_reactive_bump(rObjects, act_field)
    })
}
