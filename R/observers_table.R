#' @importFrom shiny observe updateSelectizeInput
.define_table_observers <- function(panel_name, input, session, pObjects, rObjects) {
    # Note that '.int' variables already have underscores, so these are not necessary.
    select_field <- paste0(panel_name, .int_statTableSelected)
    observeEvent(input[[select_field]], {
        chosen <- input[[select_field]]
        if (length(chosen)==0L) {
            return(NULL)
        }

        chosen <- rownames(pObjects$contents[[panel_name]])[chosen]
        previous <- pObjects$memory[[panel_name]][[.TableSelected]]
        if (chosen==previous) {
            return(NULL)
        }
        pObjects$memory[[panel_name]][[.TableSelected]] <- chosen

        .safe_reactive_bump(rObjects, paste0(panel_name, "_", .propagateDimnames))
    })

    search_field <- paste0(panel_name, .int_statTableSearch)
    act_name <- paste0(panel_name, "_", .panelReactivated)
    gen_name <- paste0(panel_name, "_", .panelGeneralInfo)
    observeEvent(input[[search_field]], {
        pObjects$memory[[panel_name]][[.TableSearch]] <- input[[search_field]]
        .safe_reactive_bump(rObjects, act_name)
        .safe_reactive_bump(rObjects, gen_name)
    })

    colsearch_field <- paste0(panel_name, .int_statTableColSearch)
    observeEvent(input[[colsearch_field]], {
        search <- input[[colsearch_field]]
        pObjects$memory[[panel_name]][[.TableColSearch]] <- search
        .safe_reactive_bump(rObjects, act_name)
        .safe_reactive_bump(rObjects, gen_name)
    })
}
