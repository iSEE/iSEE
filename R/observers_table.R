#' @importFrom shiny observe updateSelectizeInput
.define_table_selection_observer <- function(mode, id, 
    x_field, y_field, col_field, choices,
    input, session, pObjects, rObjects) 
{
    # No need for underscore in 'select_field' definition, as this is already in the '.int' constant.
    panel_name <- paste0(mode, id)
    select_field <- paste0(panel_name, .int_statTableSelected)
    
    # Updating memory for new selection parameters 
    observe({
        chosen <- input[[select_field]]
        if (length(chosen)==0L) {
            return(NULL)
        }
        pObjects$memory[[mode]][id, .statTableSelected] <- chosen

        col_kids <- pObjects$table_links[[panel_name]][["color"]]
        x_kids <- pObjects$table_links[[panel_name]][["xaxis"]]
        y_kids <- pObjects$table_links[[panel_name]][["yaxis"]]

        # Updating the selectize for the color choice.
        col_kids <- sprintf("%s_%s", col_kids, col_field)
        for (kid in col_kids) {
            updateSelectizeInput(session, kid, label=NULL, server=TRUE, selected=chosen, choices=choices)
        }

        # Updating the selectize for the x-/y-axis choices.
        x_kids <- sprintf("%s_%s", x_kids, x_field)
        for (kid in x_kids) {
            updateSelectizeInput(session, kid, label=NULL, server=TRUE, selected=chosen, choices=choices)
        }
        y_kids <- sprintf("%s_%s", y_kids, y_field)
        for (kid in y_kids) {
            updateSelectizeInput(session, kid, label=NULL, server=TRUE, selected=chosen, choices=choices)
        }

        # There is a possibility that this would cause triple-rendering as they trigger different observers.
        # But this would imply that you're plotting/colouring the same gene against itself, which would be stupid.
    })
}
