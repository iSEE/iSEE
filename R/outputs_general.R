#' @importFrom utils zip
#' @importFrom shiny downloadHandler renderPlot
.create_general_output <- function(se, input, output, session, pObjects, rObjects) {
    output[[.generalLinkGraphPlot]] <- renderPlot({
        force(input[[.generalLinkGraph]]) # trigger re-rendering every time the button is clicked.
        .snapshot_graph_linkedpanels(pObjects$selection_links,
            vapply(pObjects$memory, .getPanelColor, ""))
    })

    output[[.generalExportOutputAll]] <- downloadHandler(
        filename="iSEE_exports.zip",
        content=function(file) {
            dumptmp <- tempfile()
            dir.create(dumptmp)
            oldwd <- getwd()
            setwd(dumptmp) 

            on.exit({
                setwd(oldwd)
                unlink(dumptmp, recursive=TRUE)
            })

            # Loops through all panels, asks them for how they wish
            # to be summarized, and then saves their gunk to file.
            all.files <- list()
            for (i in seq_along(pObjects$memory)) {
                all.files[[i]] <- .exportOutput(pObjects$memory[[i]], se=se, 
                    all_memory=pObjects$memory, all_contents=pObjects$contents)
            }

            zip(file, files=unlist(all.files))
        }
    )
}
