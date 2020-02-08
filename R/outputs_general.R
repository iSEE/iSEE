#' @importFrom utils zip
#' @importFrom shiny downloadHandler renderPlot checkboxGroupInput actionButton
.create_general_output <- function(se, input, output, session, pObjects, rObjects) {
    output[[.generalLinkGraphPlot]] <- renderPlot({
        force(input[[.generalLinkGraph]]) # trigger re-rendering every time the button is clicked.
        .snapshot_graph_linkedpanels(pObjects$selection_links,
            vapply(pObjects$memory, .getPanelColor, ""))
    })

    output[[.generalExportOutputUI]] <- renderUI({
        force(input[[.generalExportOutput]]) # trigger rerendering every time the button is clicked.
        all_options <- .define_export_choices(pObjects$memory)
        tagList(
            checkboxGroupInput(.generalExportOutputChoices, label=NULL,
                choices=all_options, selected=all_options),
            actionButton(.generalExportOutputAll, label="Select all"),
            actionButton(.generalExportOutputNone, label="Select none"),
            downloadButton(.generalExportOutputDownload, "Download")
        )
    })

    output[[.generalExportOutputDownload]] <- downloadHandler(
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
            for (i in input[[.generalExportOutputChoices]]) {
                all.files[[i]] <- .exportOutput(pObjects$memory[[i]], se=se, 
                    all_memory=pObjects$memory, all_contents=pObjects$contents)
            }

            zip(file, files=unlist(all.files))
        }
    )
}

.define_export_choices <- function(all_memory) {
    all_options <- vapply(all_memory, .getEncodedName, "")
    names(all_options) <- vapply(all_memory, .getFullName, "")
    all_options
}
