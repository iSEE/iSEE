#' @importFrom utils zip
#' @importFrom shiny downloadHandler
.create_general_output <- function(se, input, output, session, pObjects, rObjects) {
    output[[.generalExportOutputAll]] <- downloadHandler(
        filename="iSEE_exports.zip",
        content=function(file) {
            dumptmp <- tempfile()
            dir.create(dumptmp)
            on.exit(unlink(dumptmp, recursive=TRUE))

            # Loops through all panels, asks them for how they wish
            # to be summarized, and then saves their gunk to file.
            all.files <- list()
            for (i in seq_along(pObjects$memory)) {
                all.files[[i]] <- .exportOutput(pObjects$memory[[i]], dumptmp, se=se, 
                    all_memory=pObjects$memory, all_contents=pObjects$contents)
            }

            zip(file, files=unlist(all.files))
        }
    )
}
