#' Render general output for the app
#'
#' Create rendering expressions for \code{\link{iSEE}} outputs that are general to the entire app, not just specific panels.
#'
#' @param se A \linkS4class{SummarizedExperiment} object containing the data of interest.
#' @param input The Shiny input object from the server function.
#' @param output The Shiny outpu object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return A \linkS4class{NULL} is invisibly returned
#' and rendering expressions for general app features are added to \code{output}.
#'
#' @author Aaron Lun, Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_create_general_output
#' @importFrom utils zip
#' @importFrom shiny downloadHandler renderPlot checkboxGroupInput actionButton downloadButton withProgress incProgress
#' @importFrom pagedown chrome_print
.create_general_output <- function(se, input, output, session, pObjects, rObjects) {
    # nocov start
    output[[.generalLinkGraphPlot]] <- renderPlot({
        force(input[[.generalLinkGraph]]) # trigger re-rendering every time the button is clicked.
        .snapshot_graph_linkedpanels(pObjects$selection_links,
            vapply(pObjects$memory, .getPanelColor, ""))
    })
    # nocov end
    # nocov start
    output[[.generalExportOutputUI]] <- renderUI({
        force(input[[.generalExportOutput]]) # trigger rerendering every time the button is clicked.
        all_options <- .define_export_choices(pObjects$memory)
        tagList(
            checkboxGroupInput(.generalExportOutputChoices, label=NULL,
                choices=all_options, selected=all_options),
            actionButton(.generalExportOutputAll, label="Select all"),
            actionButton(.generalExportOutputNone, label="Select none"),
            downloadButton(.generalExportOutputDownload, "Download"),
            downloadButton(.generalExportOutputSlides, "As slides")
        )
    })
    # nocov end
    # nocov start
    output[[.generalMemoryExport]] <- downloadHandler(
        filename="iSEE_memory.rds",
        content=function(file) {
            saveRDS(file=file, pObjects$memory)
        }
    )
    # nocov end
    # nocov start
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
            n_plots <- length(input[[.generalExportOutputChoices]])
            withProgress(message = 'Generating plots', value = 0, max = n_plots, {
                for (i in input[[.generalExportOutputChoices]]) {
                    i_object <- pObjects$memory[[i]]
                    incProgress(1, detail = sprintf("Making '%s'", .getFullName(i_object)))
                    
                    all.files[[i]] <- .exportOutput(i_object, se=se,
                        all_memory=pObjects$memory, all_contents=pObjects$contents)
                }
                
            }, session = session)

            zip(file, files=unlist(all.files))
        }
    )
    # nocov end
    # nocov start
    output[[.generalExportOutputSlides]] <- downloadHandler(
        filename="iSEE_slides.zip",
        content=function(file) {
            dumptmp <- tempfile()
            dir.create(dumptmp)
            oldwd <- getwd()
            setwd(dumptmp)
            
            on.exit({
                setwd(oldwd)
                unlink(dumptmp, recursive=TRUE)
            })
            
            rmd_tmpfile <- "iSEE_slides.Rmd"
            
            # Copy Rmd template and custom CSS
            file.copy(from = system.file(package = "iSEE", "slides", "header.Rmd"), to = rmd_tmpfile, overwrite = TRUE)
            file.copy(from = system.file(package = "iSEE", "slides", "my-theme.css"), to = "my-theme.css", overwrite = TRUE)
            file.copy(from = system.file(package = "iSEE", "slides", "references.bib"), to = "references.bib", overwrite = TRUE)
            
            # Dump se to an RDS file that the presentation can work from
            saveRDS(object = se, file = "se.rds")
            
            # Loops through all panels, add their code chunk to the Rmd file
            # Compile the Rmd into PDF and 
            all.slides <- list()
            n_plots <- length(input[[.generalExportOutputChoices]])
            withProgress(message = 'Collating markdown', value = 1, max = 3, {
                for (i in input[[.generalExportOutputChoices]]) {
                    i_object <- pObjects$memory[[i]]
                    
                    if (!is(i_object, "DotPlot")) {
                        next
                    }
                    
                    code_out <- .exportMarkdownSlide(i_object, se=se, all_memory=pObjects$memory, all_contents=pObjects$contents)
                    if (is(i_object, "DotPlot")) {
                        code_out <- c(code_out, "dot.plot")
                    }
                    code_out <- c(
                        sprintf("# %s", .getFullName(i_object)),
                        "",
                        sprintf("```{r %s, echo=FALSE, fig.align='center'}", .getEncodedName(i_object)),
                        code_out,
                        "```"
                    )
                    code_out <- paste0(code_out, collapse = "\n")
                    all.slides[[i]] <- code_out
                }
                
                rmd_out <- paste0(all.slides, collapse = "\n\n---\n\n")
                write(x = rmd_out, file = rmd_tmpfile, append = TRUE, sep = "\n")
                
                # Appendix
                rmd_out <- scan(file = system.file(package = 'iSEE', 'slides', 'footer.Rmd'), what = "character", sep = "\n", blank.lines.skip = FALSE)
                rmd_out <- c(
                    '',
                    '---',
                    '',
                    rmd_out
                )
                write(x = rmd_out, file = rmd_tmpfile, append = TRUE, sep = "\n")
                
                # Compile slides in HTML and PDF
                incProgress(1, message = 'Running chrome_print()', session = session)
                chrome_print(input = rmd_tmpfile)
                # knitr::knit(input = rmd_tmpfile, quiet = FALSE)
                # rmarkdown::render(input = rmd_tmpfile, quiet = TRUE)
                
                incProgress(1, message = 'Cleaning up', session = session)
                file.remove("se.rds")
                
            }, session = session)
            
            zip(file, files=list.files("."))
        }
    )
    # nocov end
    invisible(NULL)
}

#' Define export choices
#'
#' Define the available panels that can be chosen for exporting content.
#'
#' @param all_memory A list of \linkS4class{Panel}s specifying the current state of the app.
#'
#' @return A character vector of length equal to \code{all_memory}, containing the encoded panel names.
#' The names of this vector are set to the full panel names for display purposes.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_define_export_choices
.define_export_choices <- function(all_memory) {
    if (length(all_memory)) {
        all_options <- vapply(all_memory, .getEncodedName, "")
        names(all_options) <- vapply(all_memory, .getFullName, "")
    } else {
        all_options <- character(0)
    }
    all_options
}
