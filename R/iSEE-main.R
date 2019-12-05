#' iSEE: interactive SummarizedExperiment/SingleCellExperiment Explorer
#'
#' Interactive and reproducible visualization of data contained in a
#' SummarizedExperiment/SingleCellExperiment, using a Shiny interface.
#'
#' @param se An object that is coercible to \linkS4class{SingleCellExperiment}.
#' If missing, an app is launched with a file upload control allowing users to upload an RDS file that contains such as object.
#' See Details for information to set the maximal size limit for file uploads.
#' @param redDimArgs A DataFrame similar to that produced by \code{\link{redDimPlotDefaults}}, specifying initial parameters for the reduced dimension plots.
#' @param colDataArgs A DataFrame similar to that produced by \code{\link{colDataPlotDefaults}}, specifying initial parameters for the column data plots.
#' @param featAssayArgs A DataFrame similar to that produced by \code{\link{featAssayPlotDefaults}}, specifying initial parameters for the feature assay plots.
#' @param rowStatArgs A DataFrame similar to that produced by \code{\link{rowStatTableDefaults}}, specifying initial parameters for the row statistics tables.
#' @param rowDataArgs A DataFrame similar to that produced by \code{\link{rowDataPlotDefaults}}, specifying initial parameters for the row data plots.
#' @param sampAssayArgs A DataFrame similar to that produced by \code{\link{sampAssayPlotDefaults}}, specifying initial parameters for the sample assay plots.
#' @param colStatArgs A DataFrame similar to that produced by \code{\link{colStatTableDefaults}}, specifying initial parameters for the column statistics tables.
#' @param customDataArgs A DataFrame similar to that produced by \code{\link{customDataPlotDefaults}}, specifying initial parameters for the custom data plots.
#' @param customStatArgs A DataFrame similar to that produced by \code{\link{customStatTableDefaults}}, specifying initial parameters for the custom statistics tables.
#' @param heatMapArgs A DataFrame similar to that produced by \code{\link{heatMapPlotDefaults}}, specifying initial parameters for the heatmaps.
#' @param redDimMax An integer scalar specifying the maximum number of reduced dimension plots in the interface.
#' @param colDataMax An integer scalar specifying the maximum number of column data plots in the interface.
#' @param featAssayMax An integer scalar specifying the maximum number of feature assay plots in the interface.
#' @param rowStatMax An integer scalar specifying the maximum number of row statistics tables in the interface.
#' @param rowDataMax An integer scalar specifying the maximum number of row data plots in the interface.
#' @param sampAssayMax An integer scalar specifying the maximum number of sample assay plots in the interface.
#' @param colStatMax An integer scalar specifying the maximum number of column statistics tables in the interface.
#' @param customDataMax An integer scalar specifying the maximum number of custom data plots in the interface.
#' @param customStatMax An integer scalar specifying the maximum number of custom statistics tables in the interface.
#' @param heatMapMax An integer scalar specifying the maximum number of heatmaps in the interface.
#' @param initialPanels A DataFrame specifying which panels should be created at initialization.
#' This should contain a \code{Name} character field and may have optional \code{Width} and \code{Height} integer fields, see Details.
#' @param annotFun A function, similar to those returned by \code{\link{annotateEntrez}} or \code{\link{annotateEnsembl}}.
#' The function should accept two parameters, \code{se} and \code{row_index}, and return a HTML element with annotation for the selected row.
#' @param customDataFun A named list of functions for reporting coordinates to use in a custom data plot.
#' @param customStatFun A named list of functions for reporting coordinates to use in a custom statistics table.
#' @param customSendAll A logical scalar indicating whether all (active and saved) selections should be passed from transmitting panels to custom plots or tables.
#' The default (\code{FALSE}) only passes the row names of the points in the active selection.
#' @param colormap An \linkS4class{ExperimentColorMap} object that defines custom colormaps to apply to individual \code{assays}, \code{colData} and \code{rowData} covariates.
#' @param tour A data.frame with the content of the interactive tour to be displayed after starting up the app.
#' @param appTitle A string indicating the title to be displayed in the app.
#' If not provided, the app displays the version info of \code{\link{iSEE}}.
#' @param runLocal A logical indicating whether the app is to be run locally or remotely on a server, which determines how documentation will be accessed.
#' @param voice A logical indicating whether the voice recognition should be enabled.
#' @param bugs Set to \code{TRUE} to enable the bugs Easter egg.
#' Alternatively, a named numeric vector control the respective number of each bug type (e.g., \code{c(bugs=3L, spiders=1L)}).
#' Credits to https://github.com/Auz/Bug for the JavaScript code.
#'
#' @details
#' Users can pass default parameters via DataFrame objects in \code{redDimArgs} and \code{featAssayArgs}.
#' Each object can contain some or all of the expected fields (see \code{\link{redDimPlotDefaults}}).
#' Any missing fields will be filled in with the defaults.
#'
#' The number of maximum plots for each type of plot is set to the larger of \code{*Max} and \code{nrow(*Args)}.
#' Users can specify any number of maximum plots, though increasing the number will increase the time required to render the interface.
#'
#' The \code{initialPanels} argument specifies the panels to be created upon initializing the interface.
#' This should be a DataFrame containing a \code{Name} field specifying the identity of the panel, e.g., \code{"Reduced dimension plot 1"}, \code{"Row statistics table 2"}.
#' Please refer to \code{\link{availablePanelTypes}} for the full list of panels available.
#' The trailing number should not be greater than the number of maximum plots of that type.
#' Users can also define the \code{Width} field, specifying the width of each panel from 2 to 12 (values will be coerced inside this range);
#' and the \code{Height} field, specifying the height of each panel from 400 to 1000 pixels.
#' By default, one panel of each type (where possible) will be generated, with height of 500 and width of 4.
#'
#' The \code{tour} argument needs to be provided in a form compatible with the format expected by the \code{rintrojs} package.
#' There should be two columns, \code{element} and \code{intro}, with the former describing the element to highlight and the latter providing some descriptive text.
#' See \url{https://github.com/carlganz/rintrojs#usage} for more information.
#'
#' By default, categorical data types such as factor and character are limited to 24 levels, beyond which they are coerced to numeric variables for faster plotting.
#' This limit may be set to a different value as a global option, e.g. \code{options(iSEE.maxlevels=30)}.
#'
#' By default, the maximum request size for file uploads defaults to 5MB (https://shiny.rstudio.com/reference/shiny/0.14/shiny-options.html).
#' To raise the limit (e.g., 50MB), run \code{options(shiny.maxRequestSize=50*1024^2)}.
#'
#' @return A Shiny app object is returned, for interactive data exploration of the \linkS4class{SummarizedExperiment} or \linkS4class{SingleCellExperiment} object.
#'
#' @export
#' @importFrom utils packageVersion
#' @importFrom shinydashboard dashboardBody dashboardHeader dashboardPage dashboardSidebar menuItem tabBox valueBox valueBoxOutput dropdownMenu notificationItem
#' @importFrom utils packageVersion read.delim citation sessionInfo browseURL head
#' @importFrom shinyjs useShinyjs
#' @importFrom rintrojs introjsUI introjs
#' @importFrom shiny reactiveValues uiOutput
#' renderUI renderPrint
#' actionButton selectizeInput
#' shinyApp runApp
#' HTML br icon hr p em strong img code pre h1
#' tagList tags
#' tabsetPanel tabPanel
#' includeCSS singleton includeScript
#' fileInput observeEvent isolate
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom S4Vectors DataFrame
#' @importFrom methods as
#' @importFrom cowplot plot_grid
#'
#' @examples
#' library(scRNAseq)
#'
#' # Example data ----
#' sce <- ReprocessedAllenData(assays="tophat_counts")
#' class(sce)
#'
#' library(scater)
#' sce <- logNormCounts(sce, exprs_values="tophat_counts")
#'
#' sce <- runPCA(sce, ncomponents=4)
#' sce <- runTSNE(sce)
#' rowData(sce)$ave_count <- rowMeans(assay(sce, "tophat_counts"))
#' rowData(sce)$n_cells <- rowSums(assay(sce, "tophat_counts") > 0)
#' sce
#'
#' # launch the app itself ----
#'
#' app <- iSEE(sce)
#' if (interactive()) {
#'   shiny::runApp(app, port=1234)
#' }
iSEE <- function(se,
    redDimArgs=NULL,
    colDataArgs=NULL,
    featAssayArgs=NULL,
    rowStatArgs=NULL,
    rowDataArgs=NULL,
    sampAssayArgs=NULL,
    colStatArgs=NULL,
    customDataArgs=NULL,
    customStatArgs=NULL,
    heatMapArgs=NULL,
    redDimMax=5,
    colDataMax=5,
    featAssayMax=5,
    rowStatMax=5,
    rowDataMax=5,
    sampAssayMax=5,
    colStatMax=5,
    customDataMax=5,
    customStatMax=5,
    heatMapMax=5,
    initialPanels=NULL,
    annotFun=NULL,
    customDataFun=NULL,
    customStatFun=NULL,
    customSendAll=FALSE,
    colormap=ExperimentColorMap(),
    tour=NULL,
    appTitle=NULL,
    runLocal=TRUE,
    voice=FALSE,
    bugs=FALSE) {

    # Save the original name of the input object for renaming in the tracker
    if (has_se <- !missing(se)) {
        se_name <- deparse(substitute(se))
    } else {
        se_name <- "se"
    }
    ecm_name <- deparse(substitute(colormap))
    cdf_name <- deparse(substitute(customDataFun))
    csf_name <- deparse(substitute(customStatFun))

    #######################################################################
    ## UI definition. ----
    #######################################################################

    iSEE_ui <- dashboardPage(
        dashboardHeader(
            title = ifelse(is.null(appTitle),
                   paste0("iSEE - interactive SummarizedExperiment Explorer v", packageVersion("iSEE")),
                   appTitle),
            titleWidth = 750,
            dropdownMenu(type = "tasks",
                icon = icon("object-group fa-1g"),
                badgeStatus = NULL,
                headerText = "Organization",
                notificationItem(
                    text = actionButton(
                        'organize_panels', label="Organize panels",
                        icon = icon("object-ungroup"),
                        style=.actionbutton_biocstyle
                    ),
                    icon = icon(""), status = "primary"
                )
            ),

            dropdownMenu(type = "tasks",
                icon = icon("wrench fa-1g"),
                badgeStatus = NULL,
                headerText = "Diagnostics",
                notificationItem(
                    text=actionButton(
                        'open_linkgraph', label="Examine panel chart",
                        icon=icon("chain"),
                        style=.actionbutton_biocstyle
                    ),
                    icon=icon(""), status="primary"
                ),
                notificationItem(
                    text=actionButton(
                        'getcode_all', label="Extract the R code",
                        icon=icon("magic"),
                        style=.actionbutton_biocstyle
                    ),
                    icon=icon(""), status="primary"
                ),
                notificationItem(
                    text=actionButton(
                        'get_panel_settings', label="Display panel settings",
                         icon=icon("clipboard"),
                         style=.actionbutton_biocstyle
                    ),
                    icon=icon(""), status="primary"
                )
            ), # end of dropdownMenu

            dropdownMenu(type="tasks",
                icon=icon("question-circle fa-1g"),
                badgeStatus=NULL,
                headerText="Documentation",
                notificationItem(
                    text=actionButton(
                        "tour_firststeps", "Click me for a quick tour",
                        icon("hand-o-right"),
                        style=.actionbutton_biocstyle
                    ),
                    icon=icon(""), # tricking it to not have additional icon
                    status="primary"
                ),
                notificationItem(
                    text=actionButton(
                        'open_vignette', label="Open the vignette",
                        icon=icon("book"),
                        style=.actionbutton_biocstyle,
                        onclick=ifelse(runLocal, "",
                            # Use web vignette, with varying paths depending on whether we're release or devel.
                            sprintf("window.open('http://bioconductor.org/packages/%s/bioc/vignettes/iSEE/inst/doc/basic.html', '_blank')",
                                ifelse(unlist(packageVersion("iSEE"))[2] %% 2L==0L, "release", "devel")
                            )
                        )
                    ),
                    icon=icon(""), status="primary"
                )
            ),

            dropdownMenu(type="tasks",
                icon=icon("info fa-1g"),
                badgeStatus=NULL,
                headerText="Additional information",
                notificationItem(
                    text=actionButton(
                        'session_info', label="About this session",
                        icon=icon("window-maximize"),
                        style=.actionbutton_biocstyle
                    ),
                    icon=icon(""), status="primary"
                ),
                notificationItem(
                    text=actionButton('iSEE_info', label="About iSEE",
                        icon=icon("heart"),
                        style=.actionbutton_biocstyle
                    ),
                    icon=icon(""), status="primary"
                )
            ) # end of dropdownMenu
        ), # end of dashboardHeader

        dashboardSidebar(disable=TRUE),

        dashboardBody(
            includeCSS(system.file(package="iSEE", "www", "iSEE.css")),
            useShinyjs(),
            prepareSpeechRecognition(voice),
            .prepareBugsEasterEgg(bugs),
            introjsUI(), # must be included in UI

            # for error message handling
            tags$head(
                tags$style(HTML(paste("
.shiny-output-error-validation {
    font-size: 15px;
    color: forestgreen;
    text-align: center;
}
", .define_box_statuses))
                )
            ),

            uiOutput("allPanels")
        ), # end of dashboardBody
        skin="black"
    ) # end of dashboardPage

    #######################################################################
    ## Server definition. ----
    #######################################################################

    #nocov start
    iSEE_server <- function(input, output, session) {
        rObjects <- reactiveValues(rerendered=1L)

        initialize_server <- function(se, rObjects) {
            if (grepl("[[:digit:]]+-12-06", Sys.Date())) {
                showNotification(ui=HTML(paste0(
                    "<p style='font-size:500%; text-align:center;'>&#x1F382;</p>",
                    "<p style='font-size:200%; text-align:center;'>Happy Birthday <code>iSEE</code>!</p>", collapse = "")),
                    type="default", duration = NULL)
            }
            
            se_out <- .sanitize_SE_input(se)
            se <- se_out$object
            se_cmds <- se_out$cmds

            # Precomputing UI information - must be before .setup_memory()
            se <- .precompute_UI_info(se, customDataFun, customStatFun)

            # Throw an error if the colormap supplied is not compatible with the object

            errors <- checkColormapCompatibility(colormap, se)

            if (!is.null(errors)){
                colormap <- ExperimentColorMap()
                # Show unknown number of errors first, as they may be pushed out of screen
                for (i in seq_along(errors)) {
                    ui_msg <- tagList(strong("Compatibility error:"), errors[i], ".")
                    showNotification(ui=ui_msg, type="error", duration=10)
                }
                # Show overall warning last, so that it is visible at the bottom of the screen
                ui_msg <- tagList(
                    strong("Invalid colormap:"), br(),
                    "Reverting to default", code("ExperimentColorMap()"), "."
                )
                showNotification(ui=ui_msg, type="warning", duration=10)
            }

            # Defining the maximum number of plots.
            memory <- .setup_memory(se,
                redDimArgs, colDataArgs, featAssayArgs, rowStatArgs, rowDataArgs,
                sampAssayArgs, colStatArgs, customDataArgs, customStatArgs, heatMapArgs,
                redDimMax, colDataMax, featAssayMax, rowStatMax, rowDataMax,
                sampAssayMax, colStatMax, customDataMax, customStatMax, heatMapMax)

            # Defining the initial elements to be plotted.
            active_panels <- .setup_initial(initialPanels, memory)
            memory <- .sanitize_memory(active_panels, memory)

            all_names <- list()
            for (mode in all_panel_types) {
                max_plots <- nrow(memory[[mode]])
                all_names[[mode]] <- sprintf("%s%i", mode, seq_len(max_plots))
            }
            all_names <- unlist(all_names)
            empty_list <- vector("list", length(all_names))
            names(empty_list) <- all_names

            # Storage for persistent non-reactive objects.
            pObjects <- new.env()
            pObjects$memory <- memory
            pObjects$commands <- empty_list

            pObjects$coordinates <- empty_list
            pObjects$selection_links <- .spawn_selection_chart(memory)
            pObjects$table_links <- .spawn_table_links(memory)
            pObjects$cached_info <- empty_list
            pObjects[[.voiceActivePanel]] <- NA_character_

            # Generating the reactive objects, used to coordinate
            # behaviour across observers.
            rObjects$active_panels <- active_panels

            for (mode in all_panel_types) {
                max_plots <- nrow(pObjects$memory[[mode]])
                for (id in seq_len(max_plots)) {
                    # Reactive to trigger replotting.
                    rObjects[[paste0(mode, id)]] <- 1L

                    # Reactive to regenerate information panels.
                    rObjects[[paste0(mode, id, "_", .panelLinkInfo)]] <- 1L
                    rObjects[[paste0(mode, id, "_", .panelGeneralInfo)]] <- 1L

                    # Reactive to regenerate multi-selection selectize.
                    rObjects[[paste0(mode, id, "_", .selectMultiSaved)]] <- 1L

                    # Reactive to regenerate children when the point population of the current panel changes.
                    rObjects[[paste0(mode, id, "_repopulated")]] <- 1L

                    # Reactive to regenerate children when the active selection of the current panel changes.
                    rObjects[[paste0(mode, id, "_reactivated")]] <- 1L

                    # Reactive to regenerate children when the saved selection of the current panel changes.
                    rObjects[[paste0(mode, id, "_resaved")]] <- 1L
                }
            }

            mode <- "heatMapPlot"
            max_plots <- nrow(pObjects$memory[[mode]])
            for (id in seq_len(max_plots)) {
                rObjects[[paste0(mode, id, "_", .heatMapLegend)]] <- 1L
            }

            # Evaluating certain plots to fill the coordinate list, if there are any selections.
            # This is done in topological order so that all dependencies are satisfied.
            eval_order <- .establish_eval_order(pObjects$selection_links)
            for (panelname in eval_order) {
                enc <- .split_encoded(panelname)
                FUN <- switch(enc$Type,
                    redDimPlot=.make_redDimPlot,
                    featAssayPlot=.make_featAssayPlot,
                    colDataPlot=.make_colDataPlot,
                    rowDataPlot=.make_rowDataPlot,
                    sampAssayPlot=.make_sampAssayPlot)
                p.out <- FUN(enc$ID, pObjects$memory, pObjects$coordinates, se, colormap)
                pObjects$coordinates[[panelname]] <- p.out$xy[, intersect(.allCoordinatesNames, colnames(p.out$xy))]
            }

            # Observer set-up.
            .general_observers(input, session, pObjects, rObjects, tour, runLocal,
                 se_name, ecm_name, cdf_name, csf_name, se_cmds)

            .organization_observers(input, output, se, pObjects, rObjects)

            .box_observers(input, pObjects)

            .selection_parameter_observers(input, session, pObjects, rObjects)

            .brush_observers(input, session, pObjects, rObjects)

            .child_propagation_observers(pObjects, rObjects, customSendAll)

            .multiselect_param_observers(input, session, pObjects, rObjects)

            .lasso_observers(input, session, pObjects, rObjects)

            .zoom_observers(input, session, pObjects, rObjects)

            .selectize_update_observers(input, session, se, pObjects, rObjects)

            .multiple_select_observers(input, session, pObjects, rObjects)

            .dot_plot_observers(input, output, session, se, colormap, pObjects, rObjects)

            .assay_plot_observers(input, session, se, pObjects, rObjects)

            .reddim_plot_observers(input, session, se, pObjects, rObjects)

            .custom_panel_observers(input, output, session, se, pObjects, rObjects, customSendAll)

            .linked_table_observers(input, output, session, se, pObjects, rObjects, annotFun)

            .voice_control_observers(input, session, se, pObjects, rObjects)

            .heatmap_observers(input, output, session, se, colormap, pObjects, rObjects)
        }

        if (!has_se) {
            output$allPanels <- renderUI({
                fileInput("new_se", "Choose RDS File", multiple = FALSE)
            })

            observeEvent(input$new_se, {
                se2 <- try(readRDS(input$new_se$datapath), silent=TRUE)
                if (is(se2, "try-error")) {
                    showNotification("must upload a valid RDS file", type="error")
                } else if (!is(se2, "SummarizedExperiment")) {
                    showNotification("must upload a SummarizedExperiment object", type="error")
                } else {
                    initialize_server(se2, rObjects)
                    rObjects$rerendered <- .increment_counter(isolate(rObjects$rerendered))
                }
            }, ignoreNULL=TRUE, once=TRUE)
        } else {
            initialize_server(se, rObjects)
        }
    } # end of iSEE_server
    #nocov end

    #######################################################################
    # Launching the app.
    #######################################################################

    shinyApp(ui=iSEE_ui, server=iSEE_server)
}
