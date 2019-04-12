#' iSEE: interactive SummarizedExperiment/SingleCellExperiment Explorer
#'
#' Interactive and reproducible visualization of data contained in a
#' SummarizedExperiment/SingleCellExperiment, using a Shiny interface.
#'
#' @param se An object that is coercible to \linkS4class{SingleCellExperiment}.
#' @param redDimArgs A DataFrame similar to that produced by \code{\link{redDimPlotDefaults}}, specifying initial parameters for the reduced dimension plots.
#' @param colDataArgs A DataFrame similar to that produced by \code{\link{colDataPlotDefaults}}, specifying initial parameters for the column data plots.
#' @param featAssayArgs A DataFrame similar to that produced by \code{\link{featAssayPlotDefaults}}, specifying initial parameters for the feature assay plots.
#' @param rowStatArgs A DataFrame similar to that produced by \code{\link{rowStatTableDefaults}}, specifying initial parameters for the row statistics tables.
#' @param rowDataArgs A DataFrame similar to that produced by \code{\link{rowDataPlotDefaults}}, specifying initial parameters for the row data plots.
#' @param sampAssayArgs A DataFrame similar to that produced by \code{\link{sampAssayPlotDefaults}}, specifying initial parameters for the sample assay plots.
#' @param colStatArgs A DataFrame similar to that produced by \code{\link{colStatTableDefaults}}, specifying initial parameters for the sample assay plots.
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
#' @return A Shiny app object is returned, for interactive data exploration of the \linkS4class{SummarizedExperiment} or \linkS4class{SingleCellExperiment} object.
#'
#' @export
#' @importFrom utils packageVersion
#' @importFrom shinydashboard dashboardBody dashboardHeader dashboardPage dashboardSidebar menuItem tabBox valueBox valueBoxOutput dropdownMenu notificationItem
#' @importFrom utils packageVersion read.delim citation sessionInfo browseURL head
#' @importFrom shinyjs useShinyjs
#' @importFrom rintrojs introjsUI introjs
#' @importFrom shiny plotOutput uiOutput
#' renderUI renderPlot renderPrint
#' observe observeEvent reactiveValues isolate req
#' actionButton selectizeInput
#' showModal modalDialog showNotification removeNotification
#' shinyApp runApp
#' HTML br icon hr p em strong img
#' tagList tags
#' tabsetPanel tabPanel
#' updateSelectInput updateSelectizeInput updateRadioButtons
#' includeCSS singleton includeScript
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom shinyAce aceEditor
#' @importFrom S4Vectors DataFrame
#' @importFrom methods as
#' @importFrom cowplot plot_grid
#'
#' @examples
#' library(scRNAseq)
#' data(allen)
#' class(allen)
#'
#' # Example data ----
#'
#' library(scater)
#' sce <- as(allen, "SingleCellExperiment")
#' counts(sce) <- assay(sce, "tophat_counts")
#' sce <- normalize(sce)
#'
#' sce <- runPCA(sce, ncomponents=4)
#' sce <- runTSNE(sce)
#' rowData(sce)$ave_count <- rowMeans(counts(sce))
#' rowData(sce)$n_cells <- rowSums(counts(sce)>0)
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
    voice=FALSE) {

    # Save the original name of the input object for renaming in the tracker
    se_name <- deparse(substitute(se))
    ecm_name <- deparse(substitute(colormap))
    cdf_name <- deparse(substitute(customDataFun))
    csf_name <- deparse(substitute(customStatFun))

    se_out <- .sanitize_SE_input(se)
    se <- se_out$object
    se_cmds <- se_out$cmds

    # Precomputing UI information - must be before .setup_memory()
    se <- .precompute_UI_info(se, customDataFun, customStatFun)

    # Throw an error if the colormap supplied is not compatible with the object
    isColorMapCompatible(colormap, se, error=TRUE)

    # Setting up inputs for DT::datatable something to play with.
    feature_data <- data.frame(rowData(se), check.names=FALSE)
    rownames(feature_data) <- rownames(se)
    if (ncol(feature_data)==0L) {
        feature_data$Present <- !logical(nrow(feature_data))
    }
    feature_data_select_col <- .safe_field_name("Selected", colnames(feature_data))

    sample_data <- data.frame(colData(se), check.names=FALSE)
    rownames(sample_data) <- colnames(se)
    if (ncol(sample_data)==0L) {
        sample_data$Present <- !logical(nrow(sample_data))
    }
    sample_data_select_col <- .safe_field_name("Selected", colnames(sample_data))

    # Defining the maximum number of plots.
    memory <- .setup_memory(se,
        redDimArgs, colDataArgs, featAssayArgs, rowStatArgs, rowDataArgs, sampAssayArgs, colStatArgs, customDataArgs, customStatArgs, heatMapArgs,
        redDimMax, colDataMax, featAssayMax, rowStatMax, rowDataMax, sampAssayMax, colStatMax, customDataMax, customStatMax, heatMapMax)

    # Defining the initial elements to be plotted.
    active_panels <- .setup_initial(initialPanels, memory)
    memory <- .sanitize_memory(active_panels, memory)

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

        # Storage for all the reactive objects
        rObjects <- reactiveValues(
            active_panels=active_panels,
            rerendered=1L
        )

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

        #######################################################################
        # General observers. ----
        #######################################################################

        observeEvent(input$tour_firststeps, {
            if(is.null(tour)) {
                tour <- read.delim(system.file("extdata", "intro_firststeps.txt", package="iSEE"),
                    sep=";", stringsAsFactors=FALSE, row.names=NULL, quote="")
            }
            introjs(session, options=list(steps=tour))
        })

        if (!is.null(tour)) {
            # Only triggers _after_ panels are fully setup, so observers are properly ID'd.
            session$onFlushed(function() { introjs(session, options=list(steps=tour)) })
        }

        observeEvent(input$getcode_all, {
            spawn_editor <- function(editor_name, select_only) {
                aceEditor(editor_name, mode="r",theme="solarized_light", autoComplete="live",
                    value=paste0(.track_it_all(rObjects$active_panels, pObjects,
                        se_name, ecm_name, cdf_name, csf_name, se_cmds), collapse="\n"),
                    height="600px")
            }
            showModal(modalDialog(
                title="My code", size="l",fade=TRUE,
                footer=NULL, easyClose=TRUE,
                p("You can click anywhere in the code editor and select all the code using",
                  "a keyboard shortcut that depends on your operating system (e.g. Ctrl/Cmd + A",
                  "followed by Ctrl/Cmd + C).",
                  "This will copy the selected parts to the clipboard."),
                aceEditor("report_all_cmds", mode="r", theme="solarized_light", autoComplete="live",
                    value=paste0(.track_it_all(rObjects$active_panels, pObjects,
                            se_name, ecm_name, cdf_name, csf_name, se_cmds), collapse="\n"),
                    height="600px")
            ))
        })

        observeEvent(input$get_panel_settings, {
            showModal(modalDialog(
                title="Panel settings", size="l", fade=TRUE,
                footer=NULL, easyClose=TRUE,
                aceEditor("acereport_r", mode="r", theme="solarized_light", autoComplete="live",
                    value=paste0(.report_memory(rObjects$active_panels, pObjects$memory), collapse="\n"),
                    height="600px")
            ))
        })

        observeEvent(input$session_info, {
            showModal(modalDialog(
                title="Session information", size="l",fade=TRUE,
                footer=NULL, easyClose=TRUE,
                tagList(renderPrint({
                    sessionInfo()
                }))
            ))
        })

        observeEvent(input$iSEE_info, {
            showModal(modalDialog(
                title="About iSEE", size="m", fade=TRUE,
                footer=NULL, easyClose=TRUE,
                tagList(
                    iSEE_info, br(), br(),
                    HTML("If you use this package, please use the following citation information:"),
                    renderPrint({
                        citation("iSEE")
                    })
                )
            ))
        })

        observeEvent(input$open_linkgraph, {
            showModal(modalDialog(
                title="Graph of inter-panel links", size="l",
                fade=TRUE, footer=NULL, easyClose=TRUE,
                renderPlot({
                    .snapshot_graph_linkedpanels(rObjects$active_panels, pObjects)
                })
            ))
        })

        if (runLocal) {
            observeEvent(input$open_vignette, {
                path <- system.file("doc", "basic.html", package="iSEE")
                if (path=="") {
                    showNotification("vignette has not been built on this system", type="error")
                } else {
                    browseURL(path)
                }
            })
        }

        #######################################################################
        # Multipanel UI generation section. ----
        # This is adapted from https://stackoverflow.com/questions/15875786/dynamically-add-plots-to-web-page-using-shiny.
        #######################################################################

        output$allPanels <- renderUI({
            rObjects$rerendered <- .increment_counter(isolate(rObjects$rerendered))
            .panel_generation(rObjects$active_panels, pObjects$memory, se)
        })

        n_available <- vapply(pObjects$memory, nrow, FUN.VALUE=0L)
        enc_ids <- unlist(lapply(n_available, FUN=seq_len))
        enc_names <- rep(names(pObjects$memory), n_available)
        available_panels <- paste0(enc_names, enc_ids)
        names(available_panels) <- .decode_panel_name(enc_names, enc_ids)
        
        # Panel ordering, addition and deletion.
        observeEvent(input$organize_panels, {
            active_panels <- paste0(rObjects$active_panels$Type, rObjects$active_panels$ID)
            names(active_panels) <- .decode_panel_name(rObjects$active_panels$Type, rObjects$active_panels$ID)
            inactive_panels <- available_panels[which(!available_panels %in% active_panels)]
            ordered_panel_choices <- c(active_panels, inactive_panels)
            showModal(modalDialog(
                title = "Panel organization", size = "m", fade = TRUE,
                footer = NULL, easyClose = TRUE,
                actionButton("update_ui", "Apply settings", icon=icon("object-ungroup"), width = '100%'),
                hr(),
                selectizeInput("panel_order", label=NULL, choices=ordered_panel_choices, multiple=TRUE,
                    selected=active_panels,
                    options=list(plugins=list('remove_button', 'drag_drop')), width="500px"),
                uiOutput("panelParams")
            ))
        })

        output$panelParams <- renderUI({
            .panel_organization(rObjects$active_panels)
        })
        
        # Height and width are both under the control of the action button
        # Note that panel order is not (see above)
        observeEvent(input$update_ui, {
            ### Update panel width/height ###
            # Note that this must happen _before_ adding new active panels, as those don't have height/width inputs yet!
            for (mode in all_panel_types) {
                max_plots <- nrow(pObjects$memory[[mode]])
                for (id in seq_len(max_plots)) {
                    # Note: we need "local" so that each item gets its own number. Without it, the value
                    # of 'id' in the renderPlot() will be the same across all instances, because
                    # of when the expression is evaluated.
                    local({
                        mode0 <- mode
                        id0 <- id
                        prefix <- paste0(mode0, id0, "_")
                        max_plots0 <- max_plots
                        
                        # Panel width
                        width_name <- paste0(prefix, .organizationWidth)
                        all_active <- rObjects$active_panels
                        index <- which(all_active$Type==mode0 & all_active$ID==id0)
                        cur.width <- all_active$Width[index]
                        new.width <- as.integer(input[[width_name]])
                        if (!isTRUE(all.equal(new.width, cur.width))) {
                            rObjects$active_panels$Width[index] <- new.width
                        }
                        
                        # Panel height
                        height_name <- paste0(prefix, .organizationHeight)
                        all_active <- rObjects$active_panels
                        index <- which(all_active$Type==mode0 & all_active$ID==id0)
                        cur.height <- all_active$Height[index]
                        new.height <- input[[height_name]]
                        if (!isTRUE(all.equal(new.height, cur.height))) {
                            rObjects$active_panels$Height[index] <- new.height
                        }
                    })
                }
            }
            
            ### Reorder/add/remove panels ###
            cur_active <- paste0(rObjects$active_panels$Type, rObjects$active_panels$ID)
            if (identical(input$panel_order, cur_active)) {
                return(NULL)
            }
            
            m <- match(input$panel_order, cur_active)
            new_active_panels <- rObjects$active_panels[m,,drop=FALSE]
            
            to_add <- is.na(m)
            if (any(to_add)) {
                enc_add <- .split_encoded(input$panel_order[to_add])
                new_active_panels[to_add,] <- data.frame(Type=enc_add$Type, ID=enc_add$ID, Width=4, Height=500L, stringsAsFactors=FALSE)
            }
            
            rObjects$active_panels <- new_active_panels
        })

        #######################################################################
        # Parameter panel observers.
        #######################################################################

        for (mode in all_panel_types) {
            if (mode %in% point_plot_types) {
                box_types <- c(.dataParamBoxOpen, .visualParamBoxOpen, .selectParamBoxOpen)
            } else if (mode=="heatMapPlot") {
                box_types <- c(.heatMapFeatNameBoxOpen, .heatMapColDataBoxOpen, .selectParamBoxOpen)
            } else if (mode %in% custom_panel_types) {
                box_types <- c(.dataParamBoxOpen, .selectParamBoxOpen)
            } else if (mode %in% linked_table_types) {
                box_types <- .selectParamBoxOpen
            } else {
                box_types <- character(0)
            }

            max_plots <- nrow(pObjects$memory[[mode]])
            for (id in seq_len(max_plots)) {
                for (boxtype in box_types) {
                    local({
                        mode0 <- mode
                        id0 <- id
                        box0 <- boxtype

                        open_field <- paste0(mode0, id0, "_", box0)
                        observeEvent(input[[open_field]], {
                            pObjects$memory[[mode0]][[box0]][id0] <- input[[open_field]]
                        })
                    })
                }
            }
        }

        #######################################################################
        # Point selection observers.
        #######################################################################

        # Selection choice observer; applicable to all non-custom panels.
        for (mode in c(point_plot_types, linked_table_types, "heatMapPlot")) {
            max_panels <- nrow(pObjects$memory[[mode]])

            for (id in seq_len(max_panels)) {
                local({
                    mode0 <- mode
                    id0 <- id
                    panel_name <- paste0(mode0, id0)
                    select_panel_field <- paste0(panel_name, "_", .selectByPlot)
                    repop_field <- paste0(panel_name, "_repopulated")
                    can_transmit <- mode %in% point_plot_types

                    observeEvent(input[[select_panel_field]], {
                        old_transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                        new_transmitter <- input[[select_panel_field]]
                        if (old_transmitter==new_transmitter) {
                            return(NULL)
                        }

                        old_encoded <- old_transmitter
                        if (old_transmitter!=.noSelection) {
                            old_encoded <- .decoded2encoded(old_transmitter)
                        }
                        new_encoded <- new_transmitter
                        if (new_transmitter!=.noSelection) {
                            new_encoded <- .decoded2encoded(new_transmitter)
                        }
                        tmp <- .choose_new_selection_source(pObjects$selection_links, panel_name, new_encoded, old_encoded)

                        # Trying to update the graph, but breaking if it's not a DAG.
                        # We also break if users try to self-select in restrict mode.
                        # These concerns are only relevant for transmitting panels (i.e., point plots).
                        if (can_transmit) {
                            daggy <- is_dag(simplify(tmp, remove.loops=TRUE))
                            self_restrict <- new_encoded==panel_name &&
                                new_encoded!=.noSelection &&
                                pObjects$memory[[mode0]][id0, .selectEffect]==.selectRestrictTitle

                            if (!daggy || self_restrict) {
                                if (!daggy) {
                                    showNotification("point selection relationships cannot be cyclic", type="error")
                                } else if (self_restrict){
                                    showNotification("selecting to self is not compatible with 'Restrict'", type="error")
                                }
                                updateSelectInput(session, select_panel_field, selected=old_transmitter)
                                return(NULL)
                            }
                        }

                        pObjects$selection_links <- tmp
                        pObjects$memory[[mode0]][id0, .selectByPlot] <- new_transmitter

                        # Update the elements reporting the links between panels.
                        for (relinked in setdiff(c(old_encoded, new_encoded, panel_name), .noSelection)) {
                            relink_field <- paste0(relinked, "_", .panelLinkInfo)
                            rObjects[[relink_field]] <- .increment_counter(isolate(rObjects[[relink_field]]))
                        }

                        # Update the multi-selection selectize.
                        saved_select_name <- paste0(panel_name, "_", .selectMultiSaved)
                        rObjects[[saved_select_name]] <- .increment_counter(isolate(rObjects[[saved_select_name]]))

                        saved_val <- pObjects$memory[[mode0]][id0, .selectMultiSaved]
                        if (saved_val!=0L && new_encoded!=.noSelection) {
                            new_enc <- .split_encoded(new_encoded)
                            if (saved_val > length(pObjects$memory[[new_enc$Type]][,.multiSelectHistory][[new_enc$ID]])) {
                                pObjects$memory[[mode0]][id0, .selectMultiSaved] <- 0L
                            }
                        }

                        # Checking if there were active/saved selections in either the new or old transmitters.
                        no_old_selection <- !.transmitted_selection(old_encoded, pObjects$memory, mode=mode0, id=id0)
                        no_new_selection <- !.transmitted_selection(new_encoded, pObjects$memory, mode=mode0, id=id0)
                        if (no_old_selection && no_new_selection) {
                            return(NULL)
                        }

                        rObjects[[panel_name]] <- .increment_counter(isolate(rObjects[[panel_name]]))

                        # Updating children, if the current panel is set to restrict
                        # (and thus the point population changes with a new transmitted selection).
                        if (can_transmit && pObjects$memory[[mode0]][id0, .selectEffect]==.selectRestrictTitle) {
                            rObjects[[repop_field]] <- .increment_counter(isolate(rObjects[[repop_field]]))
                        }
                    }, ignoreInit=TRUE)
                })
            }
        }

        # Selection effect observer.
        for (mode in c(point_plot_types, "heatMapPlot")) {
            max_plots <- nrow(pObjects$memory[[mode]])

            for (id in seq_len(max_plots)) {
                local({
                    mode0 <- mode
                    id0 <- id
                    panel_name <- paste0(mode0, id0)
                    select_effect_field <- paste0(panel_name, "_", .selectEffect)
                    repop_field <- paste0(panel_name, "_repopulated")
                    can_transmit <- mode %in% point_plot_types

                    observeEvent(input[[select_effect_field]], {
                        cur_effect <- input[[select_effect_field]]
                        old_effect <- pObjects$memory[[mode0]][id0, .selectEffect]

                        # Storing the new choice into memory, unless self-selecting to restrict.
                        # In which case, we trigger an error and reset to the previous choice.
                        if (can_transmit) {
                            if (cur_effect == .selectRestrictTitle
                                    && pObjects$memory[[mode0]][id0, .selectByPlot]==.decode_panel_name(mode0, id0)) {
                                showNotification("selecting to self is not compatible with 'Restrict'", type="error")
                                updateRadioButtons(session, select_effect_field, selected=old_effect)
                                return(NULL)
                            }
                        }
                        pObjects$memory[[mode0]][id0, .selectEffect] <- cur_effect

                        # Avoiding replotting if there was no transmitting selection.
                        transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                        if (!.transmitted_selection(transmitter, pObjects$memory, mode=mode0, id=id0, encoded=FALSE)) {
                            return(NULL)
                        }

                        rObjects[[panel_name]] <- .increment_counter(isolate(rObjects[[panel_name]]))

                        # Updating children if the selection in the current plot changes due to gain/loss of Restrict.
                        if (cur_effect==.selectRestrictTitle || old_effect==.selectRestrictTitle) {
                            rObjects[[repop_field]] <- .increment_counter(isolate(rObjects[[repop_field]]))
                        }
                    }, ignoreInit=TRUE)
                })
            }
        }

        # Shiny brush structure observers. There are three "phases" of a Shiny brush:
        # - the Javascript (JS) brush, which is what the user draws and the observer responds to.
        #   This is eliminated upon replotting for various consistency reasons.
        # - the active brush, which is what is stored in iSEE's ".brushData" field.
        # - the saved brush(es), stored in iSEE's ".multiSelectHistory" field.
        for (mode in point_plot_types) {
            max_plots <- nrow(pObjects$memory[[mode]])
            for (id in seq_len(max_plots)) {
                local({
                    mode0 <- mode
                    id0 <- id
                    plot_name <- paste0(mode0, id0)
                    act_field <- paste0(mode0, id0, "_reactivated")
                    save_field <- paste0(plot_name, "_", .multiSelectSave)

                    brush_id <- paste0(plot_name, "_", .brushField)
                    observeEvent(input[[brush_id]], {
                        cur_brush <- input[[brush_id]]
                        old_brush <- pObjects$memory[[mode0]][,.brushData][[id0]]
                        pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .brushData, cur_brush)

                        # If the Shiny brushes have the same coordinates, we don't bother replotting.
                        replot <- !.identical_brushes(cur_brush, old_brush)

                        # Destroying lasso points upon Shiny brush (replotting if existing lasso was not NULL).
                        replot <- replot || !is.null(pObjects$memory[[mode0]][,.lassoData][[id0]])
                        pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .lassoData, NULL)
                        if (!replot) {
                            return(NULL)
                        }

                        .disableButtonIf(
                            save_field,
                            !.any_active_selection(mode0, id0, pObjects$memory),
                            .buttonNoSelectionLabel, .buttonSaveLabel, session
                        )

                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                        rObjects[[act_field]] <- .increment_counter(isolate(rObjects[[act_field]]))
                    }, ignoreInit=TRUE)
                })
            }
        }

        #######################################################################
        # Child propagating observers.
        #######################################################################

        # Observers to decide whether children need to be replotted.
        for (mode in point_plot_types) {
            max_panels <- nrow(pObjects$memory[[mode]])
            for (id in seq_len(max_panels)) {
                local({
                    mode0 <- mode
                    id0 <- id
                    plot_name <- paste0(mode0, id0)
                    repop_field <- paste0(plot_name, "_repopulated")

                    ## Observer for changes in the point population in the current panel. ---
                    # This refers to changes in the points retained by restriction of selections from upstream transmitters.
                    observe({
                        force(rObjects[[repop_field]])
                        has_active <- .any_active_selection(mode0, id0, pObjects$memory)
                        has_saved <- .any_saved_selection(mode0, id0, pObjects$memory)

                        children <- .get_direct_children(pObjects$selection_links, plot_name)
                        for (child_plot in children) {
                            child_enc <- .split_encoded(child_plot)
                            child_mode <- child_enc$Type
                            child_id <- child_enc$ID

                            # We have special rules for custom panel types, as these don't have .selectMultiType or .selectMultiSaved.
                            if (child_mode %in% custom_panel_types) {
                                if (has_active || (customSendAll && has_saved)) {
                                    rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
                                }
                                next
                            }

                            # To warrant replotting of the child, there needs to be Active or Saved selections in the current panel.
                            # Any point population changes in the current panel will result in new subsets if those selections are available.
                            replot <- FALSE
                            select_mode <- pObjects$memory[[child_mode]][child_id, .selectMultiType]
                            if (select_mode==.selectMultiActiveTitle || select_mode==.selectMultiUnionTitle) {
                                if (has_active) replot <- TRUE
                            }
                            if (select_mode==.selectMultiSavedTitle && pObjects$memory[[child_mode]][child_id, .selectMultiSaved]!=0L) {
                                if (has_saved) replot <- TRUE
                            }
                            if (select_mode==.selectMultiUnionTitle) {
                                if (has_saved) replot <- TRUE
                            }

                            if (replot) {
                                rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))

                                # To warrant replotting of the grandchildren, the child must itself be restricted.
                                if (child_mode %in% point_plot_types && pObjects$memory[[child_mode]][child_id, .selectEffect]==.selectRestrictTitle) {
                                    react_child <- paste0(child_mode, child_id, "_repopulated")
                                    rObjects[[react_child]] <- .increment_counter(isolate(rObjects[[react_child]]))
                                }
                            }
                        }
                    })

                    ## Observer for changes in the active selection in the current panel. ---
                    # We can't use the 'repopulated' observer above, because the point population in the current
                    # panel hasn't changed if the active selection changes in that panel. Instead, we need to
                    # manually implement the first generation of propagation.
                    act_field <- paste0(plot_name, "_reactivated")
                    observe({
                        force(rObjects[[act_field]])

                        children <- .get_direct_children(pObjects$selection_links, plot_name)
                        for (child_plot in children) {
                            child_enc <- .split_encoded(child_plot)
                            child_mode <- child_enc$Type
                            child_id <- child_enc$ID

                            # Custom panels don't have any other settings, so we just replot and move on.
                            if (child_mode %in% custom_panel_types) {
                                rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
                                next
                            }

                            select_mode <- pObjects$memory[[child_mode]][child_id, .selectMultiType]
                            if (select_mode==.selectMultiActiveTitle || select_mode==.selectMultiUnionTitle) {
                                rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))

                                # To warrant replotting of the grandchildren, the child must itself be restricted.
                                if (child_mode %in% point_plot_types && pObjects$memory[[child_mode]][child_id, .selectEffect]==.selectRestrictTitle) {
                                    react_child <- paste0(child_mode, child_id, "_repopulated")
                                    rObjects[[react_child]] <- .increment_counter(isolate(rObjects[[react_child]]))
                                }
                            }
                        }
                    })

                    ## Observer for changes in the saved selections in the current panel. ---
                    # As described above, we can't use the 'repopulated' observer. The conditions
                    # to propagate are also slightly different to the conditions for the 'reactivated' observer.
                    save_field <- paste0(plot_name, "_resaved")
                    observe({
                        force(rObjects[[save_field]])
                        Nsaved <- length(pObjects$memory[[mode0]][,.multiSelectHistory][[id0]])

                        children <- .get_direct_children(pObjects$selection_links, plot_name)
                        for (child_plot in children) {
                            child_enc <- .split_encoded(child_plot)
                            child_mode <- child_enc$Type
                            child_id <- child_enc$ID

                            # Custom panels don't have any other settings, so we just replot and move on.
                            if (child_mode %in% custom_panel_types) {
                                if (customSendAll) {
                                    rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))
                                }
                                next
                            }

                            reset <- pObjects$memory[[child_mode]][child_id, .selectMultiSaved] > Nsaved
                            if (reset) {
                                pObjects$memory[[child_mode]][child_id, .selectMultiSaved] <- 0L
                            }

                            child_select_type <- pObjects$memory[[child_mode]][child_id, .selectMultiType]
                            if (child_select_type==.selectMultiUnionTitle || (child_select_type==.selectMultiSavedTitle && reset)) {
                                rObjects[[child_plot]] <- .increment_counter(isolate(rObjects[[child_plot]]))

                                if (child_mode %in% point_plot_types && pObjects$memory[[child_mode]][child_id, .selectEffect]==.selectRestrictTitle) {
                                    react_child <- paste0(child_plot, "_repopulated")
                                    rObjects[[react_child]] <- .increment_counter(isolate(rObjects[[react_child]]))
                                }
                            }

                            # Updating the selectize as well.
                            child_saved <- paste0(child_plot, "_", .selectMultiSaved)
                            rObjects[[child_saved]] <- .increment_counter(isolate(rObjects[[child_saved]]))
                        }
                    })
                })
            }
        }

        #######################################################################
        # Multiple selection observers.
        #######################################################################

        for (mode in c(point_plot_types, linked_table_types, "heatMapPlot")) {
            max_panels <- nrow(pObjects$memory[[mode]])
            for (id in seq_len(max_panels)) {
                local({
                    mode0 <- mode
                    id0 <- id
                    panel_name <- paste0(mode0, id0)
                    repop_field <- paste0(mode0, id0, "_repopulated")

                    ## Type field observers. ---
                    type_field <- paste0(mode0, id0, "_", .selectMultiType)
                    observeEvent(input[[type_field]], {
                        old_type <- pObjects$memory[[mode0]][[.selectMultiType]][id0]
                        new_type <- as(input[[type_field]], typeof(old_type))
                        if (identical(new_type, old_type)) {
                            return(NULL)
                        }
                        pObjects$memory[[mode0]][[.selectMultiType]][id0] <- new_type

                        # Skipping if neither the old or new types were relevant.
                        transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                        no_old_selection <- !.transmitted_selection(transmitter, pObjects$memory, select_type=old_type, mode=mode0, id=id0, encoded=FALSE)
                        no_new_selection <- !.transmitted_selection(transmitter, pObjects$memory, mode=mode0, id=id0, encoded=FALSE)
                        if (no_old_selection && no_new_selection) {
                            return(NULL)
                        }

                        rObjects[[panel_name]] <- .increment_counter(isolate(rObjects[[panel_name]]))
                        if (pObjects$memory[[mode0]][id0, .selectEffect]==.selectRestrictTitle) {
                            rObjects[[repop_field]] <- .increment_counter(isolate(rObjects[[repop_field]]))
                        }
                    }, ignoreInit=TRUE)

                    ## Saved field observers. ---
                    saved_select <- paste0(panel_name, "_", .selectMultiSaved)
                    observeEvent(input[[saved_select]], {
                        req(input[[saved_select]]) # Required to defend against empty strings before updateSelectizeInput runs.
                        matched_input <- as(input[[saved_select]], typeof(pObjects$memory[[mode0]][[.selectMultiSaved]]))
                        if (identical(matched_input, pObjects$memory[[mode0]][[.selectMultiSaved]][id0])) {
                            return(NULL)
                        }
                        pObjects$memory[[mode0]][[.selectMultiSaved]][id0] <- matched_input

                        transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                        if (transmitter==.noSelection) {
                            return(NULL)
                        }

                        # Switch of 'Saved' will ALWAYS change the current plot, so no need for other checks.
                        rObjects[[panel_name]] <- .increment_counter(isolate(rObjects[[panel_name]]))
                        if (pObjects$memory[[mode0]][id0, .selectEffect]==.selectRestrictTitle) {
                            rObjects[[repop_field]] <- .increment_counter(isolate(rObjects[[repop_field]]))
                        }
                    }, ignoreInit=TRUE)

                    ## Selectize observer. ---
                    # Do NOT be tempted to centralize code by setting .selectMultiSaved to zero here.
                    # This needs to be done in upstream observers, otherwise there is no guarantee
                    # that the zero is set before plotting observers that use it.
                    observe({
                        force(rObjects[[saved_select]])
                        force(rObjects$rerendered)

                        transmitter <- pObjects$memory[[mode0]][id0,.selectByPlot]
                        if (transmitter==.noSelection) {
                            available_choices <- integer(0)
                        } else {
                            trans <- .encode_panel_name(transmitter)
                            N <- length(pObjects$memory[[trans$Type]][,.multiSelectHistory][[trans$ID]])
                            available_choices <- seq_len(N)
                            names(available_choices) <- available_choices
                        }

                        no_choice <- 0L
                        names(no_choice) <- .noSelection
                        available_choices <- c(no_choice, available_choices)
                        updateSelectizeInput(session, saved_select, choices=available_choices, server=TRUE,
                            selected=pObjects$memory[[mode0]][id0, .selectMultiSaved])
                    })
                })
            }
        }

        #######################################################################
        # Click observers.
        #######################################################################

        for (mode in point_plot_types) {
            max_plots <- nrow(pObjects$memory[[mode]])
            for (id in seq_len(max_plots)) {
                local({
                    mode0 <- mode
                    id0 <- id
                    plot_name <- paste0(mode0, id0)
                    click_field <- paste0(plot_name, "_", .lassoClick)
                    brush_field <- paste0(plot_name, "_", .brushField)
                    act_field <- paste0(plot_name, "_reactivated")
                    save_field <- paste0(plot_name, "_", .multiSelectSave)

                    observeEvent(input[[click_field]], {
                        # Hack to resolve https://github.com/rstudio/shiny/issues/947.
                        # By luck, the triggering of the click field seems to be delayed enough
                        # that input data is sent to the brush field first. Thus, we can
                        # check the brush field for a non-NULL value avoid action if
                        # the user had brushed rather than clicked. A separate click should
                        # continue past this point, as any Shiny brush would be wiped upon
                        # replotting and thus would not have any value in the input.
                        if (!is.null(input[[brush_field]])) {
                            return(NULL)
                        }

                        # Don't add to waypoints if a Shiny brush exists in memory, but instead, destroy the brush.
                        # Also destroy any closed lassos, or update open lassos.
                        reactivated <- FALSE
                        if (!is.null(pObjects$memory[[mode0]][,.brushData][[id0]])) {
                            pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .brushData, NULL)
                            reactivated <- TRUE
                        } else {
                            prev_lasso <- pObjects$memory[[mode0]][,.lassoData][[id0]]
                            was_closed <- if(is.null(prev_lasso)) FALSE else prev_lasso$closed

                            if (was_closed) {
                                new_lasso <- NULL
                                reactivated <- TRUE
                            } else {
                                new_lasso <- .update_lasso(input[[click_field]], prev_lasso)
                                if (new_lasso$closed) {
                                    reactivated <- TRUE
                                }
                            }

                            pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .lassoData, new_lasso)
                        }

                        .disableButtonIf(
                            save_field,
                            !.any_active_selection(mode0, id0, pObjects$memory),
                            .buttonNoSelectionLabel, .buttonSaveLabel, session
                        )

                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

                        if (reactivated) {
                            rObjects[[act_field]] <- .increment_counter(isolate(rObjects[[act_field]]))
                        }
                    })
                })
            }
        }

        #######################################################################
        # Double-click observers.
        #######################################################################

        for (mode in point_plot_types) {
            max_plots <- nrow(pObjects$memory[[mode]])
            for (id in seq_len(max_plots)) {
                local({
                    mode0 <- mode
                    id0 <- id
                    plot_name <- paste0(mode0, id0)
                    dblclick_field <- paste0(plot_name, "_", .zoomClick)
                    act_field <- paste0(plot_name, "_reactivated")
                    save_field <- paste0(plot_name, "_", .multiSelectSave)

                    observeEvent(input[[dblclick_field]], {
                        existing_brush <- pObjects$memory[[mode0]][,.brushData][[id0]]
                        existing_lasso <- pObjects$memory[[mode0]][,.lassoData][[id0]]

                        # Zooming destroys all active brushes or lassos.
                        pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .lassoData, NULL)
                        pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .brushData, NULL)

                        new_coords <- NULL
                        if (!is.null(existing_brush)) {
                            dblclick_vals <- input[[dblclick_field]]
                            if (dblclick_vals$x >= existing_brush$xmin
                                    && dblclick_vals$x <= existing_brush$xmax
                                    && dblclick_vals$y >= existing_brush$ymin
                                    && dblclick_vals$y <= existing_brush$ymax) {

                                # Panels are either NULL or not.
                                if (identical(dblclick_vals$panelvar1, existing_brush$panelvar1)
                                        && identical(dblclick_vals$panelvar2, existing_brush$panelvar2)) {
                                    new_coords <- c(xmin=existing_brush$xmin, xmax=existing_brush$xmax, ymin=existing_brush$ymin, ymax=existing_brush$ymax)
                                }
                            }
                            .disableButtonIf(
                                save_field,
                                !.any_active_selection(mode0, id0, pObjects$memory),
                                .buttonNoSelectionLabel, .buttonSaveLabel, session
                            )
                        }

                        pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .zoomData, new_coords)
                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

                        if (!is.null(existing_brush) || (!is.null(existing_lasso) && existing_lasso$closed)) {
                            rObjects[[act_field]] <- .increment_counter(isolate(rObjects[[act_field]]))
                        }
                    })
                })
            }
        }

        # Zoom observers for the heatmaps.
        max_plots <- nrow(pObjects$memory$heatMapPlot)
        for (id in seq_len(max_plots)) {
            local({
                mode0 <- "heatMapPlot"
                id0 <- id
                plot_name <- paste0(mode0, id0)
                prefix <- paste0(plot_name, "_")

                brush_id <- paste0(prefix, .brushField)
                dblclick_field <- paste0(prefix, .zoomClick)

                observeEvent(input[[dblclick_field]], {
                    brush <- input[[brush_id]]
                    if (!is.null(brush)) {
                        new_coords <- c(xmin=brush$xmin, xmax=brush$xmax, ymin=brush$ymin, ymax=brush$ymax)
                        if (is.null(pObjects$memory$heatMapPlot[id0,][[.zoomData]][[1]])) { # if we haven't already zoomed in
                            inp_rows <- seq_along(pObjects$memory$heatMapPlot[id0,][[.heatMapFeatName]][[1]])
                        } else {
                            inp_rows <- pObjects$memory$heatMapPlot[id0,][[.zoomData]][[1]]
                        }

                        # Is the heatmap receiving a color brush (in that case the number of annotations should be increased by 1)
                        trans_enc <- .decoded2encoded(pObjects$memory$heatMapPlot[id0, .selectByPlot])
                        is_receiving_color_selection <- pObjects$memory$heatMapPlot[id0, .selectByPlot]!=.noSelection &&
                            pObjects$memory$heatMapPlot[id0,.selectEffect]==.selectColorTitle &&
                            .transmitted_selection(trans_enc, pObjects$memory, mode=mode0, id=id0)

                        # Update data.
                        n.annot <- length(pObjects$memory$heatMapPlot[,.heatMapColData][[id0]]) + is_receiving_color_selection
                        ymin <- .transform_global_to_local_y(new_coords["ymin"], n.genes=length(inp_rows), n.annot=n.annot)
                        ymax <- .transform_global_to_local_y(new_coords["ymax"], n.genes=length(inp_rows), n.annot=n.annot)
                        new_rows <- inp_rows[ymin:ymax]

                    } else {
                        new_rows <- NULL # Zoom out.
                    }

                    pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .zoomData, new_rows)
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                }, ignoreInit=TRUE)
            })
        }

        #######################################################################
        # Feature/sample name selectize updators. ----
        #######################################################################

        feature_choices <- seq_len(nrow(se))
        names(feature_choices) <- rownames(se)
        sample_choices <- seq_len(ncol(se))
        names(sample_choices) <- colnames(se)

        # Selectize updates for features.
        for (mode in c(point_plot_types, "heatMapPlot")) {
            if (mode=="featAssayPlot") {
                fields <- c(.featAssayYAxisFeatName, .featAssayXAxisFeatName, .colorByFeatName)
            } else if (mode %in% point_plot_types) {
                fields <- .colorByFeatName
            } else if (mode=="heatMapPlot") {
                fields <- .heatMapFeatName
            }

            max_plots <- nrow(pObjects$memory[[mode]])
            for (id in seq_len(max_plots)) {
                for (field in fields) {
                    local({
                        id0 <- id
                        mode0 <- mode
                        field0 <- field
                        cur_field <- paste0(mode0, id0, "_", field0)

                        observe({
                            force(rObjects$rerendered)
                            updateSelectizeInput(session, cur_field, choices=feature_choices, server=TRUE,
                                selected=pObjects$memory[[mode0]][id0, field0][[1]])
                        })
                    })
                }
            }
        }

        # Selectize updates for samples.
        for (mode in point_plot_types) {
            if (mode=="sampAssayPlot") {
                fields <- c(.sampAssayYAxisSampName, .sampAssayXAxisSampName, .colorBySampName)
            } else if (mode %in% point_plot_types) {
                fields <- .colorBySampName
            }

            max_plots <- nrow(pObjects$memory[[mode]])
            for (id in seq_len(max_plots)) {
                for (field in fields) {
                    local({
                        id0 <- id
                        mode0 <- mode
                        field0 <- field
                        cur_field <- paste0(mode0, id0, "_", field0)

                        observe({
                            force(rObjects$rerendered)
                            updateSelectizeInput(session, cur_field, choices=sample_choices, server=TRUE,
                                selected=pObjects$memory[[mode0]][id0, field0][[1]])
                        })
                    })
                }
            }
        }

        #####################################################################
        # Multiple selection observers.
        #######################################################################

        for (mode in point_plot_types) {
            max_plots <- nrow(pObjects$memory[[mode]])

            for (id in seq_len(max_plots)) {
                local({
                    mode0 <- mode
                    id0 <- id
                    plot_name <- paste0(mode0, id0)
                    save_field <- paste0(plot_name, "_", .multiSelectSave)
                    del_field <- paste0(plot_name, "_", .multiSelectDelete)
                    info_field <- paste0(plot_name, "_", .panelGeneralInfo)
                    saved_field <- paste0(plot_name, "_", .selectMultiSaved)
                    resaved_field <- paste0(plot_name, "_resaved")

                    ## Save selection observer. ---
                    observeEvent(input[[save_field]], {
                        current <- pObjects$memory[[mode0]][,.multiSelectHistory][[id0]]

                        to_store <- pObjects$memory[[mode0]][,.brushData][[id0]]
                        if (is.null(to_store)) {
                            to_store <- pObjects$memory[[mode0]][,.lassoData][[id0]]
                            if (is.null(to_store) || !to_store$closed) {
                                return(NULL)
                            }
                        }

                        pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .multiSelectHistory, c(current, list(to_store)))

                        # Updating self (replot to get number).
                        rObjects[[info_field]] <- .increment_counter(isolate(rObjects[[info_field]]))
                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

                        transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                        if (transmitter!=.noSelection && .decoded2encoded(transmitter)==plot_name) {
                            rObjects[[saved_field]] <- .increment_counter(isolate(rObjects[[saved_field]]))
                            if (pObjects$memory[[mode0]][id0, .selectMultiType]==.selectMultiUnionTitle) {
                                rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                            }
                        }

                        # Updating children.
                        rObjects[[resaved_field]] <- .increment_counter(isolate(rObjects[[resaved_field]]))

                        .disableButtonIf(
                            del_field,
                            !.any_saved_selection(mode0, id0, pObjects$memory),
                            .buttonEmptyHistoryLabel, .buttonDeleteLabel, session
                        )
                    })

                    ## Deleted selection observer. ---
                    observeEvent(input[[del_field]], {
                        current <- pObjects$memory[[mode0]][,.multiSelectHistory][[id0]]
                        current <- head(current, -1)
                        pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, .multiSelectHistory, current)

                        # Updating self.
                        rObjects[[info_field]] <- .increment_counter(isolate(rObjects[[info_field]]))
                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

                        transmitter <- pObjects$memory[[mode0]][id0, .selectByPlot]
                        if (transmitter!=.noSelection && .decoded2encoded(transmitter)==plot_name) {
                            rObjects[[saved_field]] <- .increment_counter(isolate(rObjects[[saved_field]]))

                            reset <- pObjects$memory[[mode0]][id0, .selectMultiSaved] > length(current)
                            if (reset) {
                                pObjects$memory[[mode0]][id0, .selectMultiSaved] <- 0L
                            }
                        }

                        # Updating children.
                        rObjects[[resaved_field]] <- .increment_counter(isolate(rObjects[[resaved_field]]))

                        .disableButtonIf(
                            del_field,
                            !.any_saved_selection(mode0, id0, pObjects$memory),
                            .buttonEmptyHistoryLabel, .buttonDeleteLabel, session
                        )
                    })
                })
            }
        }

        #######################################################################
        # Dot-related plot creation section. ----
        #######################################################################

        for (mode in point_plot_types) {
            max_plots <- nrow(pObjects$memory[[mode]])

            # Defining mode-specific plotting functions.
            FUN <- switch(mode,
                redDimPlot=.make_redDimPlot,
                featAssayPlot=.make_featAssayPlot,
                colDataPlot=.make_colDataPlot,
                rowDataPlot=.make_rowDataPlot,
                sampAssayPlot=.make_sampAssayPlot)

            # Defining fundamental parameters that destroy brushes/lassos upon being changed.
            protected <- switch(mode,
                redDimPlot=c(.redDimXAxis, .redDimYAxis),
                colDataPlot=c(.colDataYAxis, .colDataXAxis, .colDataXAxisColData),
                featAssayPlot=c(.featAssayAssay, .featAssayXAxisColData),
                rowDataPlot=c(.colorBySampNameAssay, .rowDataYAxis, .rowDataXAxis, .rowDataXAxisRowData),
                sampAssayPlot=c(.sampAssayAssay, .sampAssayXAxisRowData))
            protected <- c(protected, .facetByRow, .facetByColumn, .facetRowsByColData, .facetColumnsByColData)

            # Defining non-fundamental parameters that do not destroy brushes/lassos.
            if (mode %in% row_point_plot_types) {
                nonfundamental <- c(.colorByRowData, .colorByFeatNameColor, .shapeByField, .shapeByRowData, .sizeByField, .sizeByRowData, .colorByFeatNameColor)
            } else {
                nonfundamental <- c(.colorByColData, .colorByFeatNameAssay, .shapeByField, .shapeByColData, .sizeByField, .sizeByColData, .colorBySampNameColor)
            }
            nonfundamental <- c(nonfundamental,
                .colorByDefaultColor, .selectColor, .selectTransAlpha,
                .plotPointSize, .plotPointAlpha, .plotFontSize, .plotLegendPosition,
                .plotPointDownsample, .plotPointSampleRes, .contourAddTitle,
                .contourColor)

            for (id in seq_len(max_plots)) {
                # Observers for the non-fundamental parameter options.
                for (field in nonfundamental) {
                    local({
                        id0 <- id
                        mode0 <- mode
                        field0 <- field
                        plot_name <- paste0(mode0, id0)
                        cur_field <- paste0(plot_name, "_", field0)

                        observeEvent(input[[cur_field]], {
                            matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                            if (identical(matched_input, pObjects$memory[[mode0]][[field0]][id0])) {
                                return(NULL)
                            }
                            pObjects$memory[[mode0]][[field0]][id0] <- matched_input
                            rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                        }, ignoreInit=TRUE)
                    })
                }

                # Observers for non-fundamental list parameters.
                for (field in c(.visualParamChoice)) {
                    local({
                        id0 <- id
                        mode0 <- mode
                        field0 <- field
                        plot_name <- paste0(mode0, id0)
                        cur_field <- paste0(plot_name, "_", field0)

                        observeEvent(input[[cur_field]], {
                            existing <- pObjects$memory[[mode0]][,field0][[id0]]
                            incoming <- as(input[[cur_field]], typeof(existing))
                            if (identical(incoming, existing)) {
                                return(NULL)
                            }
                            pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, field0, incoming)
                        }, ignoreInit=TRUE, ignoreNULL=FALSE)
                    })
                }

                # Observers for the fundamental plot parameters.
                for (field in protected) {
                    local({
                        id0 <- id
                        mode0 <- mode
                        field0 <- field
                        plot_name <- paste0(mode0, id0)
                        cur_field <- paste0(plot_name, "_", field0)

                        observeEvent(input[[cur_field]], {
                            matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                            if (identical(matched_input, pObjects$memory[[mode0]][[field0]][id0])) {
                                return(NULL)
                            }
                            pObjects$memory[[mode0]][[field0]][id0] <- matched_input
                            .regenerate_unselected_plot(mode0, id0, pObjects, rObjects)
                         }, ignoreInit=TRUE)
                    })
                }

                # Feature and sample name observers. This is handled separately from the other observers,
                # due to the fact that the selectizeInput can be updated and because the feature name
                # can change (due to the linked table) without directly affecting the plot.
                for (field_type in c("feature", "sample")) {
                    if (field_type=="feature") {
                        name_field <- .colorByFeatName
                        color_title <- .colorByFeatNameTitle
                        table_field <- .colorByRowTable
                        choices <- feature_choices
                    } else {
                        name_field <- .colorBySampName
                        color_title <- .colorBySampNameTitle
                        table_field <- .colorByColTable
                        choices <- sample_choices
                    }

                    local({
                        id0 <- id
                        mode0 <- mode
                        name_field0 <- name_field
                        color_title0 <- color_title
                        table_field0 <- table_field
                        choices0 <- choices
                        plot_name <- paste0(mode0, id0)

                        # Observer for the feature/sample name.
                        name_input <- paste0(plot_name, "_", name_field0)
                        observeEvent(input[[name_input]], {
                            req(input[[name_input]]) # Required to defend against empty strings before updateSelectizeInput runs upon re-render.
                            matched_input <- as(input[[name_input]], typeof(pObjects$memory[[mode0]][[name_field0]]))
                            if (identical(matched_input, pObjects$memory[[mode0]][[name_field0]][id0])) {
                                return(NULL)
                            }
                            pObjects$memory[[mode0]][[name_field0]][id0] <- matched_input
                            if (pObjects$memory[[mode0]][id0,.colorByField]==color_title0) { # Only regenerating if featName is used for coloring.
                                rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                            }
                        }, ignoreInit=TRUE)

                        # Observers for the linked color by feature name. This also updates the table_links information.
                        observe({
                            replot <- .setup_table_observer(mode0, id0, pObjects, rObjects, input, session,
                                                            by_field=.colorByField, title=color_title0,
                                                            select_field=name_field0, tab_field=table_field0,
                                                            select_choices=choices0, param='color')
                            if (replot) {
                                rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                            }
                        })
                    })
                }

                local({
                    id0 <- id
                    mode0 <- mode
                    FUN0 <- FUN
                    plot_name <- paste0(mode0, id0)

                    # Defining the rendered plot, and saving the coordinates.
                    gen_field <- paste0(plot_name, "_", .panelGeneralInfo)
                    output[[plot_name]] <- renderPlot({
                        force(rObjects[[plot_name]])
                        rObjects[[gen_field]] <- .increment_counter(isolate(rObjects[[gen_field]]))
                        p.out <- FUN0(id0, pObjects$memory, pObjects$coordinates, se, colormap)
                        pObjects$commands[[plot_name]] <- p.out$cmd_list
                        pObjects$coordinates[[plot_name]] <- p.out$xy[, intersect(.allCoordinatesNames, colnames(p.out$xy))]
                        p.out$plot
                    })

                    # Describing some general panel information.
                    dec_name <- .decode_panel_name(mode0, id0)
                    output[[gen_field]] <- renderUI({
                        force(rObjects[[gen_field]])
                        selected <- .get_selected_points(
                            rownames(pObjects$coordinates[[plot_name]]), dec_name,
                            pObjects$memory, pObjects$coordinates, select_all=TRUE
                        )

                        all_output <- list()
                        if (!is.null(selected$active)) {
                            n_selected <- sum(selected$active)
                            n_total <- length(selected$active)
                            all_output <- append(all_output,
                                list(
                                    sprintf(
                                        "%i of %i points in active selection (%.1f%%)",
                                        n_selected, n_total, 100*n_selected/n_total
                                    ),
                                    br()
                                )
                            )
                        }

                        for (i in seq_along(selected$saved)) {
                            n_selected <- sum(selected$saved[[i]])
                            n_total <- length(selected$saved[[i]])
                            all_output <- append(all_output,
                                list(
                                    sprintf(
                                        "%i of %i points in saved selection %i (%.1f%%)",
                                        n_selected, n_total, i, 100*n_selected/n_total
                                    ),
                                    br()
                                )
                            )
                        }

                        if (length(all_output)==0L) return(NULL)
                        do.call(tagList, all_output)
                    })

                    # Describing the links between panels.
                    link_field <- paste0(plot_name, "_", .panelLinkInfo)
                    output[[link_field]] <- renderUI({
                        force(rObjects[[link_field]])
                        .define_plot_links(plot_name, pObjects$memory, pObjects$selection_links)
                    })
                })
            }
        }

        #######################################################################
        # Feature/sample assay plot section. ----
        #######################################################################

        # Feature and sample assay plots need some careful handling, as we need to update the
        # table links and destroy a brush/lasso whenever an x/y-axis-specifying parameter changes.
        for (mode in c("featAssayPlot", "sampAssayPlot")) {
            max_plots <- nrow(pObjects$memory[[mode]])
            if (mode=="featAssayPlot") {
                byx_field <- .featAssayXAxis
                byx_title <- .featAssayXAxisFeatNameTitle
                x_name_field <- .featAssayXAxisFeatName
                x_name_tab <- .featAssayXAxisRowTable
                y_name_field <- .featAssayYAxisFeatName
                y_name_tab <- .featAssayYAxisRowTable
                choices <- feature_choices
            } else {
                byx_field <- .sampAssayXAxis
                byx_title <- .sampAssayXAxisSampNameTitle
                x_name_field <- .sampAssayXAxisSampName
                x_name_tab <- .sampAssayXAxisColTable
                y_name_field <- .sampAssayYAxisSampName
                y_name_tab <- .sampAssayYAxisColTable
                choices <- sample_choices
            }

            for (id in seq_len(max_plots)) {
                local({
                    id0 <- id
                    mode0 <- mode
                    plot_name <- paste0(mode0, id0)

                    byx_field0 <- byx_field
                    byx_title0 <- byx_title
                    x_name_field0 <- x_name_field
                    x_name_tab0 <- x_name_tab
                    y_name_field0 <- y_name_field
                    y_name_tab0 <- y_name_tab
                    choices0 <- choices

                    # X-axis table observer.
                    observe({
                        replot <- .setup_table_observer(mode0, id0, pObjects, rObjects, input, session,
                                                        by_field=byx_field0, title=byx_title0,
                                                        select_field=x_name_field0, tab_field=x_name_tab0,
                                                        select_choices=choices0, param="xaxis")
                        if (replot) {
                            .regenerate_unselected_plot(mode0, id0, pObjects, rObjects)
                        }
                    })

                    # X-axis feature name observer (see the explanation above for the colorByFeatName observer).
                    x_field <- paste0(plot_name, "_", x_name_field0)
                    observeEvent(input[[x_field]], {
                        req(input[[x_field]]) # Required to defend against empty strings in XAxisFeatName prior to updateSelectize upon re-render.
                        matched_input <- as(input[[x_field]], typeof(pObjects$memory[[mode0]][[x_name_field0]]))
                        if (identical(matched_input, pObjects$memory[[mode0]][[x_name_field0]][id0])) {
                            return(NULL)
                        }
                        pObjects$memory[[mode0]][[x_name_field0]][id0] <- matched_input
                        if (pObjects$memory[[mode0]][id0, byx_field0]==byx_title0) { # Only regenerating if featName is being used for plotting.
                            .regenerate_unselected_plot(mode0, id0, pObjects, rObjects)
                        }
                    }, ignoreInit=TRUE)

                    # Y-axis table observer.
                    observe({
                        replot <- .setup_table_observer(mode0, id0, pObjects, rObjects, input, session,
                                                        select_field=y_name_field0, tab_field=y_name_tab0,
                                                        select_choices=choices0, param="yaxis")
                        if (replot) {
                            .regenerate_unselected_plot(mode0, id0, pObjects, rObjects)
                        }
                    })

                    # Y-axis feature name observer. Unlike the X-axis observer, there is no choice for the Y-Axis,
                    # i.e., the feature name is always being used for plotting.
                    y_field <- paste0(plot_name, "_", y_name_field0)
                    observeEvent(input[[y_field]], {
                        req(input[[y_field]]) # Required for empty strings in YAxisFeatName prior to updateSelectize upon re-render.
                        matched_input <- as(input[[y_field]], typeof(pObjects$memory[[mode0]][[y_name_field0]]))
                        if (identical(matched_input, pObjects$memory[[mode0]][[y_name_field0]][id0])) {
                            return(NULL)
                        }
                        pObjects$memory[[mode0]][[y_name_field0]][id0] <- matched_input
                        .regenerate_unselected_plot(mode0, id0, pObjects, rObjects)
                    }, ignoreInit=TRUE)
                })
            }
        }

        #######################################################################
        # Reduced dimension plot section. ----
        #######################################################################

        # Reduced dimension plots also need a special observer to update the maximum of the selectInput when the type changes.
        max_plots <- nrow(pObjects$memory$redDimPlot)
        for (id in seq_len(max_plots)) {
            local({
                id0 <- id
                mode0 <- "redDimPlot"
                plot_name <- paste0(mode0, id0)
                cur_field <- paste0(plot_name, "_", .redDimType)
                dim_fieldX <- paste0(plot_name, "_", .redDimXAxis)
                dim_fieldY <- paste0(plot_name, "_", .redDimYAxis)

                observeEvent(input[[cur_field]], {
                    matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[.redDimType]]))
                    if (identical(matched_input, pObjects$memory[[mode0]][[.redDimType]][id0])) {
                        return(NULL)
                    }
                    pObjects$memory[[mode0]][[.redDimType]][id0] <- matched_input

                    # Updating the selectInputs as well. This should not trigger re-plotting as the identical() check in the
                    # corresponding observers should stop the replotting flag from being set.
                    new_max <- ncol(reducedDim(se, matched_input))
                    capped_X <- pmin(new_max, pObjects$memory[[mode0]][[.redDimXAxis]][id0])
                    capped_Y <- pmin(new_max, pObjects$memory[[mode0]][[.redDimYAxis]][id0])
                    pObjects$memory[[mode0]][[.redDimXAxis]][id0] <- capped_X
                    pObjects$memory[[mode0]][[.redDimYAxis]][id0] <- capped_Y

                    new_choices <- seq_len(new_max)
                    names(new_choices) <- new_choices
                    updateSelectInput(session, dim_fieldX, choices=new_choices, selected=capped_X)
                    updateSelectInput(session, dim_fieldY, choices=new_choices, selected=capped_Y)

                    .regenerate_unselected_plot(mode0, id0, pObjects, rObjects)
                }, ignoreInit=TRUE)
            })
        }

        #######################################################################
        # Custom panel section. ----
        #######################################################################

        for (mode in custom_panel_types) {
            max_plots <- nrow(pObjects$memory[[mode]])

            for (id in seq_len(max_plots)) {
                # UI containing transmission information.
                local({
                    id0 <- id
                    mode0 <- mode
                    panel_name <- paste0(mode0, id0)

                    link_field <- paste0(panel_name, "_", .panelLinkInfo)
                    output[[link_field]] <- renderUI({
                        force(rObjects[[link_field]])
                        output <- list()
                        for (src in c(.customRowSource, .customColSource)) {
                            select_in <- pObjects$memory[[mode0]][id0,src]
                            if (select_in!=.noSelection) {
                                output <- c(output, list("Receiving selection from", em(strong(select_in)), br()))
                            }
                        }
                        do.call(tagList, output)
                    })
                })

                # Field defining the function to use.
                for (field in c(.customFun)) {
                    local({
                        id0 <- id
                        mode0 <- mode
                        field0 <- field
                        panel_name <- paste0(mode0, id0)
                        cur_field <- paste0(panel_name, "_", field0)

                        observeEvent(input[[cur_field]], {
                            matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                            if (identical(matched_input, pObjects$memory[[mode0]][[field0]][id0])) {
                                return(NULL)
                            }
                            pObjects$memory[[mode0]][[field0]][id0] <- matched_input
                            rObjects[[panel_name]] <- .increment_counter(isolate(rObjects[[panel_name]]))
                        }, ignoreInit=TRUE)
                    })
                }

                local({
                    id0 <- id
                    mode0 <- mode
                    panel_name <- paste0(mode0, id0)
                    cur_vis_field <- paste0(panel_name, "_", .customVisibleArgs)
                    cur_submit <- paste0(panel_name, "_", .customSubmit)

                    # Switch button class depending on whether the arguments and visible arguments are different.
                    observeEvent(input[[cur_vis_field]], {
                        matched_input <- as(input[[cur_vis_field]], typeof(pObjects$memory[[mode0]][[.customVisibleArgs]]))
                        pObjects$memory[[mode0]][[.customVisibleArgs]][id0] <- matched_input

                        .disableButtonIf(
                            id=cur_submit,
                            condition=identical(matched_input, pObjects$memory[[mode0]][[.customArgs]][id0]),
                            inactiveLabel=.buttonUpToDateLabel, activeLabel=.buttonUpdateLabel, session)
                    }, ignoreInit=TRUE)

                    # Switch visible arguments with Arguments upon button click.
                    observeEvent(input[[cur_submit]], {
                        visible <- pObjects$memory[[mode0]][[.customVisibleArgs]][[id0]]
                        if (identical(visible, pObjects$memory[[mode0]][[.customArgs]][id0])) {
                            return(NULL)
                        }
                        pObjects$memory[[mode0]][[.customArgs]][[id0]] <- visible
                        rObjects[[panel_name]] <- .increment_counter(isolate(rObjects[[panel_name]]))
                        disable(cur_submit)
                        updateActionButton(session, cur_submit, .buttonUpToDateLabel)
                    }, ignoreInit=TRUE)
                })

                # Specifying the row/column selection.
                for (src in c(.customRowSource, .customColSource)) {
                    local({
                        id0 <- id
                        src0 <- src
                        mode0 <- mode
                        panel_name <- paste0(mode0, id0)
                        select_panel_field <- paste0(panel_name, "_", src0)

                        observeEvent(input[[select_panel_field]], {
                            old_transmitter <- pObjects$memory[[mode0]][id0, src0]
                            new_transmitter <- input[[select_panel_field]]

                            # Determining whether the new and old transmitting panel have selections.
                            old_encoded <- old_transmitter
                            if (old_transmitter!=.noSelection) {
                                old_encoded <- .decoded2encoded(old_transmitter)
                            }
                            new_encoded <- new_transmitter
                            if (new_transmitter!=.noSelection) {
                                new_encoded <- .decoded2encoded(new_transmitter)
                            }

                            # Trying to update the graph. No need to worry about DAGs as custom panels cannot transmit.
                            pObjects$selection_links <- .choose_new_selection_source(pObjects$selection_links, panel_name, new_encoded, old_encoded)
                            pObjects$memory[[mode0]][id0, src0] <- new_transmitter

                            # Update the elements reporting the links between panels.
                            for (relinked in setdiff(c(old_encoded, new_encoded, panel_name), .noSelection)) {
                                relink_field <- paste0(relinked, "_", .panelLinkInfo)
                                rObjects[[relink_field]] <- .increment_counter(isolate(rObjects[[relink_field]]))
                            }

                            # Not recreating panels if there were no selection in either the new or old transmitters.
                            # We use "Union" here to trigger replotting if customSendAll=TRUE as custom plots will
                            # receive everything from their upstream transmitters. Otherwise, if customSendAll=TRUE,
                            # we only respond to the active selection in the upstream transmitter.
                            select_type <- if (customSendAll) .selectMultiUnionTitle else .selectMultiActiveTitle
                            no_old_selection <- !.transmitted_selection(old_encoded, pObjects$memory, select_type=select_type, select_saved=0L)
                            no_new_selection <- !.transmitted_selection(new_encoded, pObjects$memory, select_type=select_type, select_saved=0L)
                            if (no_old_selection && no_new_selection) {
                                return(NULL)
                            }

                            # Triggering self update of the panel.
                            rObjects[[panel_name]] <- .increment_counter(isolate(rObjects[[panel_name]]))
                        })
                    })
                }
            }
        }

        # Defining the custom plots.
        for (id in seq_len(nrow(pObjects$memory$customDataPlot))) {
            local({
                id0 <- id
                plot_name <- paste0("customDataPlot", id0)

                output[[plot_name]] <- renderPlot({
                    force(rObjects[[plot_name]])
                    p.out <- .make_customDataPlot(id0, pObjects$memory, pObjects$coordinates, se, select_all=customSendAll)
                    pObjects$commands[[plot_name]] <- p.out$cmd_list
                    p.out$plot
                })
            })
        }

        # Defining the custom tables.
        for (id in seq_len(nrow(pObjects$memory$customStatTable))) {
            local({
                id0 <- id
                panel_name <- paste0("customStatTable", id0)

                output[[panel_name]] <- renderDataTable({
                    force(rObjects$active_panels) # to trigger recreation when the number of plots is changed.
                    force(rObjects[[panel_name]])
                    param_choices <- pObjects$memory$customStatTable[id0,]
                    chosen_fun <- param_choices[[.customFun]]
                    if (chosen_fun==.noSelection) {
                        return(NULL)
                    }

                    select_out <- .process_custom_selections(param_choices, pObjects$memory, select_all = customSendAll)
                    brushes <- lassos <- histories <- list()

                    for (i in seq_along(select_out$transmitter)) {
                        current <- select_out$transmitter[[i]]
                        transmit_param <- pObjects$memory[[current$Type]][current$ID,]
                        temp_env <- new.env()
                        .populate_selection_environment(transmit_param, temp_env)

                        # Storing the extracted brushes and such.
                        brushes <- c(brushes, temp_env$all_brushes)
                        lassos <- c(lassos, temp_env$all_lassos)
                        histories <- c(histories, temp_env$all_select_histories)
                    }

                    eval_env <- new.env()
                    eval_env$all_coordinates <- pObjects$coordinates
                    eval_env$all_brushes <- brushes
                    eval_env$all_lassos <- lassos
                    eval_env$all_select_histories <- histories
                    .text_eval(select_out$cmds, eval_env)

                    row_selected <- eval_env$row.names
                    col_selected <- eval_env$col.names

                    chosen_args <- param_choices[[.customArgs]]
                    FUN <- .get_internal_info(se, "custom_stat_fun")[[chosen_fun]]
                    tmp_df <- do.call(FUN, c(list(se, row_selected, col_selected), as.list(.text2args(chosen_args))))

                    search <- param_choices[[.customStatSearch]]
                    datatable(tmp_df, filter="top", rownames=TRUE,
                        options=list(search=list(search=search, smart=FALSE, regex=TRUE, caseInsensitive=FALSE), scrollX=TRUE))
                })

                # Updating memory for new selection parameters.
                search_field <- paste0(panel_name, .int_customStatSearch)
                observe({
                    search <- input[[search_field]]
                    if (length(search)) {
                        pObjects$memory$customStatTable[id0, .customStatSearch] <- search
                    }
                })
            })
        }

        #######################################################################
        # Linked table section. ----
        #######################################################################

        for (mode in linked_table_types) {
            max_plots <- nrow(pObjects$memory[[mode]])
            if (mode == "rowStatTable") {
                current_df <- feature_data
                current_select_col <- feature_data_select_col
                choices <- feature_choices
                col_field <- .colorByFeatName
                x_field <- .featAssayXAxisFeatName
                y_field <- .featAssayYAxisFeatName
            } else {
                current_df <- sample_data
                current_select_col <- sample_data_select_col
                choices <- sample_choices
                col_field <- .colorBySampName
                x_field <- .sampAssayXAxisSampName
                y_field <- .sampAssayYAxisSampName
            }

            for (id in seq_len(max_plots)) {
                local({
                    mode0 <- mode
                    id0 <- id
                    panel_name <- paste0(mode0, id0)

                    current_df0 <- current_df
                    current_select_col0 <- current_select_col

                    col_field0 <- col_field
                    x_field0 <- x_field
                    y_field0 <- y_field
                    choices0 <- choices

                    output[[panel_name]] <- renderDataTable({
                        force(rObjects$active_panels) # to trigger recreation when the number of plots is changed.
                        force(rObjects[[panel_name]])

                        param_choices <- pObjects$memory[[mode0]][id0,]
                        chosen <- param_choices[[.statTableSelected]]
                        search <- param_choices[[.statTableSearch]]
                        search_col <- param_choices[[.statTableColSearch]][[1]]
                        search_col <- lapply(search_col, FUN=function(x) { list(search=x) })

                        # After the first initialization there may not be any need to add columns
                        # Fact is, the extra "Selected" column may make the table it has more columns than filters
                        missing_columns <- max(0, ncol(current_df0) - length(search_col))
                        search_col <- c(search_col, rep(list(list(search="")), missing_columns)) # TODO: fix for internal fields.

                        # Adding a "Selected" field to the plotting data, which responds to point selection input.
                        # Note that this AUTOMATICALLY updates search_col upon re-rendering via the observer below.
                        # The code below keeps search_col valid for the number of columns (i.e., with or without selection).
                        selected <- .get_selected_points(rownames(current_df0), param_choices[[.selectByPlot]], pObjects$memory, pObjects$coordinates,
                            select_type=param_choices[[.selectMultiType]], select_saved=param_choices[[.selectMultiSaved]])

                        tmp_df <- current_df0
                        columnDefs <- list()
                        if (!is.null(selected)) {
                            tmp_df[[current_select_col0]] <- selected
                            if (length(search_col)!=ncol(tmp_df)) {
                                search_col <- c(search_col, list(list(search="[\"true\"]"))) # brackets appears to fix row indexing in RStudio browser (1/2)
                            } else {
                                search_col[[ncol(tmp_df)]]$search <- "[\"true\"]" # brackets appears to fix row indexing in RStudio browser (2/2)
                            }
                            columnDefs <- append(columnDefs, list(list(visible=FALSE, targets=length(search_col)))) # this line must stay below the if block
                        } else {
                            search_col <- search_col[seq_len(ncol(tmp_df))]
                        }

                        datatable(
                            tmp_df, filter="top", rownames=TRUE,
                            options=list(
                                search=list(search=search, smart=FALSE, regex=TRUE, caseInsensitive=FALSE),
                                searchCols=c(list(NULL), search_col), # row names are the first column!
                                columnDefs=columnDefs,
                                scrollX=TRUE),
                            selection=list(mode="single", selected=chosen))
                    })

                    # Updating memory for new selection parameters (no need for underscore
                    # in 'select_field' definition, as this is already in the '.int' constant).
                    select_field <- paste0(panel_name, .int_statTableSelected)
                    observe({
                        chosen <- input[[select_field]]
                        if (length(chosen)==0L) {
                            return(NULL)
                        }
                        pObjects$memory[[mode0]][id0, .statTableSelected] <- chosen

                        col_kids <- pObjects$table_links[[panel_name]][["color"]]
                        x_kids <- pObjects$table_links[[panel_name]][["xaxis"]]
                        y_kids <- pObjects$table_links[[panel_name]][["yaxis"]]

                        # Updating the selectize for the color choice.
                        col_kids <- sprintf("%s_%s", col_kids, col_field0)
                        for (kid in col_kids) {
                            updateSelectizeInput(session, kid, label=NULL, server=TRUE, selected=chosen, choices=choices0)
                        }

                        # Updating the selectize for the x-/y-axis choices.
                        x_kids <- sprintf("%s_%s", x_kids, x_field0)
                        for (kid in x_kids) {
                            updateSelectizeInput(session, kid, label=NULL, server=TRUE, selected=chosen, choices=choices0)
                        }
                        y_kids <- sprintf("%s_%s", y_kids, y_field0)
                        for (kid in y_kids) {
                            updateSelectizeInput(session, kid, label=NULL, server=TRUE, selected=chosen, choices=choices0)
                        }

                        # There is a possibility that this would cause triple-rendering as they trigger different observers.
                        # But this would imply that you're plotting/colouring the same gene against itself, which would be stupid.
                    })

                    # Updating memory for new selection parameters.
                    search_field <- paste0(panel_name, .int_statTableSearch)
                    observe({
                        search <- input[[search_field]]
                        if (length(search)) {
                            pObjects$memory[[mode0]][id0, .statTableSearch] <- search
                        }
                    })

                    colsearch_field <- paste0(panel_name, .int_statTableColSearch)
                    observe({
                        search <- input[[colsearch_field]]
                        if (length(search)) {
                            pObjects$memory[[mode0]]<- .update_list_element(pObjects$memory[[mode0]], id0, .statTableColSearch, search)
                        }
                    })

                    # Updating the annotation box.
                    if (mode0 == "rowStatTable") {
                        anno_field <- paste0(panel_name, "_annotation")
                        output[[anno_field]] <- renderUI({
                            if(is.null(annotFun)) return(NULL)
                            chosen <- input[[select_field]]
                            annotFun(se,chosen)
                        })
                    }

                    # Describing the links between panels.
                    link_field <- paste0(panel_name, "_", .panelLinkInfo)
                    output[[link_field]] <- renderUI({
                        force(rObjects[[link_field]])
                        .define_table_links(panel_name, pObjects$memory, pObjects$table_links)
                    })
                })
            }
        }

        #######################################################################
        # Voice observers. ----
        #######################################################################

        observeEvent(input[[.voiceShowPanelInput]], {
            voice <- input[[.voiceShowPanelInput]]
            if (voice != "") {
                showNotification(sprintf("<Show panel> %s", voice), type="message")
            }

            decodedPanel <- .nearestDecodedPanel(voice, memory, max.edits=5)
            if (is.null(decodedPanel)) { return(NULL) }
            encodedPanel <- .decoded2encoded(decodedPanel)
            encodedSplit <- .split_encoded(encodedPanel)

            # Add the panel to the active table if not there yet
            all_active <- rObjects$active_panels
            if (any(all_active$Type==encodedSplit$Type & all_active$ID==encodedSplit$ID)) {
                return(NULL)
            }

            rObjects$active_panels <- .showPanel(encodedSplit$Type, encodedSplit$ID, all_active)

            # Memorize last valid panel (only if the command succeeded)
            showNotification(sprintf("<Show panel> %s", decodedPanel), type="message")
            pObjects[[.voiceActivePanel]] <- encodedPanel
            showNotification(sprintf("Active panel: %s", decodedPanel), id=.voiceActivePanel, duration=NULL)
        })

        observeEvent(input[[.voiceHidePanelInput]], {
            voice <- input[[.voiceHidePanelInput]]
            if (voice != "") {
                showNotification(sprintf("<Hide panel> %s", voice), type="message")
            }

            decodedPanel <- .nearestDecodedPanel(voice, memory, max.edits=5)
            if (is.null(decodedPanel)) { return(NULL) }
            encodedPanel <- .decoded2encoded(decodedPanel)
            encodedSplit <- .split_encoded(encodedPanel)

            # Remove the panel to the active table if it is currently there
            all_active <- rObjects$active_panels
            panelIndex <- which(all_active$Type==encodedSplit$Type & all_active$ID==encodedSplit$ID)
            if (length(panelIndex) == 0) {
                showNotification(sprintf("Panel %s is not currently active", decodedPanel), type="error")
                return(NULL)
            }

            rObjects$active_panels <- .hidePanel(encodedSplit$Type, encodedSplit$ID, all_active, pObjects)

            showNotification(sprintf("<Hide panel> %s", decodedPanel), type="message")
            # Clear memory of last panel accessed, as this one is now inactive
            pObjects[[.voiceActivePanel]] <- NA_character_
            removeNotification(.voiceActivePanel, session)
            showNotification("Panel memory cleared", type="message")
        })

        observeEvent(input[[.voiceControlPanelInput]], {
            voice <- input[[.voiceControlPanelInput]]
            if (voice != "") {
                showNotification(sprintf("<Control panel> %s", voice), type="message")
            }

            decodedPanel <- .nearestDecodedPanel(voice, memory, max.edits=5)
            if (is.null(decodedPanel)) { return(NULL) }
            encodedPanel <- .decoded2encoded(decodedPanel)
            encodedSplit <- .split_encoded(encodedPanel)

            # Take control of the panel if it is currently there
            all_active <- rObjects$active_panels
            panelIndex <- which(all_active$Type==encodedSplit$Type & all_active$ID==encodedSplit$ID)
            if (length(panelIndex) == 0) {
                showNotification(sprintf("Panel %s is not currently active", decodedPanel), type="error")
                return(NULL)
            }

            # Memorize last valid panel (only if the command succeeded)
            showNotification(sprintf("<Control panel> %s", decodedPanel), type="message")
            pObjects[[.voiceActivePanel]] <- encodedPanel
            showNotification(sprintf("Active panel: %s", decodedPanel), id=.voiceActivePanel, duration=NULL)
        })

        observeEvent(input[[.voiceShowActivePanelInput]], {
            # TODO: refactor next 4 lines into function
            activePanel <- pObjects[[.voiceActivePanel]]
            if (is.na(activePanel)) {
                showNotification("No active panel", type="error")
                return(NULL)
            }
            activeSplit <- .split_encoded(activePanel)
            activeDecoded <- .decode_panel_name(activeSplit$Type, activeSplit$ID)
            showNotification(sprintf("Active panel: %s", activeDecoded), id=.voiceActivePanel, duration=NULL)
        })

        observeEvent(input[[.voiceColorUsingInput]], {
            # TODO: refactor next 4 lines into function
            activePanel <- pObjects[[.voiceActivePanel]]
            if (is.na(activePanel)) {
                showNotification("No active panel", type="error")
                return(NULL)
            }

            voice <- input[[.voiceColorUsingInput]]
            if (voice != "") {
                showNotification(sprintf("<Color using> %s", voice), type="message")
            }

            activeSplit <- .split_encoded(activePanel)

            # Check if the choice matches one of the available titles
            if (activeSplit$Type %in% row_point_plot_types) {
                create_FUN <- .define_color_options_for_row_plots
            } else {
                create_FUN <- .define_color_options_for_column_plots
            }
            choices <- create_FUN(se)
            matchedChoice <- .nearestValidChoice(voice, choices, max.edits=5)
            if (length(matchedChoice) != 1L) {
                return(NULL)
            }

            updateSelectizeInput(session, paste(activePanel, "ColorBy", sep="_"), selected=matchedChoice)
            showNotification(sprintf("<Color using> %s", matchedChoice), type="message")
        })

        observeEvent(input[[.voiceColorByInput]], {
            # TODO: refactor next 4 lines into function
            activePanel <- pObjects[[.voiceActivePanel]]
            if (is.na(activePanel)) {
                showNotification("No active panel", type="error")
                return(NULL)
            }

            voice <- input[[.voiceColorByInput]]
            if (voice != "") {
                showNotification(sprintf("<Color by> %s", voice), type="message")
            }

            activeSplit <- .split_encoded(activePanel)

            colorby_field <- paste0(activeSplit$Type, activeSplit$ID, "_", .colorByField)
            colorby_title <- isolate(input[[colorby_field]])

            # Fetch the available choices
            choices <- .colorByChoices(colorby_title, se)

            # Check if the choice matches one of the available values
            matchedChoice <- character(0)
            if (colorby_title == .colorByNothingTitle) {
                return(NULL)
            } else if (colorby_title == .colorByColDataTitle) {
                colorby_param <- .colorByColData
                matchedChoice <- .nearestValidChoice(voice, choices, max.edits=5)
            } else if (colorby_title == .colorByRowDataTitle) {
                colorby_param <- .colorByRowData
                matchedChoice <- .nearestValidChoice(voice, choices, max.edits=5)
            } else if (colorby_title == .colorByFeatNameTitle) {
                colorby_param <- .colorByFeatName
                matchedChoice <- .nearestValidNamedChoice(voice, choices, max.edits=5)
            } else if (colorby_title == .colorBySampNameTitle) {
                colorby_param <- .colorBySampName
                matchedChoice <- .nearestValidNamedChoice(voice, choices, max.edits=5)
            }

            if (length(matchedChoice) != 1L) {
                return(NULL)
            }

            updateSelectizeInput(session, paste(activePanel, colorby_param, sep="_"), selected=matchedChoice, choices=choices, server=TRUE)
            showNotification(sprintf("<Color by> %s", matchedChoice), type="message")
        })

        observeEvent(input[[.voiceReceiveFromInput]], {
            # TODO: refactor next 4 lines into function
            activePanel <- pObjects[[.voiceActivePanel]]
            if (is.na(activePanel)) {
                showNotification("No active panel", type="error")
                return(NULL)
            }

            voice <- input[[.voiceReceiveFromInput]]
            if (voice != "") {
                showNotification(sprintf("<Receive from> %s", voice), type="message")
            }

            decodedPanel <- .nearestDecodedPanel(voice, memory, max.edits=5)
            if (is.null(decodedPanel)) { return(NULL) }

            updateSelectizeInput(session, paste(activePanel, .selectByPlot, sep="_"), selected=decodedPanel)

            showNotification(sprintf("<Receive from> %s", decodedPanel), type="message")
        })

        observeEvent(input[[.voiceSendToInput]], {
            # TODO: refactor next 4 lines into function
            activePanel <- pObjects[[.voiceActivePanel]]
            if (is.na(activePanel)) {
                showNotification("No active panel", type="error")
                return(NULL)
            }

            activeSplit <- .split_encoded(activePanel)
            activeDecoded <- .decode_panel_name(activeSplit$Type, activeSplit$ID)

            voice <- input[[.voiceSendToInput]]
            if (voice != "") {
                showNotification(sprintf("<Send to> %s", voice), type="message")
            }

            decodedPanel <- .nearestDecodedPanel(voice, memory, max.edits=5)
            if (is.null(decodedPanel)) { return(NULL) }
            encodedPanel <- .decoded2encoded(decodedPanel)
            encodedSplit <- .split_encoded(encodedPanel)

            updateSelectizeInput(session, paste(encodedPanel, .selectByPlot, sep="_"), selected=activeDecoded)

            showNotification(sprintf("<Send to> %s", decodedPanel), type="message")
        })

        observeEvent(input[["voiceGoodBoyInput"]], {
            showNotification(HTML("<p style='font-size:300%; text-align:right;'>&#x1F357; &#x1F436;</p>"), type="message")
        })

        #######################################################################
        # Heat map section. ----
        #######################################################################

        max_plots <- nrow(pObjects$memory$heatMapPlot)
        for (id in seq_len(max_plots)) {
            local({
                mode0 <- "heatMapPlot"
                id0 <- id
                plot_name <- paste0(mode0, id0)

                # Triggering an update of the selected elements : import features
                import_button <- paste0(plot_name, "_", .heatMapImportFeatures)
                observeEvent(input[[import_button]], {
                    origin <- pObjects$memory[[mode0]][id0, .heatMapImportSource]
                    if (origin == .noSelection) {
                        return(NULL)
                    }
                    enc <- .encode_panel_name(origin)

                    incoming <- NULL
                    if (enc$Type == "rowStatTable") {
                        incoming <- input[[paste0(enc$Type, enc$ID, "_rows_all")]]
                    } else {
                        selected <- .get_selected_points(rownames(se), origin, pObjects$memory, pObjects$coordinates)
                        if (is.null(selected)) {
                            showNotification("Invalid: empty selection", type="warning")
                            return(NULL) # avoid corner case: which(NULL)
                        }
                        incoming <- which(selected)

                    }

                    limit <- 100
                    if (length(incoming) > limit) {
                        showNotification(sprintf("only the first %i features used", limit), type="warning")
                        incoming <- head(incoming, limit)
                    }

                    combined <- union(pObjects$memory[[mode0]][id0, .heatMapFeatName][[1]], incoming)
                    updateSelectizeInput(
                        session, paste0(plot_name, "_", .heatMapFeatName), choices=feature_choices,
                        server=TRUE, selected=combined)
                }, ignoreInit=TRUE)

                # Triggering an update of the selected elements : clear features, trigger replotting (caught by validate)
                clear_button <- paste0(plot_name, "_", .heatMapClearFeatures)
                observeEvent(input[[clear_button]], {
                    pObjects$memory[[mode0]][[.heatMapFeatName]][[id0]] <- integer()
                    updateSelectizeInput(session, paste0(plot_name, "_", .heatMapFeatName), choices=feature_choices,
                                         server=TRUE, selected=integer())
                    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                }, ignoreInit=TRUE)

                # Updating the import source, but this does NOT trigger replotting, as we need to press the button.
                field0 <- .heatMapImportSource
                cur_field <- paste0(plot_name, "_", field0)
                observeEvent(input[[cur_field]], {
                    matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                    if (identical(input[[cur_field]], pObjects$memory[[mode0]][id0, field0])) {
                        return(NULL)
                    }
                    pObjects$memory[[mode0]][[field0]][id0] <- matched_input
                }, ignoreInit=TRUE)

                # Defining the rendered plot, and saving the coordinates.
                # Also triggering an update to the accompanying legend plot.
                legend_field <- paste0(plot_name, "_", .heatMapLegend)
                output[[plot_name]] <- renderPlot({
                    force(rObjects[[plot_name]])
                    rObjects[[legend_field]] <- .increment_counter(isolate(rObjects[[legend_field]]))

                    p.out <- .make_heatMapPlot(id0, pObjects$memory, pObjects$coordinates, se, colormap)
                    pObjects$commands[[plot_name]] <- p.out$cmd_list
                    pObjects$coordinates[[plot_name]] <- p.out$xy # Caching the expression matrix.
                    pObjects$cached_info[[plot_name]] <- p.out$legends # Caching the legend plot for downstream use.
                    p.out$plot
                })

                # Defining the legend.
                output[[legend_field]] <- renderPlot({
                    force(rObjects[[legend_field]])
                    gg <- pObjects$cached_info[[plot_name]]
                    cowplot::plot_grid(plotlist=gg, ncol=1)
                })

                # Defining link information.
                link_field <- paste0(plot_name, "_", .panelLinkInfo)
                output[[link_field]] <- renderUI({
                    force(rObjects[[link_field]])
                    select_in <- pObjects$memory$heatMapPlot[[id0, .selectByPlot]]
                    if (select_in==.noSelection) {
                        return(NULL)
                    }
                    tagList("Receiving selection from", em(strong(select_in)), br())
                })

                # Triggering an update of the selected order.
                cluster_button <- paste0(plot_name, "_", .heatMapCluster)
                observeEvent(input[[cluster_button]], {
                    emat <- pObjects$coordinates[[plot_name]]
                    new_order <- match(.cluster_genes(emat), names(feature_choices))
                    updateSelectizeInput(
                        session, paste0(plot_name, "_", .heatMapFeatName), choices=feature_choices,
                        server=TRUE, selected=new_order)
                })
            })

            # Saving list-based values.
            for (field in c(.heatMapColData, .heatMapFeatName, .heatMapCenterScale)) {
                local({
                    id0 <- id
                    mode0 <- "heatMapPlot"
                    field0 <- field
                    plot_name <- paste0(mode0, id0)
                    cur_field <- paste0(plot_name, "_", field0)

                    observeEvent(input[[cur_field]], {
                        existing <- pObjects$memory[[mode0]][,field0][[id0]]
                        incoming <- as(input[[cur_field]], typeof(existing))
                        if (identical(incoming, existing)) {
                            return(NULL)
                        }
                        pObjects$memory[[mode0]] <- .update_list_element(pObjects$memory[[mode0]], id0, field0, incoming)
                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    }, ignoreInit=TRUE, ignoreNULL=(field0==.heatMapFeatName))

                    # ignoreNULL necessary for FeatName where updateSelectize generates a temporary NULL;
                    # this would trigger re-rendering of the plot upon re-rendering of the UI.
                })
            }

            # Saving other bits and pieces.
            for (field in c(.heatMapAssay, .heatMapLower, .heatMapUpper, .heatMapCenteredColors)) {
                local({
                    id0 <- id
                    mode0 <- "heatMapPlot"
                    field0 <- field
                    plot_name <- paste0(mode0, id0)
                    cur_field <- paste0(plot_name, "_", field0)

                    observeEvent(input[[cur_field]], {
                        matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode0]][[field0]]))
                        if (identical(input[[cur_field]], pObjects$memory[[mode0]][id0, field0])) {
                            return(NULL)
                        }
                        pObjects$memory[[mode0]][[field0]][id0] <- matched_input
                        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
                    }, ignoreInit=TRUE)
                })
            }
        }
    } # end of iSEE_server
    #nocov end

    #######################################################################
    # Launching the app.
    #######################################################################

    shinyApp(ui=iSEE_ui, server=iSEE_server)
}
