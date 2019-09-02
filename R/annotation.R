#' Annotation via Entrez database
#'
#' Annotation facility for displaying additional information on selected genes,
#' based on the data retrieved from the ENTREZ database
#'
#' @param se An object that is coercible to \linkS4class{SingleCellExperiment}.
#' @param orgdb An OrgDb object, as a basis for the annotation.
#' Typical values are \code{org.Hs.eg.db} for human, \code{org.Mm.eg.db} for mouse, and so on.
#' The corresponding package has to be available and loaded before calling this function.
#' @param keytype The keytype that matches the IDs used in the \code{se} object.
#' Can be one of \code{keytypes(org.XX.eg.db)}, typically \code{"SYMBOL"}, \code{"ENSEMBL"}, or \code{"ENTREZID"}.
#' @param rowdata_col A character string specifying which column of \code{rowData(se)} contains the keys.
#' Defaults to NULL, which corresponds to having the ids of the features as row names of the \code{se} object itself.
#'
#' @return A function to be used as the value of the \code{annotFun} parameter of \code{iSEE}.
#' This function itself returns a \code{HTML} tag object with the content extracted from the call,
#' accepting as parameters the \code{se} object and the \code{row_index} corresponding to the feature of interest.
#'
#' @export
#' @importFrom AnnotationDbi mapIds
#' @importFrom rentrez entrez_summary
#' @importFrom shiny HTML
#' @importFrom methods is
#'
#' @examples
#' library(scRNAseq)
#' sce <- ReprocessedAllenData(assays="tophat_counts")
#'
#' library(org.Mm.eg.db)
#' myfun <- annotateEntrez(sce, org.Mm.eg.db, keytype="SYMBOL")
#' \dontrun{
#' # Requires a working internet connection
#' myfun(sce, 4242)
#' }
#'
#' # to be used when launching the app itself ----
#'
#' app <- iSEE(sce, annotFun = myfun)
#' if (interactive()) {
#'   shiny::runApp(app, port = 1234)
#' }
annotateEntrez <- function(se, orgdb, keytype, rowdata_col = NULL) {
    function(se, row_index) {
        # no orgdb provided -> nothing
        if (is.null(orgdb)) {
            return(HTML(""))
        }
        #nocov start
        # if no column provided, implicitly assume the names are in the rownames of the object itself
        if (is.null(rowdata_col)) {
            selectedGene <- rownames(se)[row_index]
        } else {
            selectedGene <- rowData(se)[row_index,rowdata_col]
        }

        if (keytype!="ENTREZID") {
            e <- try(selgene_entrez <- mapIds(orgdb, selectedGene, "ENTREZID", keytype),silent = TRUE)
            validate(need(!is(e, "try-error"), sprintf("could not convert %s to ENTREZ id",selectedGene)))
        } else {
            selgene_entrez <- selectedGene
        }

        fullinfo <- entrez_summary("gene", selgene_entrez)
        link_pubmed <- paste0('<a href="http://www.ncbi.nlm.nih.gov/gene/?term=',
            selgene_entrez,
            '" target="_blank">Click here to see more at the NCBI database</a>')

        mycontent <- paste0("<b>",fullinfo$name, "</b><br/><br/>",
            fullinfo$description,"<br/><br/>",
            ifelse(fullinfo$summary == "","",paste0(fullinfo$summary, "<br/><br/>")),
            link_pubmed)

        return(HTML(mycontent))
        #nocov end
    }
}

#' Annotation via ENSEMBL database
#'
#' Annotation facility for displaying additional information on selected genes,
#' based on the data retrieved from the ENSEMBL database
#'
#' @param se An object that is coercible to \linkS4class{SingleCellExperiment}.
#' @param orgdb An OrgDb object, as a basis for the annotation.
#' Typical values are \code{org.Hs.eg.db} for human, \code{org.Mm.eg.db} for mouse, and so on.
#' The corresponding package has to be available and loaded before calling this function
#' @param keytype The keytype that matches the IDs used in the \code{se} object.
#' Can be one of the values of \code{keytypes(org.XX.eg.db)}, typically being \code{"SYMBOL"}, \code{"ENSEMBL"}, or \code{"ENTREZID"}.
#' @param rowdata_col A character string specifying which column of \code{rowData(se)} contains the keys.
#' Defaults to NULL, which corresponds to having the ids of the features as row names of the \code{se} object itself.
#' @param ens_species Character string containing the species name, coded as in the ENSEMBL database and browser.
#' For example, \code{"Homo_sapiens"} for human and \code{"Mus_musculus"}.
#' Defaults to \code{species(orgdb)}, with the whitespace replaced by underscore
#'
#' @return A function to be used as the value of the \code{annotFun} parameter of \code{iSEE}.
#' This function itself returns a \code{HTML} tag object with the content extracted from the call,
#' accepting as parameters the \code{se} object and the \code{row_index} corresponding to the feature of interest.
#'
#' @export
#' @importFrom AnnotationDbi mapIds species
#' @importFrom shiny HTML
#' @importFrom methods is
#'
#' @examples
#' library(scRNAseq)
#' sce <- ReprocessedAllenData(assays="tophat_counts")
#' library(org.Mm.eg.db)
#' myfun <- annotateEnsembl(sce, org.Mm.eg.db, keytype="SYMBOL")
#' \dontrun{
#' # Requires a working internet connection
#' myfun(sce, 4242)
#' }
#'
#' # to be used when launching the app itself ----
#'
#' app <- iSEE(sce, annotFun = myfun)
#' if (interactive()) {
#'   shiny::runApp(app, port = 1234)
#' }
annotateEnsembl <- function(se, orgdb, keytype, rowdata_col = NULL, ens_species = gsub(" ","_", species(orgdb))) {
    function(se, row_index) {
        # no species provided -> nothing
        if (is.null(ens_species)) {
            return(HTML(""))
        }
        #nocov start
        # if no column provided, implicitly assume the names are in the rownames of the object itself
        if (is.null(rowdata_col)) {
            selectedGene <- rownames(se)[row_index]
        } else {
            selectedGene <- rowData(se)[row_index,rowdata_col]
        }

        if (keytype!="ENSEMBL") {
            e <- try(selgene_ensembl <- mapIds(orgdb, selectedGene, "ENSEMBL", keytype),silent = TRUE)
            validate(need(!is(e, "try-error"), sprintf("could not convert %s to ENSEMBL id",selectedGene)))
        } else {
            selgene_ensembl <- selectedGene
        }

        link_ensembl <- paste0('<a href="http://www.ensembl.org/',ens_species,'/Gene/Summary?g=',selgene_ensembl,
            '" target="_blank"> ',
            'Click here to browse more about this gene in the ENSEMBL database</a>')

        mycontent <- paste0("<b>",selectedGene, "</b><br/><br/>ENSEMBL id: <b>",
            selgene_ensembl,"</b><br/><br/>",
            link_ensembl)

        return(HTML(mycontent))
        #nocov end
    }
}
