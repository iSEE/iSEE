
#' Annotation via Entrez database
#' 
#' Annotation facility for displaying additional information on selected genes, based on
#' the data retrieved from the ENTREZ database 
#' 
#' @param se An object that is coercible to \code{\linkS4class{SingleCellExperiment}}.
#' @param species A character string, indicating which species the samples are stemming from. See 
#' details for the format expected, and the possible values 
#' @param keytype The keytype that matches the IDs used in the \code{se} object. Can be one of 
#' the values of \code{keytypes(org.XX.eg.db)}, typically being "SYMBOL", "ENSEMBL", or "ENTREZID" 
#' @param rowdata_col A character string, corresponding to one of the columns (if present) in
#' \code{rowData(se)}. Defaults to NULL, which corresponds to having the ids of the features
#' as row names of the \code{se} object itself. 
#' 
#' @details The \code{species} parameter has to be one of the following: Anopheles, Arabidopsis,
#' Bovine, Canine, Chicken, Chimp, E coli strain K12, E coli strain Sakai, Fly, Human, Malaria,                
#' Mouse, Pig, Rat, Rhesus, Streptomyces coelicolor, Toxoplasma gondii, Worm, Xenopus, Yeast, Zebrafish.
#'              
#' Since the functionality depends on the corresponding org.XX.eg.db packages, these
#' are expected to be installed, and get loaded when running the function  
#'
#' @return A function to be used as the value of the \code{annotFun} parameter of \code{iSEE}. 
#' This function itself returns a \code{HTML} tag object with the content extracted from the call,
#' with parameteres the \code{se} object, and the \code{row_index} corresponding to the feature
#' of interest.
#' 
#' @export
#'
#' @examples
#' library(scRNAseq)
#' data(allen)
#' sce <- as(allen, "SingleCellExperiment")
#' annotateEntrez(sce,"Mouse","SYMBOL")
#' 
#' # to be used when launching the app itself ----
#'
#' app <- iSEE(sce)
#' if (interactive()) {
#'   shiny::runApp(app, port = 1234, annotFun = annotateEntrez(sce,"Mouse","SYMBOL"))
#' }
annotateEntrez <- function(
  se,
  species,
  keytype,
  rowdata_col = NULL) {
  
  function(se, row_index) {
    # no species provided -> nothing
    if (is.null(species)) {
      return(HTML(""))
    }
    if (species %in% annoSpecies_df$species)
      stop("Please pro")
    annopkg <- annoSpecies_df$pkg[annoSpecies_df$species==species]
    if(!require(annopkg,character.only=TRUE))
      stop(paste0("The package ",annopkg, " is not installed/available. ",
                  "Try installing it with biocLite('",annopkg,"')"))
    
    # if no column provided, implicitly assume the names are in the rownames of the object itself
    if (is.null(rowdata_col)) {
      selectedGene <- rownames(se)[row_index]
    } else {
      selectedGene <- rowData(se)[row_index,rowdata_col]
    }
    
    if (keytype!="ENTREZID") {
      e <- try(selgene_entrez <- mapIds(eval(parse(text=annopkg)), selectedGene, "ENTREZID", keytype),silent = TRUE)
      if(class(e) == "try-error")
        return(HTML("Sorry, could not convert this gene to ENTREZ id"))
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
  }
}

#' Annotation via ENSEMBL database
#' 
#' Annotation facility for displaying additional information on selected genes, based on
#' the data retrieved from the ENSEMBL database 
#' 
#' @param se An object that is coercible to \code{\linkS4class{SingleCellExperiment}}.
#' @param species A character string, indicating which species the samples are stemming from. See 
#' details for the format expected, and the possible values 
#' @param keytype The keytype that matches the IDs used in the \code{se} object. Can be one of 
#' the values of \code{keytypes(org.XX.eg.db)}, typically being "SYMBOL", "ENSEMBL", or "ENTREZID" 
#' @param rowdata_col A character string, corresponding to one of the columns (if present) in
#' \code{rowData(se)}. Defaults to NULL, which corresponds to having the ids of the features
#' as row names of the \code{se} object itself. 
#' 
#' @details The \code{species} parameter has to be one of the following: Anopheles, Arabidopsis,
#' Bovine, Canine, Chicken, Chimp, E coli strain K12, E coli strain Sakai, Fly, Human, Malaria,                
#' Mouse, Pig, Rat, Rhesus, Streptomyces coelicolor, Toxoplasma gondii, Worm, Xenopus, Yeast, Zebrafish.
#'              
#' Since the functionality depends on the corresponding org.XX.eg.db packages, these
#' are expected to be installed, and get loaded when running the function  
#' 
#' @return A function to be used as the value of the \code{annotFun} parameter of \code{iSEE}. 
#' This function itself returns a \code{HTML} tag object with the content extracted from the call,
#' with parameteres the \code{se} object, and the \code{row_index} corresponding to the feature
#' of interest.
#' 
#' @export
#'
#' @examples
#' library(scRNAseq)
#' data(allen)
#' sce <- as(allen, "SingleCellExperiment")
#' annotateEnsembl(sce,"Mouse","SYMBOL")
#' 
#' # to be used when launching the app itself ----
#'
#' app <- iSEE(sce)
#' if (interactive()) {
#'   shiny::runApp(app, port = 1234, annotFun = annotateEnsembl(sce,"Mouse","SYMBOL"))
#' }
annotateEnsembl <- function(
  se,
  species,
  keytype, # will be converted to be/become ENSEMBL
  rowdata_col = NULL) {
  
  function(se, row_index) {
    # no species provided -> nothing
    if (is.null(species)) {
      return(HTML(""))
    }
    annopkg <- annoSpecies_df$pkg[annoSpecies_df$species==species]
    if(!require(annopkg,character.only=TRUE))
      stop(paste0("The package ",annopkg, " is not installed/available. ",
                  "Try installing it with biocLite('",annopkg,"')"))
    
    # if no column provided, implicitly assume the names are in the rownames of the object itself
    if (is.null(rowdata_col)) {
      selectedGene <- rownames(se)[row_index]
    } else {
      selectedGene <- rowData(se)[row_index,rowdata_col]
    }
    
    if (keytype!="ENSEMBL") {
      e <- try(selgene_ensembl <- mapIds(eval(parse(text=annopkg)), selectedGene, "ENSEMBL", keytype),silent = TRUE)
      if(class(e) == "try-error")
        return(HTML("Sorry, could not convert this gene to ENSEMBL id"))
    } else {
      selgene_ensembl <- selectedGene
    }
    
    link_ensembl <- paste0('<a href="http://www.ensembl.org/',species,'/Gene/Summary?g=',selgene_ensembl,
                           '" target="_blank"> ',
                           'Click here to browse more about this gene in the ENSEMBL database</a>')
    
    mycontent <- paste0("<b>",selectedGene, "</b><br/><br/>ENSEMBL id: <b>",
                        selgene_ensembl,"</b><br/><br/>",
                        link_ensembl)
    
    return(HTML(mycontent))
  }
}


# for retrieving the annotation & linking to ensembl? + linking to UCSC afterwards?
annoSpecies_df <- data.frame(species=c("","Anopheles","Arabidopsis","Bovine","Worm",
                                       "Canine","Fly","Zebrafish","E coli strain K12",
                                       "E coli strain Sakai","Chicken","Human","Mouse",
                                       "Rhesus","Malaria","Chimp","Rat",
                                       "Yeast","Streptomyces coelicolor", "Pig","Toxoplasma gondii",
                                       "Xenopus"),
                             pkg=c("","org.Ag.eg.db", "org.At.tair.db", "org.Bt.eg.db", "org.Ce.eg.db",
                                   "org.Cf.eg.db", "org.Dm.eg.db", "org.Dr.eg.db", "org.EcK12.eg.db",
                                   "org.EcSakai.eg.db", "org.Gg.eg.db", "org.Hs.eg.db", "org.Mm.eg.db",
                                   "org.Mmu.eg.db", "org.Pf.plasmo.db", "org.Pt.eg.db", "org.Rn.eg.db",
                                   "org.Sc.sgd.db", "org.Sco.eg.db", "org.Ss.eg.db", "org.Tgondii.eg.db",
                                   "org.Xl.eg.db"),
                             stringsAsFactors = FALSE)
annoSpecies_df <- annoSpecies_df[order(annoSpecies_df$species),]
annoSpecies_df$ensembl_db <- c("","","","Bos_taurus","Canis_familiaris","Gallus_gallus","Pan_troglodytes",
                               "","","Drosophila_melanogaster","Homo_sapiens","","Mus_musculus",
                               "Sus_scrofa","Rattus_norvegicus","Macaca_mulatta","","","Caenorhabditis_elegans",
                               "Xenopus_tropicalis","Saccharomyces_cerevisiae","Danio_rerio")
rownames(annoSpecies_df) <- annoSpecies_df$species # easier to access afterwards

