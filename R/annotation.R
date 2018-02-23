#' Retrieve info on a gene
#' 
#' Generate HTML content with more information about a selected gene.
#'
#' @param annot.orgdb The OrgDb object
#' @param annot.keytype The keytype that matches the keys used
#' @param annot.keyfield The field to use as column from the \code{gene_data}
#' @param gene_data The data frame containing info on the genes/features of interest,
#' such as the one generated via \code{as.data.frame(rowData(se))}
#' @param chosen_gene The selected gene
#' 
#' @details This function relies on the code{entrez_summary} function, which 
#' requires internet access to retrieve updated information. Conversion of the 
#' ids is handled via OrgDb packages
#'
#' @return HTML content with more information about a selected gene
#' @author Federico Marini
#' @rdname INTERNAL_generate_annotation
#' @importFrom AnnotationDbi mapIds
#' @importFrom rentrez entrez_summary
#' @importFrom shiny HTML
.generate_annotation <- function(annot.orgdb, annot.keytype, annot.keyfield, gene_data, chosen_gene)
{
  if (is.null(annot.orgdb)) {
    return(HTML(""))
  }

  shiny::validate(
    need(!is.null(chosen_gene), "Select a gene from the table")
  )

  if (is.null(annot.keyfield)) {
    selectedGene <- rownames(gene_data)[chosen_gene]
  } else {
    selectedGene <- gene_data[chosen_gene,annot.keyfield]
  }

  if (annot.keytype!="ENTREZID") {
    selgene_entrez <- mapIds(annot.orgdb, selectedGene, "ENTREZID", annot.keytype)
  } else {
    selgene_entrez <- selectedGene
  }

  fullinfo <- entrez_summary("gene", selgene_entrez)
  link_pubmed <- paste0('<a href="http://www.ncbi.nlm.nih.gov/gene/?term=',
                        selgene_entrez,
                        '" target="_blank" >Click here to see more at NCBI</a>')

  if(fullinfo$summary == "") {
    return(HTML(paste0("<b>",fullinfo$name, "</b><br/><br/>",
                       fullinfo$description,"<br/><br/>",
                       link_pubmed
    )))
  } else {
    return(HTML(paste0("<b>",fullinfo$name, "</b><br/><br/>",
                       fullinfo$description, "<br/><br/>",
                       fullinfo$summary, "<br/><br/>",
                       link_pubmed
    )))
  }
}


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
      selgene_entrez <- mapIds(eval(parse(text=annopkg)), selectedGene, "ENTREZID", keytype)
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
      selgene_ensembl <- mapIds(eval(parse(text=annopkg)), selectedGene, "ENSEMBL", keytype)
    } else {
      selgene_ensembl <- selectedGene
    }
    
    link_ensembl <- paste0('<a href="http://www.ensembl.org/',species,'/Gene/Summary?g=',selgene_ensembl,
                           '" target="_blank"> ',
                           'Click here to browse more about this gene in the ENSEMBL database</a>')
    
    mycontent <- paste0("<b>",selectedGene, "</b><br/><br/>ENSEMBL id:",
                        selgene_ensembl,"<br/><br/>",
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












