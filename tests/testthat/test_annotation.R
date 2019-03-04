context("annotation")

# This tests the dynamic annotation functions.
# library(testthat); library(iSEE); source("setup_sce.R"); source("test_annotation.R")

test_that("Entrez annotation function is functional", {
    library(org.Mm.eg.db)
    myfun <- annotateEntrez(sce, org.Mm.eg.db, keytype="SYMBOL")
    expect_is(myfun, 'function')

    # Trying with an actual Entrez key.
    rowData(sce)$Entrez <- mapIds(org.Mm.eg.db, keytype="SYMBOL", keys=rownames(sce), column="ENTREZID")
    myfun <- annotateEntrez(sce, org.Mm.eg.db, keytype="ENTREZID", rowdata_col="Entrez")
    expect_is(myfun, 'function')

    # Do not try running 'myfun'; requires internet connectivity.
})

test_that("Ensembl annotation function is functional", {
    library(org.Mm.eg.db)
    myfun <- annotateEnsembl(sce, org.Mm.eg.db, keytype="SYMBOL")
    expect_is(myfun, 'function')

    out <- myfun(sce, which(rownames(sce)=="Nanog"))
    expect_match(out, "Nanog")

    sce2 <- sce
    rownames(sce2)[1] <- "WHEE"
    expect_error(myfun(sce2, 1L), "could not convert WHEE to ENSEMBL id")

    # Trying with an actual Ensembl key.
    rowData(sce)$Ensembl <- mapIds(org.Mm.eg.db, keytype="SYMBOL", keys=rownames(sce), column="ENSEMBL")
    myfun <- annotateEntrez(sce, org.Mm.eg.db, keytype="ENSEMBL", rowdata_col="Ensembl")
    expect_is(myfun, 'function')

    # Do not try running 'myfun'; requires internet connectivity.
})

test_that("annotateEntrez return an empty element in the absence of annotation package", {

    out <- annotateEntrez(orgdb=NULL)()

    expect_identical(out, structure("", html = TRUE, class = c("html", "character")))

})

test_that("annotateEntrez return an empty element in the absence of annotation package", {

    out <- annotateEnsembl(ens_species=NULL)()

    expect_identical(out, structure("", html = TRUE, class = c("html", "character")))

})
