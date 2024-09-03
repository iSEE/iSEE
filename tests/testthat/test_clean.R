# This tests the cleanDataset function.
# library(testthat); library(iSEE); source("test_clean.R")

test_that("cleanDataset makes no noise for empties", {
    expect_warning(cleanDataset(SummarizedExperiment()), NA)

    expect_warning(cleanDataset(SingleCellExperiment()), NA)
})

test_that("cleanDataset works on row names", {
    se <- SummarizedExperiment(list(counts=cbind(A=1:10, B=2:11)))
    expect_warning(out <- cleanDataset(se), "rownames")
    expect_identical(rownames(out), as.character(1:10))

    rownames(se) <- rep("A", nrow(se))
    expect_warning(out <- cleanDataset(se), "rownames")
    expect_true(anyDuplicated(rownames(out))==0L)

    rownames(se) <- LETTERS[1:10]
    expect_warning(cleanDataset(se), NA)

    # Actually sets empty row names.
    out <- cleanDataset(SummarizedExperiment())
    expect_identical(rownames(out), character(0))
})

test_that("cleanDataset works on column names", {
    se <- SummarizedExperiment(list(counts=rbind(A=1:10, B=2:11)))
    expect_warning(out <- cleanDataset(se), "colnames")
    expect_identical(colnames(out), as.character(1:10))

    colnames(se) <- rep("A", ncol(se))
    expect_warning(out <- cleanDataset(se), "colnames")
    expect_true(anyDuplicated(colnames(out))==0L)

    colnames(se) <- LETTERS[1:10]
    expect_warning(cleanDataset(se), NA)

    # Actually sets empty column names.
    out <- cleanDataset(SummarizedExperiment())
    expect_identical(colnames(out), character(0))
})

test_that("cleanDataset works on colData names", {
    se <- SummarizedExperiment(colData=DataFrame(A=1:2, A=3:4, row.names=LETTERS[1:2], check.names=FALSE))
    expect_warning(out <- cleanDataset(se), "colnames(colData", fixed=TRUE)
    expect_true(anyDuplicated(colnames(colData(out)))==0L)

    colnames(colData(se)) <- letters[1:2]
    expect_warning(cleanDataset(se), NA)
})

test_that("cleanDataset works on rowData names", {
    se <- SummarizedExperiment(rowData=DataFrame(A=1:2, A=3:4, row.names=LETTERS[1:2], check.names=FALSE))
    expect_warning(out <- cleanDataset(se), "colnames(rowData", fixed=TRUE)
    expect_true(anyDuplicated(colnames(rowData(out)))==0L)

    colnames(rowData(se)) <- letters[1:2]
    expect_warning(cleanDataset(se), NA)
})

test_that("cleanDataset works on assay names", {
    se <- SummarizedExperiment(list(cbind(A=1:10, B=2:11)))
    rownames(se) <- letters[1:10]
    expect_warning(out <- cleanDataset(se), "assayNames")
    expect_identical(assayNames(out), "unnamed")

    assays(se) <- list(assay(se), assay(se))
    expect_warning(out <- cleanDataset(se), "assayNames")
    expect_identical(assayNames(out), c("unnamed", "unnamed.1"))

    assays(se) <- list(counts=assay(se), assay(se))
    expect_warning(out <- cleanDataset(se), "assayNames")
    expect_identical(assayNames(out), c("counts", "unnamed"))

    assays(se) <- list(counts=assay(se), counts=assay(se))
    expect_warning(out <- cleanDataset(se), "assayNames")
    expect_identical(assayNames(out), c("counts", "counts.1"))

    assays(se) <- list(counts=assay(se), logcounts=assay(se))
    expect_warning(out <- cleanDataset(se), NA)
})

test_that("cleanDataset works on rowRanges", {
    if (requireNamespace("GenomicRanges", quietly = TRUE)) {
        se <- SummarizedExperiment(
            rowRanges = GenomicRanges::GRanges(ranges = IRanges(start = c(1, 3, 5), 
                                                                end = c(3, 7, 18)), 
                                               strand = c("+", "-", "*"),
                                               seqnames = c("chr1", "chr1", "chr2"),
                                               annot = c("A", "B", "C")))
        expect_warning(out <- cleanDataset(se), "rowRanges")
        expect_identical(colnames(rowData(out)), c("annot", "rowRanges_start", 
                                                   "rowRanges_end", "rowRanges_seqnames",
                                                   "rowRanges_strand"))
    }
})

test_that("cleanDataset works on reducedDimNames", {
    skip("waiting for a fix to the SCE itself")
    reducedDims(sce) <- list(PCA=matrix(0,0,5), PCA=matrix(0,0,2))
    expect_warning(out <- cleanDataset(sce), "reducedDimNames")
    expect_identical(reducedDimNames(out), c("PCA", "PCA.1"))

    reducedDims(sce) <- list(PCA=matrix(0,0,5), TSNE=matrix(0,0,2))
    expect_warning(out <- cleanDataset(sce), NA)
})
