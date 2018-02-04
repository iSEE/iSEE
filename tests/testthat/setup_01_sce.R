stopifnot(
  require(scRNAseq),
  require(scater)
)
data(allen)

# Example data ----

sce <- as(allen, "SingleCellExperiment")
counts(sce) <- assay(sce, "tophat_counts")
sce <- normalize(sce)
sce <- runPCA(sce)
sce <- runTSNE(sce)

# SCE without a logcount assay ----

sce_noLogCount <- sce
assays(sce_noLogCount) <- assays(sce_noLogCount)["counts"]
