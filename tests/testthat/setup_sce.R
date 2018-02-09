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

rowData(sce)$num_cells <- rowSums(counts(sce)>0)
rowData(sce)$mean_count <- rowMeans(counts(sce))
