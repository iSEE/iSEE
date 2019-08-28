stopifnot(
  require(scRNAseq),
  require(scater)
)

# Example data ----
sce <- ReprocessedAllenData(assays = "tophat_counts")

sce <- logNormCounts(sce, exprs_values="tophat_counts")
sce <- runPCA(sce)
sce <- runTSNE(sce)

rowData(sce)$num_cells <- rowSums(assay(sce, "tophat_counts") > 0)
rowData(sce)$mean_count <- rowMeans(assay(sce, "tophat_counts"))
# Add a groupable field in rowData
rowData(sce)$letters <- sample(letters[1:3], nrow(sce), TRUE)

sizeFactors(sce)  <- runif(ncol(sce))

colData(sce)[["nested"]] <- DataFrame(
        nested1 = runif(ncol(sce)),
        nested2 = sample(letters, ncol(sce), TRUE), row.names = colnames(sce)
    )
