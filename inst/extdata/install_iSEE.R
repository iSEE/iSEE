if (!require("BiocManager"))
    install.packages("BiocManager")
devtools::install(build_vignettes=TRUE)
BiocManager::install("org.Hs.eg.db") # required for the PBMC4k demo
BiocManager::install("HDF5Array") # required for HDF5-backed assays
devtools::install_github("Bioconductor/DelayedArray") # version 0.7.44 required
