
# Set up plotting arguments ----

redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 2)
geneExprArgs <- geneExprPlotDefaults(sce, 1)
geneStatArgs <- geneStatTableDefaults(sce, 1)
redDimMax <- 1 
colDataMax <- 1
geneExprMax <- 1
geneStatMax <- 1

# Change a few default settings ----

redDimArgs[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle

colDataArgs[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle

colDataArgs[2,iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
colDataArgs[2,iSEE:::.colDataXAxisColData] <- "driver_1_s" # dynamic value?
colDataArgs[2,iSEE:::.colDataYAxis] <- "passes_qc_checks_s" # dynamic value?
colDataArgs[2,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle

geneExprArgs[1,iSEE:::.geneExprYAxisGeneTable] <- "Gene statistics table 1" # dynamic value?
geneExprArgs[1,iSEE:::.geneExprYAxisGeneTable] <- "Gene statistics table 1" # dynamic value?
geneExprArgs[1,iSEE:::.colorByField] <- iSEE:::.colorByGeneTableTitle
geneExprArgs[1,iSEE:::.colorByGeneTable] <- "Gene statistics table 1" # dynamic value?

# Set up memory ----

all_memory <- iSEE:::.setup_memory(sce, redDimArgs, colDataArgs, geneExprArgs, geneStatArgs,
                          redDimMax, colDataMax, geneExprMax, geneStatMax)


# Fill in all_coordinates form a copy-paste of an allen demo session ----

all_coordinates <- list()

################################################################################
## Reduced dimension plot 1 ----
################################################################################

red.dim <- reducedDim(sce, 1);
plot.data <- data.frame(X = red.dim[, 1], Y = red.dim[, 2], row.names=colnames(sce));

# Defining plot limits
xbounds <- range(plot.data$X, na.rm = TRUE);
ybounds <- range(plot.data$Y, na.rm = TRUE);

# Saving data for transmission
all_coordinates[['redDimPlot1']] <- plot.data

################################################################################
## Column data plot 1 ----
################################################################################

plot.data <- data.frame(Y = colData(sce)[,"NREADS"], row.names=colnames(sce));
plot.data$X <- factor(character(ncol(sce)))
plot.data$X <- as.factor(plot.data$X);
plot.data <- subset(plot.data, !is.na(X) & !is.na(Y));

# Defining plot limits
ybounds <- range(plot.data$Y, na.rm = TRUE);

# Saving data for transmission
all_coordinates[['colDataPlot1']] <- plot.data

# Setting up plot aesthetics
plot.data$GroupBy <- plot.data$X;
set.seed(100);
plot.data$jitteredX <- vipor::offsetX(plot.data$Y,
    x=plot.data$X, width=0.4, varwidth=FALSE, adjust=1,
    method='quasirandom', nbins=NULL) + as.integer(plot.data$X);

################################################################################
## Gene expression plot 1 ----
################################################################################

plot.data <- data.frame(Y=assay(sce, 6)[1L,], row.names = colnames(sce))
plot.data$X <- factor(character(ncol(sce)))
plot.data$X <- as.factor(plot.data$X);
plot.data <- subset(plot.data, !is.na(X) & !is.na(Y));

# Defining plot limits
ybounds <- range(plot.data$Y, na.rm = TRUE);

# Saving data for transmission
all_coordinates[['geneExprPlot1']] <- plot.data

# Setting up plot aesthetics
plot.data$GroupBy <- plot.data$X;
set.seed(100);
plot.data$jitteredX <- vipor::offsetX(plot.data$Y,
    x=plot.data$X, width=0.4, varwidth=FALSE, adjust=1,
    method='quasirandom', nbins=NULL) + as.integer(plot.data$X)

# Add a non-standard setting to the plotting arguments ----

redDimArgsExtraField <- redDimArgs
redDimArgsExtraField$DummyExtraField <- NA_character_
