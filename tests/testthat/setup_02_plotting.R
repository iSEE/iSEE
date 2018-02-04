
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

redDimArgs[1,"ColorBy"] <- iSEE:::.colorByColDataTitle

colDataArgs[1,"ColorBy"] <- iSEE:::.colorByColDataTitle

colDataArgs[2,"XAxis"] <- iSEE:::.colDataXAxisColData
colDataArgs[2,"XAxisColData"] <- "driver_1_s"
colDataArgs[2,"YAxis"] <- "passes_qc_checks_s"
colDataArgs[2,"ColorBy"] <- iSEE:::.colorByColDataTitle

# Set up memory ----

all_memory <- iSEE:::.setup_memory(sce, redDimArgs, colDataArgs, geneExprArgs, geneStatArgs,
                          redDimMax, colDataMax, geneExprMax, geneStatMax)


all_coordinates <- list()

################################################################################
## Reduced dimension plot 1
################################################################################

red.dim <- reducedDim(sce, 1);
plot.data <- data.frame(X = red.dim[, 1], Y = red.dim[, 2], row.names=colnames(sce));

# Defining plot limits
xbounds <- range(plot.data$X, na.rm = TRUE);
ybounds <- range(plot.data$Y, na.rm = TRUE);

# Saving data for transmission
all_coordinates[['redDimPlot1']] <- plot.data

################################################################################
## Column data plot 1
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
## Gene expression plot 1
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
