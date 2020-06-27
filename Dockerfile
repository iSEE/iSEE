FROM bioconductor/bioconductor_docker:devel

MAINTAINER kevinrue67@gmail.com
LABEL authors="kevinrue67@gmail.com" \
    description="Docker image containing the iSEE package in a bioconductor/bioconductor_docker:devel container."

WORKDIR /home/rstudio/isee

COPY --chown=rstudio:rstudio . /home/rstudio/isee

RUN apt-get update && apt-get install -y libglpk-dev && apt-get clean && rm -rf /var/lib/apt/lists/*

ENV R_REMOTES_NO_ERRORS_FROM_WARNINGS=true

RUN Rscript -e "install.packages('Rtsne')"
RUN Rscript -e "devtools::install('.', dependencies=TRUE, repos = BiocManager::repositories(), build_vignettes = TRUE)"
