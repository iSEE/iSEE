FROM bioconductor/devel_base2

MAINTAINER kevin.rue-albrecht@kennedy.ox.ac.uk
LABEL authors="kevin.rue-albrecht@kennedy.ox.ac.uk" \
    description="Docker image containing the iSEE package in a bioconductor/devel_base2 container."

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
ADD . /app

# Install iSEE and dependencies
RUN Rscript -e "BiocManager::install('iSEE', version = 'devel')"

# Add additional dependencies for the GitHub version
RUN Rscript -e "BiocManager::install(c('shinyWidgets', 'ComplexHeatmap', 'circlize'), version = 'devel')"

# Reinstall the latest iSEE from GitHub branch master.
WORKDIR /isee
RUN git clone https://github.com/csoneson/iSEE.git
RUN R CMD INSTALL iSEE

CMD R
