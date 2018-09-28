FROM bioconductor/devel_base2

MAINTAINER kevin.rue-albrecht@kennedy.ox.ac.uk
LABEL authors="kevin.rue-albrecht@kennedy.ox.ac.uk" \
    description="Docker image containing the iSEE package in a bioconductor/devel_base2 container."

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
ADD . /app

# Install iSEE
RUN Rscript /app/inst/extdata/install_iSEE.R

CMD R
