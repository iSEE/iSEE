FROM bioconductor/devel_base2

MAINTAINER davis@ebi.ac.uk
LABEL authors="davis@ebi.ac.uk" \
    description="Docker image containing R and packages the iSEE package"

# Install container-wide requrements gcc, pip, zlib, libssl, make, libncurses, fortran77, g++, R
RUN apt-get update && \
    apt-get -y upgrade && \
    apt-get install -y --no-install-recommends \
        curl \
        emacs \
        ess \
        gdebi-core \
        git \
        libbz2-dev \
        libcairo2 \
        libcurl4-openssl-dev \
        libgsl-dev \
        libgsl2 \
        liblzma-dev \
        libncurses5-dev \
        libpcre3-dev \
        libreadline-dev \
        libssh2-1-dev \
        libssl-dev \
        libxml2-dev \
        libzmq3-dev \
        make \
        pandoc \
        pandoc-citeproc \
        r-cran-rjava \
        software-properties-common \
        systemd \
        wget \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN add-apt-repository ppa:ubuntu-mozilla-daily/firefox-aurora  && \
    apt-get install -y --no-install-recommends \
    firefox

RUN mkdir -p  /usr/local/lib/R/site-library

ADD inst/extdata/install.R /tmp/

RUN R -f /tmp/install.R

RUN wget https://download2.rstudio.org/rstudio-server-1.1.419-amd64.deb
RUN gdebi rstudio-server-1.1.419-amd64.deb
RUN wget https://download1.rstudio.org/rstudio-xenial-1.1.419-amd64.deb
RUN gebi rstudio-xenial-1.1.419-amd64.deb

