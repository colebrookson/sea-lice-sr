#!/usr/bin/env bash
set -euo pipefail

apt-get update -qq

# system headers required by the seed packages in DESCRIPTION:
#   tidyverse / readr / httr   -> libcurl, libssl
#   xml2 (tidyverse dep)       -> libxml2
#   gert / credentials         -> libgit2
apt-get install -y --no-install-recommends \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev

# add project-specific system deps below this line
# e.g. for sf:
#   libgdal-dev gdal-bin libgeos-dev libproj-dev libsqlite3-dev libudunits2-dev
# e.g. for Stan/rstanarm:
#   (g++ and gfortran are covered by build-essential)
#   liblapack-dev libblas-dev

apt-get clean
rm -rf /var/lib/apt/lists/*
