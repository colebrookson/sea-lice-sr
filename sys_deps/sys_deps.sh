#!/usr/bin/env bash
set -euo pipefail

apt-get update -qq

# system headers required by the seed packages in DESCRIPTION:
#   tidyverse / readr / httr   -> libcurl, libssl
#   xml2 (tidyverse dep)       -> libxml2
#   gert / credentials         -> libgit2
apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    libgit2-dev

apt-get clean
rm -rf /var/lib/apt/lists/*
