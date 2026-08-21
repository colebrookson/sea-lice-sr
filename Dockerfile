# rocker/r-ver pinned to match Depends in DESCRIPTION
FROM rocker/r-ver:4.5.3

# ---------------------------------------------------------------------------
# R config
# ---------------------------------------------------------------------------
ENV PAK_PKG_TYPE=binary \
    CRAN_MIRROR=https://cloud.r-project.org \
    MAKEFLAGS="-j1"

RUN echo "options(repos = c(CRAN = Sys.getenv('CRAN_MIRROR')))" \
    >> /usr/local/lib/R/etc/Rprofile.site

# ---------------------------------------------------------------------------
# System dependencies 
# ---------------------------------------------------------------------------
COPY sys_deps/sys_deps.sh /tmp/sys_deps.sh
RUN bash /tmp/sys_deps.sh && rm /tmp/sys_deps.sh

# ---------------------------------------------------------------------------
# git + ssh because we want to be able to commit from inside the container!
# ---------------------------------------------------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    openssh-client \
    && rm -rf /var/lib/apt/lists/*

# ---------------------------------------------------------------------------
# R packages
# ---------------------------------------------------------------------------
RUN install2.r --error --skipinstalled pak

WORKDIR /home/rproject
COPY DESCRIPTION /home/rproject/

RUN R -q -e "pak::meta_update()" \
    && R -q -e "pak::local_install_deps('.', ask = FALSE, upgrade = FALSE)"

 
# ---------------------------------------------------------------------------
# CmdStan — layer so caches independently
#
# Pin the version bc sum_to_zero_vector requires >= 2.36
# ---------------------------------------------------------------------------
ENV CMDSTAN=/opt/cmdstan
 
RUN MAKEFLAGS="" R -q -e "cmdstanr::install_cmdstan( \
      dir = '/opt', \
      cores = 4, \
      overwrite = FALSE)" \
    && R -q -e "cmdstanr::set_cmdstan_path(Sys.getenv('CMDSTAN')); \
                cat('CmdStan', cmdstanr::cmdstan_version(), '\n')"


# ---------------------------------------------------------------------------
# arf + languageserver + httpgd because gotta have a nice R interface!
# ---------------------------------------------------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends curl \
    && curl --proto '=https' --tlsv1.2 -LsSf \
    https://github.com/eitsupi/arf/releases/latest/download/arf-console-installer.sh | sh \
    && ln -s /root/.cargo/bin/arf /usr/local/bin/arf \
    && rm -rf /var/lib/apt/lists/*

RUN R -q -e "pak::pkg_install(c('languageserver', 'httpgd'))"

RUN mkdir -p /root/.config/arf
COPY arf.toml /root/.config/arf/arf.toml

# ---------------------------------------------------------------------------
# Copy project files since these change all the time
# ---------------------------------------------------------------------------
COPY . /home/rproject

# ---------------------------------------------------------------------------
# zsh + oh my zsh + powerlevel10k bc you know i always need a pretty terminal!!
# ---------------------------------------------------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
    zsh \
    && rm -rf /var/lib/apt/lists/*

RUN git clone --depth 1 https://github.com/ohmyzsh/ohmyzsh.git /root/.oh-my-zsh && \
    git clone --depth 1 https://github.com/romkatv/powerlevel10k.git \
    /root/.oh-my-zsh/custom/themes/powerlevel10k

ENV SHELL=/usr/bin/zsh

# .zshrc 
COPY .zshrc /root/.zshrc