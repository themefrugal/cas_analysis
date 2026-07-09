FROM rocker/r-ver:4.5.3

ENV RENV_CONFIG_SANDBOX_ENABLED=false \
    RENV_PATHS_LIBRARY=/opt/renv/library \
    SHINY_APP_DIR=/srv/cas/app

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    fonts-dejavu-core \
    g++ \
    gfortran \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libpoppler-cpp-dev \
    libsqlite3-dev \
    libssl-dev \
    libtiff5-dev \
    libxml2-dev \
    make \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/cas/app

COPY app/renv.lock ./renv.lock

RUN R -q -e "install.packages('renv', repos = 'https://cloud.r-project.org'); renv::restore(lockfile = 'renv.lock', prompt = FALSE)"

COPY app ./

EXPOSE 10000

CMD ["Rscript", "start_render.R"]
