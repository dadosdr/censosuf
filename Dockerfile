FROM rocker/verse:4.2.3
RUN apt-get update && apt-get install -y  gdal-bin libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libicu-dev libpng-dev libproj-dev libssl-dev libudunits2-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.6")'
RUN Rscript -e 'remotes::install_version("RColorBrewer",upgrade="never", version = "1.1-3")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "1.0-7")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.1.1")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.5")'
RUN Rscript -e 'remotes::install_version("spdep",upgrade="never", version = "1.2-4")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.9")'
RUN Rscript -e 'remotes::install_version("spatialreg",upgrade="never", version = "1.2-3")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.4")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.4")'
RUN Rscript -e 'remotes::install_version("tseries",upgrade="never", version = "0.10-51")'
RUN Rscript -e 'remotes::install_version("tmap",upgrade="never", version = "3.3-3")'
RUN Rscript -e 'remotes::install_version("splm",upgrade="never", version = "1.5-3")'
RUN Rscript -e 'remotes::install_version("sphet",upgrade="never", version = "2.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("rgeoda",upgrade="never", version = "0.0.9")'
RUN Rscript -e 'remotes::install_version("rgdal",upgrade="never", version = "1.5-32")'
RUN Rscript -e 'remotes::install_version("readxl",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.10.0")'
RUN Rscript -e 'remotes::install_version("ggthemes",upgrade="never", version = "4.2.4")'
RUN Rscript -e 'remotes::install_version("gganimate",upgrade="never", version = "1.0.7")'
RUN Rscript -e 'remotes::install_version("geobr",upgrade="never", version = "1.7.0")'
RUN Rscript -e 'remotes::install_version("flexdashboard",upgrade="never", version = "0.5.2")'
RUN Rscript -e 'remotes::install_version("broom",upgrade="never", version = "0.8.0")'
RUN Rscript -e 'remotes::install_github("Thinkr-open/golem@7a9debb1fab42368ee3b50803483416180508c6f")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');censosuf::run_app()"
