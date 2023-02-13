FROM rocker/shiny-verse

# Instalar bibliotecas para o tidyverse
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  build-essential \
  libcurl4-gnutls-dev \
  libxml2-dev \
  libssl-dev \
  r-cran-curl \
  r-cran-openssl \
  curl \
  gnupg1 \
  r-cran-xml2

RUN R -e "install.packages(pkgs=c('shiny','tidyverse','tmap','leaflet','htmltools','xgboost', 'DT', 'plotly', 'tidymodels','ggridges', 'tidytext', 'plotly', 'ggthemes','gganimate','flexdashboard','readxl','geobr','RColorBrewer','sf','spdep','broom','spatialreg','rgdal','sphet','tseries', 'splm','ggplot2','rgeoda','tseries'), repos='https://cran.rstudio.com/')"





# Instalar seu próprio app (e suas dependências)
COPY ./ /tmp/app/
RUN R -e "remotes::install_local('/inst/app/')"

# Copiar arquivos para o lugar certo
EXPOSE 3838
RUN rm /srv/shiny-server/index.html
COPY ./inst/app /srv/shiny-server/
COPY ./inst/app/shiny-server.conf /etc/shiny-server/shiny-server.conf

# Run
CMD ["/usr/bin/shiny-server.sh"]
