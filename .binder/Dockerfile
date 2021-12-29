FROM rocker/binder:3.6.0
LABEL maintainer='kca2'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/kca2/binder_map/raw/main/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-09-28/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
