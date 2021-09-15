FROM ubuntu:latest

COPY . /r-source

ENV DEBIAN_FRONTEND noninteractive
ENV R_CRAN_WEB https://cran.rstudio.com
ENV CRAN_RSYNC mirrors.nic.cz::CRAN

RUN set -e &&\
    apt-get update -y &&\
    apt-get install -y gcc wget locales git rsync gfortran xvfb autoconf pkg-config texinfo texlive-latex-extra texlive-fonts-recommended tk8.6-dev \
    libcurl4-openssl-dev libblas-dev libbz2-dev libicu-dev libjpeg-dev liblapack-dev liblzma-dev libncurses5-dev libpcre2-dev libpng-dev libreadline-dev libxt-dev \
    language-pack-en-base 

ENV LANG en_US.UTF-8

WORKDIR /r-source

RUN ./configure --enable-R-shlib --with-blas --with-lapack --disable-java --without-recommended-packages

RUN make

RUN xvfb-run make check-all

RUN tail -n100 tests/*.fail || true
