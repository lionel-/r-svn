FROM ubuntu:latest

COPY . /r-source
WORKDIR /r-source

ENV DEBIAN_FRONTEND noninteractive
ENV R_CRAN_WEB https://cran.rstudio.com
ENV CRAN_RSYNC mirrors.nic.cz::CRAN

RUN apt-get update -y &&\
    apt-get install -y gcc wget locales git rsync gfortran xvfb pkg-config texinfo tk8.6-dev \
    libcurl4-openssl-dev libblas-dev libbz2-dev libicu-dev libjpeg-dev liblapack-dev liblzma-dev libncurses5-dev libpcre2-dev libpng-dev libreadline-dev libxt-dev \
    language-pack-en-base 

ENV LANG en_US.UTF-8

RUN \
    sed -i.bak 's|$(GIT) svn info|./.github/workflows/svn-info.sh|' Makefile.in &&\
    sed -i.bak 's|timeout = 30|timeout = 90|' tests/reg-packages.R &&\
    ./.github/workflows/svn-info.sh &&\
    ./configure --enable-R-shlib --with-blas --with-lapack --disable-java --without-recommended-packages

RUN make --jobs=2

RUN \
    PDFLATEX="${PWD}/.github/workflows/dummy" xvfb-run make check-all || CHECK_FAIL=1 &&\
    find tests -name '*.fail' | while read f; do echo " ===== ERROR LOG: $f ====="; tail -n100 $f; echo " ===== END OF: $f ====="; done &&\
    exit $CHECK_FAIL
