FROM snoyberg/stackage:nightly

ENV REPO=https://github.com/ghc/ghc.git

ENV WORKDIR=/opt/src
WORKDIR ${WORKDIR}

RUN    git config --global url."git://github.com/ghc/packages-".insteadOf     git://github.com/ghc/packages/ \
    && git config --global url."http://github.com/ghc/packages-".insteadOf    http://github.com/ghc/packages/ \
    && git config --global url."https://github.com/ghc/packages-".insteadOf   https://github.com/ghc/packages/ \
    && git config --global url."ssh://git@github.com/ghc/packages-".insteadOf ssh://git@github.com/ghc/packages/ \
    && git config --global url."git@github.com:/ghc/packages-".insteadOf      git@github.com:/ghc/packages/ \
    && git clone --recursive --depth 1 --single-branch -b ${REPO_TAG} ${REPO}

ADD build.mk ${WORKDIR}/ghc/mk/build.mk

RUN apt-get install -y alex happy

RUN (cd ghc; ./boot)
RUN (cd ghc; autoreconf)
RUN (cd ghc; ./configure --prefix=/opt)
RUN (cd ghc; make)
RUN (cd ghc; make install)

ENV PATH /opt/bin:/usr/sbin:/usr/bin:/sbin:/bin
