FROM openjdk:8-jdk-slim

# Install tooling dependencies
RUN apt-get -q update && \
    apt-get -q install -y --no-install-recommends ca-certificates netbase curl git gcc g++ zlib1g-dev libncurses5-dev libbz2-dev && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    mkdir -p $HOME/.local/bin && \
    rm -rf /tmp/* \
           /var/cache/apk/* && \
    apt-get autoclean && \
    apt-get clean && \
    apt-get autoremove && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Install from source
RUN git clone --recursive https://github.com/typelead/eta && \
    cd /eta && \
    ./install.sh && \
    rm -rf /eta \
          ~/.stack \
          ~/.etlas/packages \ # This eats up over 400MB

ENV PATH=${PATH}:/${JAVA_HOME}/bin:/root/.local/bin