FROM openjdk:8-jdk-slim
MAINTAINER Sibi Prabakaran <sibi@psibi.in>

RUN apt-get -q update && \
    apt-get -q install -y --no-install-recommends git netbase dnsutils && \
    rm -rf /tmp/* \
           /var/cache/apk/* && \
    apt-get autoclean && \
    apt-get clean && \
    apt-get autoremove && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ADD bin/ /root/
ENV PATH=${PATH}:/opt/java/bin:/root/.local/bin
