# Oracle stuff taken and modified from https://github.com/dockerfile/java/blob/master/oracle-java8/Dockerfile
# MIT LICENSE of dockerfile/java  vvv
#------------------------------------
#The MIT License (MIT)
#
#Copyright (c) Dockerfile Project
#
#Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:
#
#The above copyright notice and this permission notice shall be included in
#all copies or substantial portions of the Software.
#
#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
#THE SOFTWARE.
#------------------------------------
# https://github.com/dockerfile/java/blob/master/LICENSE  ^^^

# Oracle Java 8 Dockerfile
# https://github.com/dockerfile/java
# https://github.com/dockerfile/java/tree/master/oracle-java8


FROM ubuntu:xenial

ENV JAVA_VERSION_MAJOR=8 \
    JAVA_PACKAGE=jdk \
    JAVA_HOME=/usr/lib/jvm/java-8-oracle \
    JVM_OPTS="" \
    PATH=$PATH:/root/.local/bin \
    LANG=C.UTF-8

# Initialize apt
RUN apt-get update -q

# Install eta depedencies
RUN apt-get install -q -y --no-install-recommends \
    ca-certificates \
    curl \
    unzip \
    cabal-install \
    libbz2-dev \
    git \
    zlib1g-dev \
    build-essential

# Install haskell-stack
RUN curl -sSL https://get.haskellstack.org/ | sh && \
    mkdir -p $HOME/.local/bin

# Install java8
RUN apt-get install -q -y apt-file apt-utils && \
    apt-file update && \
    apt-get install -qy software-properties-common && \
    echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && \
    add-apt-repository -y ppa:webupd8team/java && \
    apt-get update -q && \
    apt-get install -y oracle-java8-installer && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /var/cache/oracle-jdk8-installer

# Clone the latest eta
RUN cd /usr && \
    git clone --recursive https://github.com/typelead/eta && \
    cd /usr/eta && \
    ./install.sh
