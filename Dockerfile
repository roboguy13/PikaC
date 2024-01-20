FROM haskell:9.8.1

RUN apt-get update && \
    apt-get install -y z3

RUN wget https://github.com/CVC4/CVC4/releases/download/1.7/cvc4-1.7-x86_64-linux-opt && cp cvc4-1.7-x86_64-linux-opt /usr/local/bin/cvc4

RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list && \
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import && \
    chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg && \
    apt-get update && \
    apt-get install -y default-jre && \
    apt-get install -y default-jdk && \
    apt-get install -y sbt

COPY ./suslik /Pika/suslik

RUN cd /Pika/suslik && sbt assembly

COPY ./PikaC.cabal /Pika/PikaC.cabal
COPY ./cabal.project.local /Pika/cabal.project.local
COPY ./src /Pika/src
COPY ./CHANGELOG.md /Pika/CHANGELOG.md
COPY ./README.md /Pika/README.md

WORKDIR /Pika

RUN cabal update && \
    cabal build --enable-tests --enable-benchmarks

COPY . /Pika/

CMD ["bash"]
