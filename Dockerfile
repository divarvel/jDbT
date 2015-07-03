FROM fpco/stack-build:latest

RUN mkdir -p /opt/jdbt
ADD . /opt/jdbt

WORKDIR /opt/jdbt

RUN stack setup

RUN stack build --only-snapshot

RUN stack build

EXPOSE 8080

ENTRYPOINT ["stack exec -- jdbt-api"]
