FROM fpco/stack-build:latest

ADD . /opt/jdbt

WORKDIR /opt/jdbt

RUN stack setup

RUN stack build --only-snapshot

RUN stack build

RUN stack install

EXPOSE 8080

CMD stack exec -- jdbt-api
