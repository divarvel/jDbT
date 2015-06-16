FROM haskell:7.10

RUN /usr/bin/useradd -m deploy

ADD . /home/deploy/jdbt

RUN chown -R deploy:deploy /home/deploy/jdbt

ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

USER deploy
ENV HOME /home/deploy
WORKDIR /home/deploy/jdbt

RUN cabal update && cabal sandbox init
RUN cabal install -j4 --only-dependencies
RUN cabal configure -fapi && cabal build

EXPOSE 8080

ENTRYPOINT ["/home/deploy/jdbt/dist/build/jdbt-api/jdbt-api"]
