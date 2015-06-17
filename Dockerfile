FROM haskell:7.10

RUN cabal update

ADD ./jdbt.cabal /opt/jdbt/jdbt.cabal
RUN cd /opt/jdbt && cabal install -j4 --only-dependencies

ADD . /opt/jdbt
RUN cd /opt/jdbt && cabal configure -fapi && cabal build

ENV PATH /root/.cabal/bin:$PATH

EXPOSE 8080

CMD ["./dist/build/jdbt-api/jdbt-api"]
