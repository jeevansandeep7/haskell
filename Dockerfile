FROM haskell:9.2

WORKDIR /app

COPY . .

RUN cabal update
RUN cabal build

CMD ["cabal", "run"]