FROM debian:latest
RUN apt-get update && apt-get install -y \ 
    curl \
    git \
    default-jdk \
    g++ \
    cabal-install \
    dos2unix
RUN cabal update && cabal install --lib random && cabal install --lib Unique && cabal install --lib mtl && cabal install --lib parallel
WORKDIR /HyFI
COPY . .
RUN find . -type f | xargs dos2unix
