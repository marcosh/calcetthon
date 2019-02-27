FROM debian:stretch

RUN apt-get update && \
    apt-get install -y g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg curl libpq-dev

RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /app

COPY . /app

RUN stack --copy-bins build && \
    mkdir -p dist && \
    cp -r $(stack path --local-bin) dist

FROM debian:stretch

RUN apt-get update && \
    apt-get install -y g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg curl libpq-dev

COPY --from=0 /app/dist /app

WORKDIR /app

ARG executable
ENV executable=${executable}

CMD /app/bin/${executable}
