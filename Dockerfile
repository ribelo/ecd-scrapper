FROM mcr.microsoft.com/playwright:focal
WORKDIR /usr/src/app

ENV CLOJURE_VER=1.11.1.1105

COPY package.json yarn.lock shadow-cljs.edn deps.edn ./
COPY src ./src

ENV PLAYWRIGHT_BROWSERS_PATH=0

RUN yarn install --frozen-lockfile

RUN apt-get update \
    && apt-get -q -y install openjdk-17-jdk curl \
    && curl -s https://download.clojure.org/install/linux-install-$CLOJURE_VER.sh | bash

RUN yarn shadow:build

COPY . .

CMD ["node", "dist/app.js"]
