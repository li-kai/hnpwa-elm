FROM frolvlad/alpine-glibc

RUN apk add --no-cache gmp yarn

RUN yarn global add create-elm-app

RUN mkdir -p /home/elm/app
WORKDIR /home/elm/app

COPY . .

CMD [ "elm-app", "start" ]
