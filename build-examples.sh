#!/bin/sh

elm-package install && \
./node_modules/elm-live/bin/elm-live.js --port=2018 --open --pushstate --debug -- example/Main.elm --output index.js