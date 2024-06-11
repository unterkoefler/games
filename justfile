default:
    @just --list

alias s := start
start:
    elm reactor

alias b := build
build:
    elm make src/Main.elm --output docs/index.js --optimize

serve-prod: build
    simple-http-server --index --nocache docs
