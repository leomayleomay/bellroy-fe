default:
    @just --list

run:
    elm make src/Main.elm --output=Main.js
    elm reactor
