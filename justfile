default:
    @just --list

run:
    elm make src/Main.elm
    elm reactor
