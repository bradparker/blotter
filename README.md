# Blotter

An exercise in working with (and learning) Haskell.

The idea is to build a blog (a well known thing), with all the bells and whistles that a web appliction would usually have.

## Requirements

* [Stack](https://haskellstack.org)
* [Nix](https://nixos.org/nix/)
* [Docker](https://www.docker.com)
* [direnv](https://direnv.net)

## Setup

```
$ cp .envrc.example .envrc
$ direnv allow
$ docker-compose up -d
$ database/scripts/create
$ database/scripts/migrate
$ stack test
```
