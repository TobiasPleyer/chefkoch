# chefkoch - recipe parser and crawler

## About

This tool is meant to download and extract recipe information from the German
cooking website [chefkoch.de]. The recipes can be downloaded to either [Yaml]
or [JSON] format, JSON being the default. Since there basically exists a JSON
library for every common programming language this gives a standardized
interface to post processing tasks in a programming language agnostic way.

The content of the downloaded recipes consists of the recipe's ingredients list,
its cooking instructions and a couple of meta data, e.g. its URL.

[chefkoch.de]: https://www.chefkoch.de/
[Yaml]: http://yaml.org/
[JSON]: https://json.org/

## Installation

The chefkoch tool is written in [Haskell] and maintained as a [stack], [cabal]
and [Nix] project, *stack* making use of the *cabal* build system under the
hoods. The *cabal* configuration files are also provided with this project. It
is not a requirement to have *Nix* installed in order to install this program,
it is simply another option available. I use *Nix* to ensure that my
development environment stays stable. The simplest way to acquire *stack* and
*cabal* is to install the [Haskell Platform].

This results in following options to install chefkoch

```
$ git clone https://github.com/TobiasPleyer/chefkoch
$ cd chefkoch
$ # this requires stack to be installed on the system
$ stack install
```

```
$ git clone https://github.com/TobiasPleyer/chefkoch
$ cd chefkoch
$ # the next steps require runhaskell, e.g. via the Haskell Plattform
$ runhaskell Setup configure
$ runhaskell Setup build
$ sudo runhaskell Setup install
```

```
$ git clone https://github.com/TobiasPleyer/chefkoch
$ cd chefkoch
$ # the next steps require that Nix has been installed on your system
$ nix-shell
$ # We are now entering the reproducible shell environment provisioned by Nix
[nix-shell]$ cabal build
# lots of output...
Linking /path_to_repo/chefkoch/dist-newstyle/.../chefkoch ...
#       ^ The executable is found under this path
# You could leave the Nix shell now if you want...
# As a nice shorthand cabal allows you to run the executable like so
[nix-shell]$ cabal run chefkoch -- --help
#                               ^ This separates cabal's command line options
#                                 from those of the run program
```

[Haskell]: https://www.haskell.org/
[stack]: https://docs.haskellstack.org/en/stable/
[cabal]: https://www.haskell.org/cabal/
[Haskell Platform]: https://www.haskell.org/platform/
[Nix]: https://nixos.org/

## Command Line Interface

```
$ chefkoch --help
chefkoch - a web crawler for the www.chefkoch.de cooking website

Usage: chefkoch [-u|--url URL] [-o|--output FILE] [-f|--format ARG]
                [-v|--verbose]
  Download the bare ingredients and cooking informations, without all the
  clutter.

Available options:
  -u,--url URL             The url of the recipe to be downloaded.
  -o,--output FILE         The name of the file to write the output to.
  -f,--format ARG          Specify the format to be used for output. Supported
                           values: raw, yaml, json (default).
  -v,--verbose             Send more info about program execution to stdout.
  -h,--help                Show this help text
```

The chefkoch script basically has two operation modes: direct URL or recipe of
the day. In URL mode any recipe can be downloaded by providing the URL. This is
done via the *-u/--url* option.

The recipe of the day mode is the default and automatically active if the
*-u/--url* is not given. It can be used to select recipes from the
[rezept-des-tages listing]. This is a data base of featured recipes. Every day
one recipe gets featured. The chefkoch script allows you to download the recipe
of the current day.

Currently the following output formats are supported:

    * json (default)
    * yaml
    * raw

Per default chefkoch will write all recipe information to standard out. This
can be changed with the *-o/--output* option. With this option you can provide
a file name to write the information to.

[rezept-des-tages listing]: https://www.chefkoch.de/rezept-des-tages.php

## Example Calls

```
$ chefkoch --help
```

```
$ chefkoch --url https://www.chefkoch.de/rezepte/1113761217428134/Brauhaus-Gulasch.html
```

```
$ chefkoch
```

```
$ chefkoch -v -f yaml
```

```
$ chefkoch --format=yaml -o ./recipe.yaml
```

## Nix

What is this *nix* folder for? This folder contains all [Nix] related files
required to create the (fixed) build environment for the project.

The main purpose of [Nix] is to provide a stable build environment for me and
everyone interested to hack on the tool. I do all my work within a *nix-shell*
environment. Nix will make sure that all required Haskell packages are
installed and with versions compatible to each other.  In addition the Nix
derivation setups up the build and development environment for me.
This includes:

- ghc
- ghci
- ghcid
- ghcide
- hlint

Except for *ghc* these are not necessary to build the project, but are tightly
connected to my personal development environment.

If you want to work with *Nix* in the *nix-shell* then first install *Nix* and
then run the following command:

```
$ nix-shell
[nix-shell]$ # We are in the Nix environment now, all tools are on the PATH
```

This project uses a fixed version of [nixpkgs] to guarantee reproducability.
Currently this version is **19.09**. In order to use a different version of
nixpkgs you can run this command:

```
$ nix-prefetch-github --rev '19.09' NixOS nixpkgs > nix/nixpkgs/github.json
```

[Nix]: https://nixos.org/
[nixpkgs]: https://github.com/NixOS/nixpkgs
