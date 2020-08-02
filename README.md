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

Usage: chefkoch [-y|--year YEAR] [-m|--month MONTH] [-d|--day DAY]
                [-u|--url URL] [--urls-only] [-r|--random] [-o|--output FILE]
                [-f|--format ARG] [-v|--verbose]
  Download the bare ingredients and cooking informations, without all the
  clutter.

Available options:
  -y,--year YEAR           The year the recipe was published
  -m,--month MONTH         The month the recipe was published
  -d,--day DAY             The day the recipe was published
  -u,--url URL             The url of the recipe to be downloaded
  --urls-only              Don't look for the ingredients and instructions, just
                           fetch the URLs belonging to the recipes.
  -r,--random              Whether to choose a recipe at random
  -o,--output FILE         The name of the file to write the output to.
  -f,--format ARG          Specify the format to be used for output. Supported
                           values: raw, yaml, json (default)
  -v,--verbose             Send more info about program execution to stdout
  -h,--help                Show this help text
```

The chefkoch script basically has two operation modes: custom or recipe of the
day. In custom mode any recipe can be downloaded by providing the URL. This is
done via the *-u/--url* option.

The recipe of the day mode is different. It can be used to select recipes from
the [rezept-des-tages listing]. This is a data base of featured recipes. Every
day one recipe gets featured, thus every year, month, day combination
corresponds to one specific recipe. The chefkoch script allows to download
these recipes by specifying the year, month and day via the *-y/--year*,
*-m/--month* and *-d/--day* selectors. The selection process follows the
following logic:

    1. If only the year selector is given, the script will take the current
       month as the value for the month selector and then download all recipes
       for this year and month.
    2. If only the month selector is given, the script will take the current
       year as the value for the year selector and then download all recipes
       for this year and month.
    3. If only the day selector is given, the script will take the current
       month as the value for the month selector and the current year as the
       value for the year selector and then download the recipe for this year,
       month and day.
    4. If the day selector is given together with either a year or a month
       selector, then the script will use the value of the current month/year
       for the missing selector and then download the recipe for this year,
       month and day.
    5. If only all three selectors are given then the script will download the
       recipe for this exact selection.

Currently the following output formats are supported:

    * json (default)
    * yaml
    * raw

Per default chefkoch will write all recipe information in the file *recipe* in
the current working directory. This can be changed with the *-o/--output*
option.  If the information shall be sent to stdout, use **-** as the file
specifier.

[rezept-des-tages listing]: https://www.chefkoch.de/rezept-des-tages.php

## Example Calls

```
$ chefkoch --help
```

```
$ chefkoch --url https://www.chefkoch.de/rezepte/1113761217428134/Brauhaus-Gulasch.html
```

```
$ chefkoch -y 2016 -m 3
```

```
$ chefkoch -y 2015 -m 6 -d 12 --urls-only
```

```
$ chefkoch -y 2017 --format=json -o-
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
