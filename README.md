# chefkoch

## About

This tool is meant to download and extract recipe information from the German
cooking website [chefkoch]. The recipes can be downloaded to several common
formats, like [yaml] or [json]. This gives a standardized interface to post
processing tasks in a programming language agnostic way.

The content of the downloaded recipes consists of the recipe's ingredients list,
its cooking instructions and a couple of meta data, e.g. its URL.

[chefkoch]: https://www.chefkoch.de/
[yaml]: http://yaml.org/
[json]: https://json.org/

## Installation

The chefkoch tool is written in [Haskell] and maintained as a [stack] project.
*stack* is making use of the [cabal] build system under the hoods. The cabal
configuration files are also checked in to github. The simplest way to acquire
all these tools is to install the [Haskell Platform].

This results in two options to install chefkoch

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

[Haskell]: https://www.haskell.org/
[stack]: https://docs.haskellstack.org/en/stable/
[cabal]: https://www.haskell.org/cabal/
[Haskell Platform]: https://www.haskell.org/platform/

## Command Line Interface

```
$ chefkoch --help
chefkoch - a web crawler for the www.chefkoch.de cooking website

Usage: chefkoch [-y|--year YEAR] [-m|--month MONTH] [-d|--day DAY]
                [-l|--link LINK] [--links-only] [-r|--random] [-o|--output FILE]
                [-f|--format ARG]
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
  -o,--output FILE         The link (url) of the recipe to be downloaded
  -f,--format ARG          Specify the format to be used for output. Supported
                           values: raw, yaml
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

    * yaml (default)
    * json
    * raw

Per default chefkoch will write all recipe information in the file *recipe* in
the current working directory. This can be changed with the *-o/--output* option.
If the information shall be sent to stdout, use **-** as the file specifier.

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
