# chefkoch

```bash
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
  -l,--link LINK           The link (url) of the recipe to be downloaded
  --links-only             Don't look for the ingredients and instructions, just
                           for the links.
  -r,--random              Whether to choose a recipe at random
  -o,--output FILE         The link (url) of the recipe to be downloaded
  -f,--format ARG          Specify the format to be used for output. Supported
                           values: raw, yaml
  -h,--help                Show this help text
```
