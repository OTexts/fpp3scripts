# fpp3 scripts

This repository contains the R code used to produce all the tables, figures and examples in [*Forecasting: Principles and Practice* (3rd edition)](https://OTexts.com/fpp3).

## Setting up the environment

Packages are managed with [uvr](https://github.com/nbafrank/uvr), a fast R package manager.
Required packages are listed in `uvr.toml` and pinned in `uvr.lock`.
This includes the [otexts](https://github.com/otexts/otexts) package, installed from GitHub, which `before-each-chapter.R` uses to set up fonts, colours and printing options.

1. Install R (>= 4.6.1) and [uvr](https://github.com/nbafrank/uvr).

2. From the root of this repository, install the packages into `.uvr/library`:

   ```sh
   uvr sync
   ```

3. Start R (or open the project in RStudio or Positron) with the repository root as the working directory.

Most datasets come from the `fpp3` package and its dependencies, or are downloaded from <https://OTexts.com/fpp3/extrafiles/>.
The shapefile used for the map in the hierarchical forecasting chapter is included in `extrafiles/`.
