This project contains several tools for gene expression analysis with help of R/Bioconductor`

## Installation

The package requires installing `devtools` package and Bioconductor library set up.

From shell do `git clone` with submodules:

```{shell}
git clone --recursive https://github.com/ctlab/phantasus
R -e 'devtools::install_github("assaron/GEOquery")' # Installing GEOquery with better caching support
R -e 'devtools::install("phantasus")'
````

## Running

In R:

```{r}
library(phantasus)
example(servePhantasus)
```

Open `http://localhost:8000` in your browser.

## System dependencies

There are several system packages that have to be installed on the system. The names of these packages will be displayed during installation. On Ubuntu can install them beforehand and alltogether using command:

```{bash}
sudo apt-get install libapparmor-dev libprotobuf-dev protobuf-compiler libcurl4-openssl-dev
```
