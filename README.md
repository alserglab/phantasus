Phantasus is a web tool designed for visual and
interactive gene expression analysis.
In particular, it was designed to allow to go from a typical dataset to
differential expression and downstream analysis in an easy and streamlined
manner. For that aim, Phantasus integrates an intuitive heatmap interface with
gene expression analysis tools from Bioconductor. 

Main features:
* Loading public datasets from Gene Expression Omnibus with both microarrays and RNA-seq datasets (via ARCHS4) being supported.
* Differential gene expression using `limma` or `DESeq2`.
* Publication ready plots with export to SVG: PCA plot, row profiles, box plots.
* Clustering: k-means and hierarchical.
* Gene set enrichment analysis via `fgsea` package.
* Sharing session links.

<img src="https://alserglab.github.io/phantasus-doc/images/screenshot.png" width="600px" />

## Quick start

Phantasus can be accessed online via its official mirror: <https://alserglab.wustl.edu/phantasus>.

Alternatively, phantasus can be set up locally as an R package. The latest version of Phantasus 
can be installed from GitHub using `devtools` package.

```r
devtools::install_github("ctlab/phantasus")
```

There are several system packages that have to be installed on the system. The
names of these packages will be displayed during installation. On Ubuntu one can
install them beforehand and all together using the command:

```bash
sudo apt-get install libapparmor-dev libfontconfig1-dev libcairo2-dev libcurl4-openssl-dev pandoc libtiff5-dev libfribidi-dev libharfbuzz-dev libssl-dev libxml2-dev libprotobuf-dev protobuf-compiler
```

Further, the latest version of `phantasus` depends on `rhdf5client (>= 1.25.1)` from Bioconductor 3.19, which on older systems can be more convenient to install from GitHub:

```r
devtools::install_github("vjcitn/rhdf5client")
```

Before the first run of Phantasus you need to do an initial setup. To perform interactive setup run the following commands. We recommend to use the first option for each of the questions:

```r
library(phantasus)
setupPhantasus()
```

After the setup is finished, the following command will start the application at  <http://0.0.0.0:8000>
and open it in the default browser:

```r
servePhantasus()
```

Please refer to the [documentation](https://alserglab.github.io/phantasus-doc/installation.html#using-docker) for comprehensive installation and configuration instructions. 

## Links:
* Official mirror: <https://alserglab.wustl.edu/phantasus>.
* Documentation: <https://alserglab.github.io/phantasus-doc>.
* Bioconductor package: <https://bioconductor.org/packages/phantasus>.
* Docker image: <https://hub.docker.com/r/alserglab/phantasus>.

## Citation:
* Kleverov, M., Zenkova, D., Kamenev, V., Sablina, M., Artyomov, M. N., & Sergushichev, A. A. (2024). Phantasus, a web application for visual and interactive gene expression analysis. Elife, 13, e85722. https://doi.org/10.7554/eLife.85722. 

