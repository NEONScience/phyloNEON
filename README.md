
![phyloNEON logo](images/phyloNEON_logo1.png)

A set of tools in R and Python to run phylogenetic and taxonomic analyses on NEON and related data

## Installation

To install phyloNEON, you will need the `devtools` package.

```
library(devtools)

install_github("NEONScience/phyloNEON/phyloNEON")

```

## Accessing and using NEON genetic data 

NEON offers several data products that include genetic data. This repository is being developed to include tools and guidelines to help users of NEON data to better utilize the genetic data. 

### NEON metagenomic data

DNA is extracted from NEON soil and aquatic samples and sequenced with a shotgun sequence library prep. Through collaborations with the Joint Genome Institute (JGI) and the National Microbime Collaborative Network (NMDC), most of the metagenomic sequencing data are available on the data portals of these organizations. Connections to these external data sources are being built into NEON data releases. The phyloNEON package also offers some tools and guidelines to help the user find and analyze NEON metagenomic data on the JGI/NMDC data portals. 

[This page on the repo](docs/metagenomic/README.md) (in `docs/metagenomic/README.md`) will help you get started


### NEON microbe taxonomy data

The phyloNEON package has tools to help convert data from the Microbe Community Taxonomy (MCT) data products into formats for import into popular metabarcoding tools, such as phyloseq and Qiime. 

[See these instructions on downloading and converting NEON samples](docs/microbeTaxonomy/README.md). (in `docs/microbeTaxonomy/README.md`) 

