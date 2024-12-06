# phyloNEON

A set of tools in R and Python to run phylogenetic and taxonomic analyses on NEON and related data

## Accessing NEON data 

The initial functions of **phyloNEON** were developed to allow for converting data downloaded from NEON into formats that can easily be imported into other programs. Additional functions will be added, so watch this space. The first example below is to convert *Microbe Community Taxonomy* data into formats for importing into the popular R package [**Phyloseq**](https://joey711.github.io/phyloseq/). 

### Importing NEON Microbe Community Taxonomy (MCT) data into phyloseq

This example uses basic functions of **phyloNEON** to convert data downloaded using neonUtilities. This will start with a smaller dataset. Due to the increased sequencing output of NEON microbial data, the user is encouraged to begin with small sets of data, and then build from there. 

#### Download MCT data with neonUtilities

Make sure to set the `package` parameter to 'expanded' to download the perSample tables.

```
library(neonUtilities)

soil.mct <- loadByProduct(
  dpID='DP1.10081.002',
  startdate = "2021-01",
  enddate = "2021-12",
  package='expanded',
  site = c('BONA','HARV','NIWO'),

```

#### Create OTU frequency table 

The `createFreqTable` function will convert the downloaded neonUtilities data to a phyloseq frequency table object

```
library(phyloNEON)

soil.mct.freq <- createFreqTable(soil.mct, gene = 'ITS', sampleType = 'soil')

```

#### Create taxa table 

The `createTaxTable` function will convert the downloaded neonUtilities data to a phyloseq taxon table object

```
library(phyloNEON)


soil.mct.tax <- createTaxTable(soil.mct, gene = 'ITS',
                                sampleType = 'soil')


```

#### Create sample metadata table 




