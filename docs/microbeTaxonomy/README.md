
# Getting started with NEON microbe community taxonomy data

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
  site = c('BONA','HARV','NIWO'))

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

This function will create a very basic metadata table that takes the information from the sample metadata table in the download. Additional metadata can be added (and watch this space for additional information and tutorials), but this function gives you the minimum to get started using Phyloseq.

```
library(phyloNEON)

soil.meta <- createMetaTable(soil.mct,gene='ITS',sampleType = 'soil')

```

#### Create **phyloseq** object

Now we put it all together. The OTU, taxa, and metadata tables created by the phyloNEON functions can now be imported into phyloseq. This will use the phyloseq command `phyloseq` to create the object. 


```
library(phyloseq)

soil.phylo <- phyloseq(soil.mct.freq,soil.mct.tax,soil.meta)

# check the object
soil.phylo

# output:

phyloseq-class experiment-level object
otu_table()   OTU Table:         [ 14299 taxa and 198 samples ]
sample_data() Sample Data:       [ 198 samples by 5 sample variables ]
tax_table()   Taxonomy Table:    [ 14299 taxa by 7 taxonomic ranks ]


```

The `soil.phylo` object is now a phyloseq object, for which there is a wealth of tutorials and instructions available to run taxonomic and ecological analyses on the NEON data. 



