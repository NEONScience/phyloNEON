
library(phyloseq)
library('vegan')

makeOTU <- function(i){
  import_table <- read.table(i,header=TRUE,sep='\t',row.names=1, comment.char = "")
  names(import_table) <- gsub(x = names(import_table), pattern = "___", replacement = "-")
  names(import_table) <- gsub(x = names(import_table), pattern = "__", replacement = "\\.")
  otumat <- as.matrix(import_table)
  OTU = otu_table(otumat, taxa_are_rows = TRUE)
  return(OTU)
}

makeOTUflat <- function(i){
  import_table <- read.table(i,header=TRUE,sep='\t',comment.char = "")
  names(import_table) <- gsub(x = names(import_table), pattern = "___", replacement = "-")
  names(import_table) <- gsub(x = names(import_table), pattern = "__", replacement = "\\.")
  names(import_table) <- gsub(x = names(import_table), pattern = "X.OTU.ID", replacement = "sequenceName")
  return(import_table)
}


makeTAX <- function(x){
  import_taxa <- read.table(x,header=TRUE,sep='\t')
  ranks <- c("kingdom","phylum","class","order","family","genus","species")
  taxonomy <- import_taxa %>%
    mutate_at('Taxon',str_replace_all, "[a-z]__","") %>%
    separate(Taxon, sep = ';', into=ranks,remove = TRUE) %>%
    column_to_rownames(var = "Feature.ID") %>%
    as.matrix()
  TAX = tax_table(taxonomy)
  return(TAX)
}

makeTAXsize <- function(x){
  import_taxa <- read.table(x,header=TRUE,sep='\t')
  ranks <- c("kingdom","phylum","class","order","family","genus","species")
  taxonomy <- import_taxa %>%
    mutate_at('Taxon',str_replace_all, "[a-z]__","") %>%
    separate(Taxon, sep = ';', into=ranks,remove = TRUE) %>%
    mutate(Feature.ID = sapply(str_split(Feature.ID, ";"), `[`,1)) %>%
    column_to_rownames(var = "Feature.ID") %>%
    as.matrix()
  TAX = tax_table(taxonomy)
  return(TAX)
}

makeMETA <- function(y){
  import_table <- read.table(y,header=TRUE,sep='\t',row.names=1, comment.char = "")
  names(import_table) <- gsub(x = names(import_table), pattern = "___", replacement = "-")
  names(import_table) <- gsub(x = names(import_table), pattern = "__", replacement = "\\.")
  sample <- colnames(import_table)
  site <- sapply(str_split(sample, "_"), `[`,1)
  collectDate <- sapply(str_split(sample, "-"), `[`,5)
  metaDF <- data.frame(sample,site,collectDate)
  metaDFfmt <- column_to_rownames(metaDF, var="sample")
  META <- sample_data(metaDFfmt)
  return(META)
}

makeMETAflat <- function(y){
  import_table <- read.table(y,header=TRUE,sep='\t',row.names=1, comment.char = "")
  names(import_table) <- gsub(x = names(import_table), pattern = "___", replacement = "-")
  names(import_table) <- gsub(x = names(import_table), pattern = "__", replacement = "\\.")
  sample <- colnames(import_table)
  site <- sapply(str_split(sample, "_"), `[`,1)
  collectDate <- sapply(str_split(sample, "-"), `[`,5)
  metaDF <- data.frame(sample,site,collectDate)
  return(metaDF)
}
# CPER_046.M.14.9.20201102.gen.DNA1
# grepl("R1",parentFolderName)
makeMETAflatUP <- function(y){
  import_table <- read.table(y,header=TRUE,sep='\t',row.names=1, comment.char = "")
  names(import_table) <- gsub(x = names(import_table), pattern = "___", replacement = "-")
  names(import_table) <- gsub(x = names(import_table), pattern = "__", replacement = "\\.")
  sample <- colnames(import_table)
  site <- sapply(str_split(sample, "_"), `[`,1)
  plotID <- sapply(str_split(sample, "\\."), `[`,1)
  horizon <- sapply(str_split(sample, "\\."), `[`,2)
  collectYear <- str_match(sample,"202[0-3]")
  metaDF <- data.frame(sample,site,plotID,collectYear,horizon)
  return(metaDF)
}

makeMETAaq <- function(y){
  import_table <- read.table(y,header=TRUE,sep='\t',row.names=1, comment.char = "")
  names(import_table) <- gsub(x = names(import_table), pattern = "___", replacement = "-")
  names(import_table) <- gsub(x = names(import_table), pattern = "__", replacement = "\\.")
  sample <- colnames(import_table)
  site <- sapply(str_split(sample, "\\."), `[`,1)
  collectDate <- c()
  for (z in sample){
    if (!str_starts(z,"RUSH")){
      a <- sapply(str_split(z, "\\."), getElement, 2)
      if (stringr::str_starts(a, "^[:alpha:]")){
        cd <- sapply(str_split(z, "\\."), getElement, 3)
      } else {
        cd <- sapply(str_split(z, "\\."), getElement, 2)
      }
      collectDate <- c(collectDate, cd)
    } else {
      collectDate <- c(collectDate, "2021")
    }}
  metaDF <- data.frame(sample,site,collectDate)
  metaDFfmt <- column_to_rownames(metaDF, var="sample")
  META <- sample_data(metaDFfmt)
  return(META)
}


makeMETAaqFlat <- function(y){
  import_table <- read.table(y,header=TRUE,sep='\t',row.names=1, comment.char = "")
  names(import_table) <- gsub(x = names(import_table), pattern = "___", replacement = "-")
  names(import_table) <- gsub(x = names(import_table), pattern = "__", replacement = "\\.")
  sample <- colnames(import_table)
  site <- sapply(str_split(sample, "\\."), `[`,1)
  collectDate <- c()
  for (z in sample){
    if (!str_starts(z,"RUSH")){
      a <- sapply(str_split(z, "\\."), getElement, 2)
      if (stringr::str_starts(a, "^[:alpha:]")){
        cd <- sapply(str_split(z, "\\."), getElement, 3)
      } else {
        cd <- sapply(str_split(z, "\\."), getElement, 2)
      }
      collectDate <- c(collectDate, cd)
    } else {
      collectDate <- c(collectDate, "2021")
    }}
  metaDF <- data.frame(sample,site,collectDate)
  return(metaDF)
}
