##############################################################################################
#' @title Convert NEON download to OTU frequency table for Phyloseq package
#' @author 
#' Hugh Cross \email{crossh@battelleecology.org}
#' @description
#' From Microbe Community Taxonomy tables downloaded from neonUtilities (expanded package), convert the PerSampleTaxonomy table to a frequency table ready to import to Phyloseq
#' 
#' @param neonUtilObject the object downloaded with neonUtilities loadByProduct
#' @param gene the marker to process. Either 16S or ITS 
#' @param sampleType either soil, benthic, or surface
#' 
#' 
#' @return A matrix of counts for each sample (columns) for each OTU (rows) from the downloaded data product. Ready to import to Phyloseq
#' 
#' @examples
#' \dontrun{
#' # To convert community taxonomy data object downloaded with neonUtilities to Phyloseq-ready OTU table:
#' benthic.freq.its <- createFreqTable(benthic.mct, gene = 'ITS', sampleType = 'benthic')
#' }
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' @import dplyr
#' @import magrittr
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
#' @importFrom phyloseq otu_table
#' @export

# changelog and author contributions / copyrights
#   Hugh Cross (2024-12-05)
#     original creation
#' 
#' 
##############################################################################################


# parameters: neonUtilities object, 
# Gene: "ITS" or "16S", 
# sampleType: 'soil', 'benthic', or 'surface' 

createFreqTable <- function(neonUtilObject, gene=NA, sampleType=NA){
  
  # check gene
  if(!gene %in% c('ITS','16S')){
    stop(paste(gene,"is not a valid entry for gene. Gene must be 16S or ITS",sep=" "))
  }
  
  # check sample type 
  if(!sampleType %in% c("soil","benthic","surface")){
    stop(paste(sampleType, "is not a valid entry for sampleType. Must be either soil, benthic, or surface. Please make sure that you have downloaded the correct data product"))
  }
  
  # get the right table 
  if(gene == 'ITS'){
    if(sampleType == 'soil'){
      SEQTABLE = neonUtilObject$mct_soilPerSampleTaxonomy_ITS
    } else if(sampleType == 'benthic'){
      SEQTABLE = neonUtilObject$mct_benthicPerSampleTaxonomy_ITS
    } else if(sampleType == 'surface'){
      SEQTABLE = neonUtilObject$mct_surfaceWaterPerSampleTaxonomy_ITS
    }
  } else if(gene == '16S'){
    if(sampleType == 'soil'){
      SEQTABLE = neonUtilObject$mct_soilPerSampleTaxonomy_16S
    } else if(sampleType == 'benthic'){
      SEQTABLE = neonUtilObject$mct_benthicPerSampleTaxonomy_16S
    } else if(sampleType == 'surface'){
      SEQTABLE = neonUtilObject$mct_surfaceWaterPerSampleTaxonomy_16S
  }}
  
  # check that table is right 
  if(exists("SEQTABLE") && is.data.frame(get("SEQTABLE"))){
    print('table found, proceeding with conversion')
  } else {
    stop("table not found, make sure you have the right sample type for the object")
  }
  
  #print('the dimensions of the table to be analyzed')
  #print(dim(SEQTABLE))
  
  # get resequenced samples
  # have to use the metadata table here, first get correct one
  # get the right table 
  if(gene == 'ITS'){
    if(sampleType == 'soil'){
      METATABLE = neonUtilObject$mct_soilSampleMetadata_ITS
    } else if(sampleType == 'benthic'){
      METATABLE = neonUtilObject$mct_benthicSampleMetadata_ITS
    } else if(sampleType == 'surface'){
      METATABLE = neonUtilObject$mct_surfaceSampleMetadata_ITS
    }
  } else if(gene == '16S'){
    if(sampleType == 'soil'){
      METATABLE = neonUtilObject$mct_soilSampleMetadata_16S
    } else if(sampleType == 'benthic'){
      METATABLE = neonUtilObject$mct_benthicSampleMetadata_16S
    } else if(sampleType == 'surface'){
      METATABLE = neonUtilObject$mct_surfaceSampleMetadata_16S
  }}
 
  dupeseq <- METATABLE %>%
  select(dnaSampleID) |>
  group_by_all() |>
  filter(n() > 1) |>
  ungroup() %>%
  unique()

  dupeseq.samples = dupeseq$dnaSampleID

  # now convert, renaming resequenced samples if necessary
  freq.table1 <- SEQTABLE %>%
    mutate(dnaSampleID = toupper(dnaSampleID)) %>%
    dplyr::mutate(seqID = sapply(fileName, function(x) getSeqID(x,gene))) %>%
    dplyr::mutate(dnaSampleID = case_when(dnaSampleID %in% dupeseq.samples ~ paste(dnaSampleID,seqID, sep = '_'),
                                 .default = dnaSampleID)) %>%
    dplyr::select(sequenceName,dnaSampleID,individualCount) %>%
    tidyr::pivot_wider(
      names_from = dnaSampleID,
      values_from = individualCount,
    ) %>%
    tibble::column_to_rownames(var = 'sequenceName') %>%
    replace(is.na(.), 0)
  
  freq.table2 <- dplyr::mutate_all(freq.table1, function(x) as.numeric(as.character(x)))
  
  otumat <- as.matrix(freq.table2)
  
  OTU = phyloseq::otu_table(otumat, taxa_are_rows = TRUE)
  
  return(OTU)
  
}

