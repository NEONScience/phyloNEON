##############################################################################################
#' @title Convert NEON download to metadata table for Phyloseq package
#' @author 
#' Hugh Cross \email{crossh@battelleecology.org}
#' @description
#' From Microbe Community Taxonomy tables downloaded from neonUtilities (expanded package), convert the PerSampleTaxonomy table to a frequency table ready to import to Phyloseq
#' 
#' 
#' 
##############################################################################################


createMetaTable <- function(neonUtilObject, gene=NA, sampleType=NA){
  
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
      METATABLE = neonUtilObject$mct_soilSampleMetadata_ITS
      selectFields = c("dnaSampleID","siteID","plotID","collectDate","sampleMaterial")
    } else if(sampleType == 'benthic'){
      METATABLE = neonUtilObject$mct_benthicSampleMetadata_ITS
      selectFields = c("dnaSampleID","siteID","collectDate","sampleMaterial")
    } else if(sampleType == 'surface'){
      METATABLE = neonUtilObject$mct_surfaceWaterSampleMetadata_ITS
      selectFields = c("dnaSampleID","siteID","collectDate","sampleMaterial")
    }
  } else if(gene == '16S'){
    if(sampleType == 'soil'){
      METATABLE = neonUtilObject$mct_soilSampleMetadata_16S
      selectFields = c("dnaSampleID","siteID","plotID","collectDate","sampleMaterial")
    } else if(sampleType == 'benthic'){
      METATABLE = neonUtilObject$mct_benthicSampleMetadata_16S
      selectFields = c("dnaSampleID","siteID","collectDate","sampleMaterial")
    } else if(sampleType == 'surface'){
      METATABLE = neonUtilObject$mct_surfaceWaterSampleMetadata_16S
      selectFields = c("dnaSampleID","siteID","collectDate","sampleMaterial")
    }}
  
  # check that table is right 
  if(exists("METATABLE") && is.data.frame(get("METATABLE"))){
    print('table found, proceeding with conversion')
  } else {
    stop("table not found, make sure you have the right sample type for the object")
  }
  
  print('the dimensions of the table to be analyzed')
  print(dim(METATABLE))
  
  sample.meta <- METATABLE %>%
    dplyr::select(selectFields) %>%
    dplyr::mutate(sampleName = dnaSampleID) %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    column_to_rownames(var = "dnaSampleID")
  
  META <- sample_data(sample.meta)
  
  return(META)
  
}

