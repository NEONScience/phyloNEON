##############################################################################################
#' @title Convert NEON download to taxon table for Phyloseq package
#' @author 
#' Hugh Cross \email{crossh@battelleecology.org}
#' @description
#' From Microbe Community Taxonomy tables downloaded from neonUtilities (expanded package), convert the PerSampleTaxonomy table to a taxon table ready to import to Phyloseq
#' 
#' 
#' 
##############################################################################################


# parameters: neonUtilities object, 
# Gene: "ITS" or "16S", 
# sampleType: 'soil', 'benthic', or 'surface' 


createTaxTable <- function(neonUtilObject, gene=NA, sampleType=NA){
  
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
    selectRanks = c("Feature.ID","kingdom","phylum","class","order","family","genus","species")
    if(sampleType == 'soil'){
      SEQTABLE = neonUtilObject$mct_soilPerSampleTaxonomy_ITS
    } else if(sampleType == 'benthic'){
      SEQTABLE = neonUtilObject$mct_benthicPerSampleTaxonomy_ITS
    } else if(sampleType == 'surface'){
      SEQTABLE = neonUtilObject$mct_surfaceWaterPerSampleTaxonomy_ITS
    }
  } else if(gene == '16S'){
    selectRanks = c("Feature.ID","domain","phylum","class","order","family","genus","species")
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
  
  print('the dimensions of the table to be analyzed')
  print(dim(SEQTABLE))
  
  mct.tax <- SEQTABLE %>%
    mutate(species = specificEpithet) %>%
    mutate(Feature.ID = sequenceName) %>%
    select(all_of(selectRanks)) %>%
    mutate(across(.fns = ~ replace_na(as.character(.x), ""))) %>%
    unique() 
  
  tax.matrix <- mct.tax %>%
    replace(is.na(.),"") %>%
    remove_rownames %>%
    column_to_rownames(var = "Feature.ID") %>%
    as.matrix(rownames=T)
  
  TAX <- tax_table(tax.matrix)
  
  return(TAX)
  
}

