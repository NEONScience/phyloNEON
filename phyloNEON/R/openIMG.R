##############################################################################################
#' @title Open JGI landing page for NEON metagenomic samples
#' @author 
#' Hugh Cross \email{crossh@battelleecology.org}
#' @description
#' This tool will let the user select NEON samples from a provided data table and then automatically open the JGI IMG page for each sample (maximum of ten)
#' 
#' @param sampleID the dnaSampleID for the sample
#' @param sourceTable defaults
#' 
#' 
#' @return opens a web page in default browser
#' 
#' @examples
#' \dontrun{
#' # To open a page on jgi img db :
#' openIMGpage('BONA_013-O-20230710-COMP-DNA1')
#' }
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' @import readr
#' @import dplyr
#' @export

# changelog and author contributions / copyrights
#   Hugh Cross (2025-06-11)
#     original creation
#' 
#' 
##############################################################################################

openIMG <- function(sampleID,sourceTable = neon.metaDB){
  # add persistent link
  taxonPers <- "https://identifiers.org/img.taxon:"
  z <- dplyr::filter(sourceTable, dnaSampleID %in% sampleID) %>%
    dplyr::pull(imgGenomeID)
  a <- paste0(taxonPers,z)
  for (i in a){
    browseURL(i)
  }
  
}



