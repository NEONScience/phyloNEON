##############################################################################################
#' @title getSeqID derive sequencingRunID from taxon table name
#' @author 
#' Hugh Cross \email{crossh@battelleecology.org}
#' @description
#' Takes a MCT taxon table name and outputs the sequencingRunID from within the name
#' 
#' @param taxname the name of the taxon table
#' @param gene the gene under study
#' 
#' 
#' @return the sequencingRunID
#' 
#' @examples
#' \dontrun{
#' # To get the sequencingRunID from the following taxon table:
#' taxtable = 'RUSH_19S_36_3318_HKJCFDRX3_ITS_map.csv'
#' seqrun <- getSeqID(taxtable,'ITS')
#' seqrun: 'HKJCFDRX3'
#' }
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' @export

# changelog and author contributions / copyrights
#   Hugh Cross (2024-12-05)
#     original creation
#' 
#' 
##############################################################################################


getSeqID <- function(taxname,gene){
  renamed <- phyloNEON::splitID(taxname,5,6)
  seqID <- gsub(gene,'',renamed)
  seqID <- gsub('_','',seqID)
  return(seqID)
}
