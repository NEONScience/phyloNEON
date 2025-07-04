##############################################################################################
#' @title parseMapFile derive sequencingRunID from taxon table name
#' @author 
#' Hugh Cross \email{crossh@battelleecology.org}
#' @description
#' Takes a MCT taxon table name and outputs the sequencingRunID from within the name
#' 
#' @param mapFile the name of the taxon table
#' @param gene the gene under study
#' 
#' 
#' @return the sequencingRunID
#' 
#' @examples
#' \dontrun{
#' # To get the sequencingRunID from the following taxon table:
#' taxtable = 'RUSH_19S_36_3318_HKJCFDRX3_ITS_map.csv'
#' seqrun <- parseMapFile(taxtable,'ITS')
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

parseMapFile <- function(mapFile,gene){
  splitstr <- str_split(mapFile,"_")
  # get rid of map.csv
  splitRef <- splitstr[[1]][1:length(splitstr[[1]])-1]
  if(splitRef[length(splitRef)]== gene){
    seqrun = splitRef[length(splitRef)-1]
  } else {
    seqrun = splitRef[length(splitRef)]
  }
  return(seqrun)
}

