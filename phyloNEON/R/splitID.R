##############################################################################################
#' @title splitID split string into parts by underscore and keep one or series of parts 
#' @author 
#' Hugh Cross \email{crossh@battelleecology.org}
#' @description
#' Takes a string of characters delimited by underscore, breaks it by underscore, and then collapses selected pieces (by number) back
#' 
#' @param string the string of characters delimited by underscore
#' @param first the first segment to keep after splitting
#' @param last the last segment of string to keep after splitting
#' 
#' 
#' @return a new string with only the first to the last parts collapsed back together with underscore
#' 
#' @examples
#' \dontrun{
#' # To split up string and retain 5th and 6th parts:
#' taxtable = 'RUSH_19S_36_3318_HKJCFDRX3_ITS_map.csv'
#' newtax <- splitID(taxtable,5,6)
#' newtax: 'HKJCFDRX3_ITS'
#' }
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' @importFrom stringr str_split
#' @export

# changelog and author contributions / copyrights
#   Hugh Cross (2024-12-05)
#     original creation
#' 
#' 
##############################################################################################

splitID <- function(string,first,last){
  splitter <- str_split(string,"_")
  parts <- splitter[[1]][first:last]
  merge <- paste(parts,collapse="_")
  return(merge)
}

