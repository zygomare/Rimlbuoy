#' Convert coordinates degrees minutes secondes to decimal
#'
#'
#' @param c is character coordinates following "48°40.21'N" format.
#'
#' @return decimal coordinates
#' @examples
#' convert_str_coord("48°40.21'N")
#' convert_str_coord("068°34.75'W")
#'
#' @author  Thomas Jaegler, modified by Simon Bélanger
#'@export convert_str_coord
#'



convert_str_coord<-function(c){


  z <- sapply((strsplit(c, "[Â°\\.\\']")), as.numeric)
  if(is.na(z[1])){

    return(rep("NA",length(z)))

  }else{

  coord.deg = na.omit(z)[1, ] + na.omit(z)[2, ]/60 + z[3, ]/3600
  z <- sapply((strsplit(c, "[Â°\\.\\']")), as.character)
  sign = rep(1, length(coord.deg))
  sign[na.omit(z)[length(z[,1]),] == "S" | na.omit(z)[length(z[,1]),] == "W"] = -1
  return(sign*coord.deg)

  }
}
