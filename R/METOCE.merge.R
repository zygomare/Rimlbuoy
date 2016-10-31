#' Merge daily METOCE files to create time series
#'
#'@param path is the path where METOCE files are located
#'@param StartDate is the starting date of the time series
#'  (format is yyyymmdd)
#'@param EndDate is the ending date of the time series
#'  (format is yyyymmdd)
#'
#'@return Returns a list similar to the \code{\link{read.METOCE}}
#'@author Simon Bélanger
#'@export
#'
#'
#'
METOCE.merge <- function(path, StartDate, EndDate){
  setwd(path)

  # create a date vector of type YYYYMMDD
  d0 = as.POSIXct(as.character(StartDate), format="%Y%m%d")
  d1 = as.POSIXct(as.character(EndDate), format="%Y%m%d")
  dates.vec = format(seq(d0,d1,24*3600),"%Y%m%d")

  nday = length(dates.vec)

  for (i in 1:nday) {
    filen =list.files(pattern = paste("METOCE_", dates.vec[i],".*\\.DAT",sep=""))
    print(filen)
    if (length(filen) > 0) {
      for (j in 1:length(filen)) {
        x = read.METOCE(filen[j])
        if (i == 1) {
          all = x

        } else {

          # concatenate
          all = rbind(all,x)
        }

      }

    } else print(paste("No file for : ", dates.vec[i]))

  }

  return(all)
}
