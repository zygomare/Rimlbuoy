#' Merge daily OPTICD files to create multi days time series
#'
#' The OPTICD file contains OCR data as well as
#' fluorescence (FDOM and FCHL), GPS, and
#' tilt and heading information.
#'
#'@param path is the path where OPTICD files are located
#'@param StartDate is the starting date of the time series
#'  (format is yyyymmdd)
#'@param EndDate is the ending date of the time series
#'  (format is yyyymmdd)
#'
#'@return Returns a list similar to the \code{\link{read.OPTICD}}
#'@author Simon Bélanger
#'@export
#
OPTICD.merge <- function(path, StartDate, EndDate){
  setwd(path)

  # create a date vector of type YYYYMMDD
  d0 = as.POSIXct(as.character(StartDate), format="%Y%m%d")
  d1 = as.POSIXct(as.character(EndDate), format="%Y%m%d")
  dates.vec = format(seq(d0,d1,24*3600),"%Y%m%d")

  nday = length(dates.vec)

  for (i in 1:nday) {
    filen =list.files(pattern = paste("OPTICD_", dates.vec[i],".*\\.DAT",sep=""))
    print(filen)
    if (length(filen) > 0) {
      for (j in 1:length(filen)) {
        x = read.OPTICD(filen[j])
        if (i == 1) {
          all = x
          # remove waves from the list and convert into data frame
          all = as.data.frame(all[-which(names(all) == "waves")])

        } else {

          # remove
          x = as.data.frame(x[-which(names(x) == "waves")])

          # concatenate
          all = rbind(all,x)
        }

      }

    } else print(paste("No file for : ", dates.vec[i]))

  }

  ix.Ed = which(names(all) == "Ed0p.412")
  ix.Lu = which(names(all) == "Lu0.86m.412")

  return(list(DateTime=all$DateTime, DOY=all$DOY,
              lat=all$lat, lon=all$lon,
              waves = c(412,443,490,510,550,670, 683),
              Ed0p=all[,ix.Ed:(ix.Ed+6)],
              Ed0p.sd=all[,(ix.Ed+7):(ix.Ed+13)],
              Lu0.86m=all[,ix.Lu:(ix.Lu+6)],
              Lu0.86m.sd=all[,(ix.Lu+7):(ix.Lu+13)],
              PAR=all$PAR, PAR.sd=all$PAR.sd,
              FCHL=all$FCHL, FCHL.sd=all$FCHL.sd,
              FDOM=all$FDOM, FDOM.sd=all$FDOM.sd,
              pitch=all$pitch, pitch.sd=all$pitch.sd,
              roll=all$roll, roll.sd=all$roll.sd,
              tilt=all$tilt, tilt.sd=all$tilt.sd,
              heading=all$heading))
}
