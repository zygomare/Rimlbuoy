#' Reads optics data coming from IML Buoy and returns a list of field containing all the data stored in the OCR3 file. (see example)
#' @export read.OCR3
#' @param fname Filename
#' @return list of data
#' Headers :
#' DateTime   : Time Format DD-MMM-YYYY HH:MM:SS.ss
#' DOY        : Counter
#' lat        : Latitude (North +ve)
#' lon        : Longitude (East +ve)
#' waves      : Wavelength standard
#'
#' @examples
#' # read an optical data file from IML Buoy dat file
#' setwd(path.package("Rimlbuoy"))
#' raw = read.OCR3("extdata/IML-4_OCR3_20160501_000320.DAT")
#' str(raw)
#'
#'
#'
#'



read.OCR3<- function(filen) {

  con=file(filen,encoding="latin1")
  data=readLines(con)
  close(con)

  nrec = length(data)
  ncolumn=length(unlist(strsplit(data[nrec], ",")))
  
  tmp=unlist(strsplit(data, ","))
  tmp.mat = as.data.frame(matrix(tmp, ncol=ncolumn, nrow=nrec, byrow=T))

  DateTime = as.POSIXct(paste(tmp.mat$V1, tmp.mat$V2), format="%Y/%m/%d %H:%M:%S", tz="GMT")
  DOY = as.numeric(levels(tmp.mat$V3)[tmp.mat$V3])

  # Extract Lat and Lon
  lat = convert_str_coord(levels(tmp.mat$V4)[tmp.mat$V4])
  lon = convert_str_coord(levels(tmp.mat$V5)[tmp.mat$V5])


  BioShutter = tmp.mat$V20

  tmp=sapply(tmp.mat[,6:19], function(x) as.numeric(as.character(x)))

  Lu = (tmp[,c(1,3,5,7,9,11, 13)])
  Lu.sd = tmp[,c(2,4,6,8,10,12,14)]
  colnames(Lu) <- c("412","443","490","510","555","669", "683")
  colnames(Lu.sd) <- c("412","443","490","510","555","669", "683")



  return(list(DateTime=DateTime, DOY=DOY, lat=lat,lon=lon,
              waves = c(412,443,490,510,555,669, 683),
              Lu0.86m=Lu, Lu0.86m.sd=Lu.sd))

}
