#' Reads PAR_DIGI file coming from IML Buoy
#'
#' The PAR_DIGI file contains PAR and tilt data from the Mini buoy.
#'
#' @param fname Filename
#'
#' @return  It returns a list of fields containing all the data
#' stored in the PAR_DIGI file. (see example)
#'
#' Headers :
#' DateTime   : Time Format DD-MMM-YYYY HH:MM:SS.ss
#' DOY        : Counter
#' lat        : Latitude (North +ve)
#' lon        : Longitude (East +ve)
#' pitch      : Pitch Angle
#' pitch.sd   : Standard deviation of Pitch Angle
#' roll       : Roll Angle
#' roll.sd    : Standard deviation of Roll Angle
#' tilt       : Tilt Angle
#' tilt.sd    : Standard deviation of Tilt Angle
#'
#' @examples
#' # read an optical data file from IML Buoy dat file
#' setwd(path.package("Rimlbuoy"))
#' raw = read.PAR_DIGI("extdata/IML-")
#' str(raw)
#'
#'
#'@author Thomas Jaegler
#'
#'@export read.PAR_DIGI
#'



read.PAR_DIGI <- function(filen) {

  con=file(filen,encoding="latin1")
  data=readLines(con)
  close(con)

  nrec = length(data)
  nid=length(unlist(strsplit(data[1], ",")))
  tmp=unlist(strsplit(data, ","))
  tmp.mat = as.data.frame(matrix(tmp, ncol=nid, nrow=nrec, byrow=T))

  DateTime = as.POSIXct(paste(tmp.mat$V1, tmp.mat$V2), format="%Y/%m/%d %H:%M:%S", tz="GMT")

  Model_numb=tmp.mat$V3

  timer = as.numeric(levels(tmp.mat$V4)[tmp.mat$V4])
  PAR= as.numeric(levels(tmp.mat$V5)[tmp.mat$V5])
  pitch =  as.numeric(levels(tmp.mat$V6)[tmp.mat$V6])
  roll =  as.numeric(levels(tmp.mat$V7)[tmp.mat$V7])
  #### Compute Tilt from Roll and Pitch for each sensor
  d2r <- pi / 180
  tilt <- atan(sqrt(tan(roll*d2r)^2+tan(pitch*d2r)^2))/d2r


  internal_temp= as.numeric(levels(tmp.mat$V8)[tmp.mat$V8])
  cheksum= as.numeric(levels(tmp.mat$V9)[tmp.mat$V9])

  return(list(DateTime=DateTime, Model_numb=Model_numb,
              PAR=PAR,
              pitch=pitch, roll=roll,tilt=tilt,
              internal_temp=internal_temp,cheksum=cheksum))

}
