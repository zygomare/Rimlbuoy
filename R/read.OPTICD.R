#' Reads OPTICD file coming from IML Buoy
#'
#' The OPTICD file contains OCR data as well as
#' fluorescence (FDOM and FCHL), GPS, and tilt and heading information.
#'
#' @param fname Filename
#'
#' @return  It returns a list of fields containing all the data
#' stored in the OPTICD file. (see example)
#'
#' Headers :
#' DateTime   : Time Format DD-MMM-YYYY HH:MM:SS.ss
#' DOY        : Counter
#' lat        : Latitude (North +ve)
#' lon        : Longitude (East +ve)
#' waves      : Wavelength standard
#' Ed0p       : Irradiance for each wavelength
#' Ed0p.sd    : Standard deviation of irradiance for each wavelength
#' Lu0.86m    : Radiance for each wavelength
#' Lu0.86m.sd : Standard deviation of radiance for each wavelength
#' PAR        : Photosynthetic Active Radiation
#' PAR.sd     : Standard deviation of Photosynthetic Active Radiation
#' FCHL       : Fluorescence of Chlorophyl
#' FCHL.sd    : Standard deviation of Fluorescence of Chlorophyl
#' FDOM       : Fluorescence of CDOM (Coloured Dissolved Organic Matter)
#' FDOM.sd    : Standard deviation of Fluorescence of CDOM (Coloured Dissolved Organic Matter)
#' pitch      : Pitch Angle
#' pitch.sd   : Standard deviation of Pitch Angle
#' roll       : Roll Angle
#' roll.sd    : Standard deviation of Roll Angle
#' tilt       : Tilt Angle
#' tilt.sd    : Standard deviation of Tilt Angle
#' heading    : Heading
#'
#' @examples
#' # read an optical data file from IML Buoy dat file
#' setwd(path.package("Rimlbuoy"))
#' raw = read.OPTICD("extdata/IML-4_OPTICD_20160501_00032")
#' str(raw)
#'
#'
#'@author Simon Bélanger
#'
#'@export read.OPTICD
#'



read.OPTICD <- function(filen) {

  con=file(filen,encoding="latin1")
  data=readLines(con)
  close(con)

  nrec = length(data)
  tmp=unlist(strsplit(data, ","))
  tmp.mat = as.data.frame(matrix(tmp, ncol=47, nrow=nrec, byrow=T))

  DateTime = as.POSIXct(paste(tmp.mat$V1, tmp.mat$V2), format="%Y/%m/%d %H:%M:%S", tz="GMT")
  DOY = as.numeric(levels(tmp.mat$V3)[tmp.mat$V3])

  # Extract Lat and Lon
  if (levels(tmp.mat$V4) == "########") {
    lat = rep(NA, nrec)
    lon =  rep(NA, nrec)
  } else {
    lat = convert_str_coord(levels(tmp.mat$V4)[tmp.mat$V4])
    lon = convert_str_coord(levels(tmp.mat$V5)[tmp.mat$V5])
  }




  BioShutter = tmp.mat$V47

  tmp=sapply(tmp.mat[,6:46], function(x) as.numeric(as.character(x)))

  Ed0p = (tmp[,c(1,3,5,7,9,11, 13)])
  Ed0p.sd = tmp[,c(2,4,6,8,10,12,14)]
  colnames(Ed0p) <- c("412","443","490","510","555","669", "683")
  colnames(Ed0p.sd) <- c("412","443","490","510","555","669", "683")

  Lu = tmp[,c(15,17,19,21,23,25,27)]
  Lu.sd = tmp[,c(16,18,20,22,24,26,28)]
  colnames(Lu) <- c("412","443","490","510","555","669", "683")
  colnames(Lu.sd) <- c("412","443","490","510","555","669", "683")

  PAR = tmp[,29]
  PAR.sd = tmp[,30]

  FCHL = tmp[,31]
  FCHL.sd = tmp[,32]

  FDOM = tmp[,33]
  FDOM.sd = tmp[,34]

  heading = tmp[,35]

  pitch = tmp[,36]
  pitch.sd = tmp[,37]

  roll = tmp[,38]
  roll.sd = tmp[,39]

  tilt = tmp[,40]
  tilt.sd = tmp[,41]

  return(list(DateTime=DateTime, DOY=DOY, lat=lat,lon=lon,
              waves = c(412,443,490,510,555,669, 683),
              Ed0p=Ed0p,Ed0p.sd=Ed0p.sd,
              Lu0.86m=Lu, Lu0.86m.sd=Lu.sd,
              PAR=PAR, PAR.sd=PAR.sd,
              FCHL=FCHL, FCHL.sd=FCHL.sd,
              FDOM=FDOM, FDOM.sd=FDOM.sd,
              pitch=pitch, pitch.sd=pitch.sd,
              roll=roll, roll.sd=roll.sd,
              tilt=tilt, tilt.sd=tilt.sd,
              heading=heading))

  }
