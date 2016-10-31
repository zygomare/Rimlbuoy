#' Reads optics data coming from IML Buoy and returns a list of field containing all the data stored in the OPTIC file. (see example)
#' @export read.OCR_MED
#' @param fname Filename
#' @return list of data
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
#' @examples
#' # read an optical data file from IML Buoy dat file
#' setwd(path.package("Rimlbuoy"))
#' raw = read.OCR_MED("data/IML-4_OCR_MED_20160501_000320.DAT")
#' str(raw)
#'
#'
#'
#'



read.OCR_MED<- function(filen) {

  con=file(filen,encoding="latin1")
  data=readLines(con)
  close(con)

  nrec = length(data)
  tmp=unlist(strsplit(data, ","))
  tmp.mat = as.data.frame(matrix(tmp, ncol=40, nrow=nrec, byrow=T))

  DateTime = as.POSIXct(paste(tmp.mat$V1, tmp.mat$V2), format="%Y/%m/%d %H:%M:%S", tz="GMT")
  DOY = as.numeric(levels(tmp.mat$V3)[tmp.mat$V3])

  # Extract Lat and Lon
  lat = convert_str_coord(levels(tmp.mat$V4)[tmp.mat$V4])
  lon = convert_str_coord(levels(tmp.mat$V5)[tmp.mat$V5])


  BioShutter = tmp.mat$V40

  tmp=sapply(tmp.mat[,6:39], function(x) as.numeric(as.character(x)))

  Ed0p = (tmp[,c(1:7)])
  colnames(Ed0p) <- c("412","443","490","510","550","670", "683")
#a modifier pour fitter avec les bonnes colonnes
  Lu = tmp[,c(8:14)]
  colnames(Lu) <- c("412","443","490","510","550","670", "683")

  PAR = tmp[,22]
  PAR.sd = tmp[,23]

  FCHL = tmp[,24]
  FCHL.sd = tmp[,25]

  FDOM = tmp[,26]
  FDOM.sd = tmp[,27]

  heading = tmp[,28]

  pitch = tmp[,29]
  pitch.sd = tmp[,30]

  roll = tmp[,31]
  roll.sd = tmp[,32]

  tilt = tmp[,33]
  tilt.sd = tmp[,34]

  return(list(DateTime=DateTime, DOY=DOY, lat=lat,lon=lon,
              waves = c(412,443,490,510,550,670, 683),
              Ed0p=Ed0p,
              Lu0.86m=Lu,
              PAR=PAR, PAR.sd=PAR.sd,
              FCHL=FCHL, FCHL.sd=FCHL.sd,
              FDOM=FDOM, FDOM.sd=FDOM.sd,
              pitch=pitch, pitch.sd=pitch.sd,
              roll=roll, roll.sd=roll.sd,
              tilt=tilt, tilt.sd=tilt.sd,
              heading=heading))

}
