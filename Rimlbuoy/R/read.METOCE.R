#' Reads METOCE file coming from IML Buoy
#'
#' The METOCE file contains 20 parameters: meteorological data
#' (wind speed, raffale, wind direction,
#' air temperature, pressure, PAR),
#' oceanographic data (water temperature, conductivity, salinity),
#' fluorescence (FDOM and FCHL), GPS, and tilt and heading information.
#'
#' @param fname Filename
#'
#' @return  It returns a list of fields containing all the data
#' stored in the METOCE file.
#'
#' @author Simon Bélanger
#' @export


read.METOCE <- function(filen) {

  #print(paste("Read", filen))
  #read the ODF file
  con=file(filen,encoding="latin1")
  data=readLines(con)
  close(con)

  nrec = length(data)
  tmp=unlist(strsplit(data, ","))
  tmp.mat = as.data.frame(matrix(tmp, ncol=21, nrow=nrec, byrow=T))

  DateTime = as.POSIXct(paste(tmp.mat$V1, tmp.mat$V2), format="%Y/%m/%d %H:%M:%S", tz="GMT")
  DOY = as.numeric(levels(tmp.mat$V3)[tmp.mat$V3])

  # Extract Lat and Lon
  lat = convert_str_coord(levels(tmp.mat$V4)[tmp.mat$V4])
  lon = convert_str_coord(levels(tmp.mat$V5)[tmp.mat$V5])


  tmp=sapply(tmp.mat[,6:21], function(x) as.numeric(as.character(x)))

  #print(dim(tmp))
  wind.speed = tmp[,1]
  raffale = tmp[,2]
  wind.direction = tmp[,3]
  temperature.air = tmp[,4]
  humidity = tmp[,5]
  pressure = tmp[,6]
  temperature.water = tmp[,7]
  conductivity = tmp[,8]
  salinity = tmp[,9]
  density = tmp[,10]
  FCHL = tmp[,11]
  FCHL.sd = tmp[,12]
  FDOM = tmp[,13]
  FDOM.sd = tmp[,14]
  PAR = tmp[,15]
  PAR.sd = tmp[,16]

  return(data.frame(DateTime=DateTime, DOY=DOY, lat=lat,lon=lon,
              wind.speed = wind.speed,
              raffale = raffale,
              wind.direction = wind.direction,
              temperature.air = temperature.air,
              humidity = humidity,
              pressure = pressure,
              temperature.water = temperature.water,
              conductivity = conductivity,
              salinity = salinity,
              density = density,
              PAR=PAR, PAR.sd=PAR.sd,
              FCHL=FCHL, FCHL.sd=FCHL.sd,
              FDOM=FDOM, FDOM.sd=FDOM.sd))

}
