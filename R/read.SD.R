#' Reads meteo data coming from IML Buoy and returns a list of field containing all the data stored in the SD file. (see example)
#' @export read.SD
#' @param fname Filename
#' @return file
#' Headers :
#' DateTime   : Time Format DD-MMM-YYYY HH:MM:SS.ss
#' lat        : Latitude (North +ve)
#' lon        : Longitude (East +ve)
#' WSP        : Wind speed
#' RW         : Draft speed
#' DirW       : Direction wind
#' TAIR       : Temperatur air
#' HUM        : Humidity
#' PRES       : Pressure
#' TWAT       : Water temperature
#' COND       : Conductivity
#' SAL        : Salinity
#' DENS       : Density
#' WETS       : Wetstar
#' WETS_sd    : Wetstar standard deviation
#' CDOM       : CdOM fluo
#' CDOM_sd    : CDOM fluo standard deviation
#' PAR        : Photosyntetical active radiation
#' PAR_sd     : Photosyntetical active radiation standard deviation
#'
#'
#' @examples
#' # read an SD data file from IML Buoy dat file
#' setwd(path.package("Rimlbuoy"))
#' raw = read.SD("data/IML-4_SD_20160501_000300.DAT")
#' str(raw)
#'
#'
#'

read.SD = function (filen)
{
  con = file(filen, encoding = "latin1")
  data = readLines(con)
  close(con)
  nrec = length(data)
  tmp = unlist(strsplit(data, ","))
  tmp.mat = as.data.frame(matrix(tmp, ncol = 31, nrow = nrec,
                                 byrow = T))
  DateTime = as.POSIXct(paste(tmp.mat$V1, tmp.mat$V2), format = "%Y/%m/%d %H:%M:%S",
                        tz = "GMT")


  lat = convert_str_coord(levels(tmp.mat$V3)[tmp.mat$V3])

  lon = convert_str_coord(levels(tmp.mat$V4)[tmp.mat$V4])
  WSP =  as.numeric(levels(tmp.mat$V5)[tmp.mat$V5])
  RW =  as.numeric(levels(tmp.mat$V6)[tmp.mat$V6])
  DirW =  as.numeric(levels(tmp.mat$V7)[tmp.mat$V7])
  TAIR =  as.numeric(levels(tmp.mat$V8)[tmp.mat$V8])
  HUM =  as.numeric(levels(tmp.mat$V9)[tmp.mat$V9])
  PRES =  as.numeric(levels(tmp.mat$V10)[tmp.mat$V10])
  TWAT =  as.numeric(levels(tmp.mat$V11)[tmp.mat$V11])


  SAL =  as.numeric(levels(tmp.mat$V12)[tmp.mat$V12])
  DENS =  as.numeric(levels(tmp.mat$V13)[tmp.mat$V13])
  FLUO =  as.numeric(levels(tmp.mat$V14)[tmp.mat$V14])
  CDOM =  as.numeric(levels(tmp.mat$V15)[tmp.mat$V15])
  PAR =  as.numeric(levels(tmp.mat$V16)[tmp.mat$V16])
  WAVEPer =  as.numeric(levels(tmp.mat$V17)[tmp.mat$V17])
  WAVE =  as.numeric(levels(tmp.mat$V18)[tmp.mat$V18])
  WAVEPEAK =  as.numeric(levels(tmp.mat$V19)[tmp.mat$V19])
  VOLT =  as.numeric(levels(tmp.mat$V20)[tmp.mat$V20])
  ELECCONSO =  as.numeric(levels(tmp.mat$V21)[tmp.mat$V21])
  ELECCHARGE =  as.numeric(levels(tmp.mat$V22)[tmp.mat$V22])
  PITCH =  as.numeric(levels(tmp.mat$V23)[tmp.mat$V23])
  ROLL =  as.numeric(levels(tmp.mat$V24)[tmp.mat$V24])
  ADCP =  as.numeric(levels(tmp.mat$V25)[tmp.mat$V25])
  Q =  as.numeric(levels(tmp.mat$V26)[tmp.mat$V26])
  HEADING =  as.numeric(levels(tmp.mat$V27)[tmp.mat$V27])
  SPEEDGPS =  as.numeric(levels(tmp.mat$V28)[tmp.mat$V28])
  DIRGPS =  as.numeric(levels(tmp.mat$V29)[tmp.mat$V29])
  RAINACC =  as.numeric(levels(tmp.mat$V30)[tmp.mat$V30])


  return(list(DateTime = DateTime,lat = lat, lon = lon,
              WSP=WSP, RW=RW,DirW=DirW,TAIR=TAIR,HUM=HUM,PRES=PRES,
              TWAT=TWAT,SAL=SAL,DENS=DENS,FLUO=FLUO,CDOM=CDOM,
              PAR=PAR,WAVEPer=WAVEPer,WAVE=WAVE,WAVEPEAK=WAVEPEAK,VOLT=VOLT,
              ELECCONSO=ELECCONSO,ELECCHARGE=ELECCHARGE,PITCH=PITCH,ROLL=ROLL,ADCP=ADCP,
              Q=Q,HEADING=HEADING,SPEEDGPS=SPEEDGPS,DIRGPS=DIRGPS,RAINACC=RAINACC))
}
