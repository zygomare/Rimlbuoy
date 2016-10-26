#' Read SGDO ODF file and output list similar to
#' read.OPTICD in the Rimlbuoy package
#'
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
#' # read an optical data file from IML Buoy ODF file
#' setwd(path.package("Rimlbuoy"))
#' raw = read.OGSL.OPTICD("data/MMOB_BOUEE2013_RIMOUSKI_IML4_OPTICD.ODF")
#' str(raw)
#'
#'


read.OGSL.OPTICD <- function(fname){
  #read the ODF file
  con=file(fname,encoding="latin1")
  data=readLines(con)
  close(con)
  #find the name of the data in the file
  header =data[grep("NAME=",data)][-c(1:6)]
  units =data[grep("UNITS=",data)]

  #find the line of the character --DATA--
  line=grep("-- DATA --",data)

  #read and write to a variable list the data
  test=strsplit(data[c((line+1):length(data))],"\\ " )

  #split the data to eliminate the blank column
  getdata.func=function(x){ x[which( nchar(x) != 0)] }
  list.tmp=lapply(test,getdata.func)


  #read and write the time and date
  organize.time_date=function(x) {paste(x[1],x[2],sep=" ")}
  list.time_date=lapply(list.tmp,organize.time_date)
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  DateTime = as.POSIXct(gsub("'","",unlist(list.time_date)), format = "%d-%b-%Y %H:%M:%S", tz = "GMT")

  #read header and create for each header a list of data
  list.data = lapply(list.tmp,"[",-c(1,2))
  df <- matrix(as.numeric(unlist(list.data)), ncol=length(header)-1, byrow=T)

 # get index for each element in the header
 header_2=header[-1]
 ixlon=grep("Longitude",header_2)
 ixlat=grep("Latitude",header_2)
 ixEd=grep("Irradiance",header_2)
 ixEd_flag=grep("flag",header_2[ixEd])
 ixEd_sd=grep("standard deviation",header_2[ixEd[-ixEd_flag]])

 ixLu=grep("Radiance",header_2)
 ixLu_flag=grep("flag",header_2[ixLu])
 ixLu_sd=grep("standard deviation",header_2[ixLu[-ixLu_flag]])

 ixPAR=grep("Photosynthetic Active Radiation",header_2)
 ixPAR_flag=grep("flag",header_2[ixPAR])
 ixPAR_sd=grep("standard deviation",header_2[ixPAR[-ixPAR_flag]])

 # ATTENTION : Je pense que lea FDOM et FCHL est inversé
 # dans le header.
 ixFCHL=grep("Fluorescence'",header_2)
 ixFCHL_flag=grep("flag",header_2[ixFCHL])
 ixFCHL_sd=grep("Fluorescence standard deviation",header_2)
 ixFCHL_sd_flag=grep("flag",header_2[ixFCHL_sd])


 ixFCDOM=grep("Fluorescence of CDOM ",header_2)
 ixFCDOM_flag=grep("flag",header_2[ixFCDOM])
 ixFCDOM_sd=grep("standard deviation",header_2[ixFCDOM[-ixFCDOM_flag]])

 ixheading=grep("Heading",header_2)
 ixheading_flag=grep("flag",header_2[ixheading])


 ixpitch=grep("Pitch",header_2)
 ixpitch_flag=grep("flag",header_2[ixpitch])
 ixpitch_sd=grep("standard deviation",header_2[ixpitch[-ixpitch_flag]])

 ixroll=grep("Roll",header_2)
 ixroll_flag=grep("flag",header_2[ixroll])
 ixroll_sd=grep("standard deviation",header_2[ixroll[-ixroll_flag]])

 ixtilt=grep("Tilt",header_2)
 ixtiltflag=grep("flag",header_2[ixtilt])
 ixtilt_sd=grep("standard deviation",header_2[ixtilt[-ixtiltflag]])

 # get element in the dataframe
 lat = df[,ixlat]
 lon =  df[,ixlon]

 Ed0p =df[,ixEd[-ixEd_flag]][,-ixEd_sd]
 Ed0p.sd = df[,ixEd[-ixEd_flag]][,ixEd_sd]

 colnames(Ed0p) <- c("412", "443", "490", "510", "555", "669",
                     "683")
 colnames(Ed0p.sd) <- c("412", "443", "490", "510", "555",
                        "669", "683")

 Lu =df[,ixLu[-ixLu_flag]][,-ixLu_sd]
 Lu.sd = df[,ixLu[-ixLu_flag]][,ixLu_sd]

 colnames(Lu) <- c("412", "443", "490", "510", "555", "669",
                     "683")
 colnames(Lu.sd) <- c("412", "443", "490", "510", "555",
                        "669", "683")


 PAR = df[,ixPAR[-ixPAR_flag]][,-ixPAR_sd]
 PAR.sd = df[,ixPAR[-ixPAR_flag]][,ixPAR_sd]

 FCHL = df[,ixFCHL[-ixFCHL_flag]]
 FCHL.sd =df[,ixFCHL_sd[-ixFCHL_sd_flag]]

 FDOM =df[,ixFCDOM[-ixFCDOM_flag]][,-ixFCDOM_sd]
 FDOM.sd =df[,ixFCDOM[-ixFCDOM_flag]][,ixFCDOM_sd]

 heading =df[,ixheading[-ixheading_flag]]

 pitch =df[,ixpitch[-ixpitch_flag]][,-ixpitch_sd]
 pitch.sd =df[,ixpitch[-ixpitch_flag]][,ixpitch_sd]

 roll =df[,ixroll[-ixroll_flag]][,-ixroll_sd]
 roll.sd =df[,ixroll[-ixroll_flag]][,ixroll_sd]

 tilt = df[,ixtilt[-ixtiltflag]][,-ixtilt_sd]
 tilt.sd =df[,ixtilt[-ixtiltflag]][,ixtilt_sd]

 DOY = rep(NA,length(DateTime))




 return(list(DateTime = DateTime, DOY = DOY, lat = lat, lon = lon,
             waves = c(412, 443, 490, 510, 555, 669, 683), Ed0p = Ed0p,
             Ed0p.sd = Ed0p.sd, Lu0.86m = Lu, Lu0.86m.sd = Lu.sd,
             PAR = PAR, PAR.sd = PAR.sd, FCHL = FCHL, FCHL.sd = FCHL.sd,
             FDOM = FDOM, FDOM.sd = FDOM.sd, pitch = pitch, pitch.sd = pitch.sd,
             roll = roll, roll.sd = roll.sd, tilt = tilt, tilt.sd = tilt.sd,
             heading = heading))
}
