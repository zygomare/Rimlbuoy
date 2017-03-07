#' Generate an ASCII file for the MERMAID data base containing
#' dark corrected remote sensing reflectance and water leaving radiance from the
#' IML buoy.
#'
#' This routine takes two files coming from the buoys as input, then apply
#' a number of quality control check and compute Rrs following the method detailed
#' in Belanger et al.
#'
#' @param OPTICD_fname Name of the OPTICD data file IML buoy containing
#' mean and standard deviation for each OCR channels.
#' @param outputdir Path for the ouptut directory where the
#' results will be saved in ASCII format.
#' @param Tilt_threshold is the threshold for maximum tilt angle tolerance.
#' Default value is 10.
#' @param USE.MEDIAN is a logical parameter indicating whether the Rrs is
#' cumputed using median values stored OCR_MED files. If TRUE, the file name
#' of the output will begin by rrs_med_.
#' Default is FALSE.
#' @param PI : pi full name
#' @param PI_Email : pi email adress
#' @param land_dist_IS : In-situ measurement distance to the land
#' @param PQC : Processing quality control from MERMAID (see MERMAID data format report from ACRI-ST)
#' @param MQC : Measurement Quality Control from MERMAID (see MERMAID data format report from ACRI-ST)
#' @param USE.BANDSHIFT.COEF is a logical parameter indicating whether the bandshift coefficient must
#' be used if TRUE the following parameter are needed (band.width,waves2,band.width2).
#' Default is TRUE.
#' @param band.width : bandwidth of the reference wavelength (numeric vector).
#' @param waves2 : target wavelength for the bandshift correction (numeric vector).
#' @param band.width2 : bandwidth of a target wavelength (numeric vector).
#' @details This function reads the data file provided as input,
#' apply quality control checks and then compute Rrs using \code{\link{compute.Rrs.from.buoy}}.
#' Next ìf USE.BANDSHIFT.COEF is true, the bandshift correction is apply to Lw and rho_w. Then the
#' flags for coefficient of variation (<20%) and tilt (<10°) are identified and excluded. Then the final
#' data frame is formated with MERMAID header.
#'
#'
#' @return It writes an csv file names rrs_DATE.csv (or rrs_med_DATE.csv) and
#' returns a data frame in the command line.
#'
#' @examples
#'#Define file names
#'setwd(path.package("Rimlbuoy"))
#'pathwd="extdata/"
#'fnames = list.files(pathwd,full.names = T)
#'fn=fnames[grep("OPTICD",fnames)]
#'
#'#Define threshold for tilt flag
#'Tilt_threshold = 10
#'band.width=rep(10,7)
#'waves2= c(560,665,681)
#'band.width2 = rep(10,3)
#'PI="Simon_Belanger"
#'PI_Email ="sbe@arctus.ca"
#'land_dist_IS = 20
#'PQC = "P000010000"
#'MQC="M110111001101020200"
#'
#
#'
#'#Define output dir
#' output_dir_data="extdata/"
#'
#'#Run write.to.MERMAID
#'res = write.to.MERMAID(OPTICD_fname=fn,outputdir=output_dir_data,Tilt_threshold=Tilt_threshold)
#'res.med = write.to.MERMAID(OPTICD_fname=fn,outputdir=output_dir_data,Tilt_threshold=Tilt_threshold,USE.MEDIAN=T)
#'str(res)
#'
#' @export write.to.MERMAID
#'
#' @author Thomas Jaegler modified by Simon Belanger

write.to.MERMAID <- function(OPTICD_fname,
                             outputdir,
                             Tilt_threshold=10,
                             USE.MEDIAN=FALSE,
                             PI,
                             PI_Email,
                             land_dist_IS,
                             PQC,
                             MQC,
                             USE.BANDSHIFT.COEF=TRUE,
                             band.width,
                             waves2,
                             band.width2) {

  # Extract information from the file name
  base = basename(OPTICD_fname)
  inpath = dirname(OPTICD_fname)
  BuoyID = paste(str_sub(base,1,4),str_sub(base,5,5),sep="")
  idexdat= str_sub(base,13,28)
  #######

  # Skip missing function between R3.02 and R3.03
  anyNA <- function(x) any(is.na(x))
  #######


  # Read file
  raw = read.OPTICD(OPTICD_fname)

  if (USE.MEDIAN) { # This will allows the possibility to process archived data when MED was not available
    OCR_MED_fname = paste(inpath,"/",BuoyID, "_OCR_MED",idexdat,".DAT",sep="")
    raw_med = read.OCR_MED(OCR_MED_fname)
  }

  ######

  # Detect if buoy coordinate is present if not define coordinate
  #####
  if (is.na(as.numeric(raw$lat))){
    if (BuoyID == "IML4"){
      raw$lat= rep(convert_str_coord("48° 40.002' N"),length(raw$DOY))
      raw$lon = rep(convert_str_coord("68° 34.998' O"),length(raw$DOY))
    }else if(BuoyID == "IML6"){
      raw$lat = rep(convert_str_coord("47° 46.998' N"),length(raw$DOY))
      raw$lon = rep(convert_str_coord("64° 1.998' O"),length(raw$DOY))
    }else if(BuoyID == "IML10"){
      raw$lat= rep(convert_str_coord("48° 0' N"),length(raw$DOY))
      raw$lon= rep(convert_str_coord("60° 30' O"),length(raw$DOY))
    }
  } else {
    # SB: Il faudrait en mettre plus des print() et les sauvegarder dans un fichier .log
    # a faire
    print("GPS OK")
  }

  #same for med
  if (USE.MEDIAN) { # This will allows the possibility to process archived data when MED was not available
    if (is.na(as.numeric(raw$lat))){
      if (BuoyID == "IML4"){
        raw$lat= raw_med$lat = rep(convert_str_coord("48° 40.002' N"),length(raw$DOY))
        raw$lon =  raw_med$lon = rep(convert_str_coord("68° 34.998' O"),length(raw$DOY))
      }else if(BuoyID == "IML6"){
        raw$lat= raw_med$lat = rep(convert_str_coord("47° 46.998' N"),length(raw$DOY))
        raw$lon = raw_med$lon = rep(convert_str_coord("64° 1.998' O"),length(raw$DOY))
      }else if(BuoyID == "IML10"){
        raw$lat= raw_med$lat = rep(convert_str_coord("48° 0' N"),length(raw$DOY))
        raw$lon = raw_med$lon = rep(convert_str_coord("60° 30' O"),length(raw$DOY))
      }
    } else {
      # SB: Il faudrait en mettre plus des print() et les sauvegarder dans un fichier .log
      # a faire
      print("GPS OK")
    }

  }
  #####

  # Find the index falling within the two hours of solar noon
  #####
  GMT_noon =  as.POSIXct(paste(strftime(raw$DateTime, format = "%Y-%m-%d")[1],"12:00:00", sep = " "),
                         format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  diff.GMT.noon.in.sec <- mean(raw$lon, na.rm = T)/360*24 * 3600
  solar.noon.time = GMT_noon - diff.GMT.noon.in.sec

  # get the first and last indices retained for the processing
  First_time = abs(difftime(solar.noon.time - 7200, raw$DateTime))
  First_time_ix = which.min(First_time)
  Last_time = abs(difftime(solar.noon.time + 7200, raw$DateTime))
  Last_time_ix = which.min(Last_time)

  # Create a vector of good indices
  ix.good = First_time_ix:Last_time_ix
  ngood = length(ix.good)
  #####


  # Find the index falling within the one hours of local midnight
  #####
  GMT_midnight =  as.POSIXct(paste(strftime(raw$DateTime, format = "%Y-%m-%d")[1],"04:00:00", sep = " "),
                             format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

  # get the first and last indices retained for the processing
  First_time_night = abs(difftime(GMT_midnight - 3600, raw$DateTime))
  First_time_ix_night = which.min(First_time_night)
  Last_time_night = abs(difftime(GMT_midnight + 3600, raw$DateTime))
  Last_time_ix_night = which.min(Last_time_night)

  # Create a vector of good indices
  ix.good_night= First_time_ix_night:Last_time_ix_night
  ngood_night = length(ix.good_night)
  #####

  #Make Dark readings
  ####
  if (USE.MEDIAN) {
    dark.read.med.Lu=apply(raw_med$Lu0.86m[ix.good_night,],2,median,na.rm=T)
  }else{
    dark.read.Lu=apply(raw$Lu0.86m[ix.good_night,],2,median,na.rm=T)
  }
  ###

  #Make Dark correction
  ####
  if (USE.MEDIAN) {
    dark.med.Lu=raw_med$Lu0.86m-dark.read.med.Lu
  }else{
    dark.Lu=raw$Lu0.86m-dark.read.Lu
  }
  ###

  #####Calculate coefficient of variation for Ed and define new variable for Ed_flag
   Ed0p.cv <- raw$Ed0p.sd[ix.good,]/raw$Ed0p[ix.good,]
   Ed0p.cv.flag <- Ed0p.cv > 0.2
  #######

  ######Calculate coefficient of variation for Lu and define new variable for Lu_flag
   Lu0.86m.cv <- raw$Lu0.86m.sd[ix.good,]/dark.Lu[ix.good,]
   Lw.cv.flag <- Lu0.86m.cv > 0.2 | is.na(Lu0.86m.cv)
  ###################
  ######### Detect tilt above threshold and create variable flag
   tilt.flag <- raw$tilt[ix.good] > Tilt_threshold
   ###################



  # If there is any data within the two hours before or after the Solar Noon proceed, otherwise stop processing
  if (ngood > 1) {
    # Create matrices or vectors to store the data
    rho_wn = matrix(NA, ncol=7, nrow=ngood)
    sunzen = rep(NA, ngood)
    Lw = matrix(NA, ncol=7, nrow=ngood)


    # Loop on each record between solar noon +/- 2 hours
    igood.rec = 0
    for (i in ix.good){
      igood.rec <- igood.rec + 1 # this in crement is needed to write in the new matrices and vectors
      print(paste("Process :", raw$DateTime[i]))

      # check if the curent record is complete
      if (USE.MEDIAN) {
        complete = (!anyNA(raw_med$Ed0p[i,]) & !anyNA(raw_med$Lu0.86m[i,]))
      }
      else complete = (!anyNA(raw$Ed0p[i,]) & !anyNA(raw$Lu0.86m[i,]))

      if (complete) {
        if (USE.MEDIAN) {
          res = compute.Rrs.from.buoy(raw$waves,
                                      raw_med$Ed0p[i,],
                                      dark.med.Lu[i,],
                                      raw$DateTime[i],
                                      raw$lon[i],
                                      raw$lat[i])
        } else res = compute.Rrs.from.buoy(raw$waves,
                                           raw$Ed0p[i,],
                                           dark.Lu[i,],
                                           raw$DateTime[i],
                                           raw$lon[i],
                                           raw$lat[i])

        rho_wn[igood.rec, ] <- res$rho_wn
        Lw[igood.rec, ] <- res$Lw
        sunzen[igood.rec] <- res$sunzen

        ######Make bandshift correction

        if (complete) { #get info if the data is complete
          if (USE.BANDSHIFT.COEF) {
            bandshift.fact=get.bandshift.coefficient(res$Rrs,raw$waves,band.width,waves2,band.width2)

            for (i in c(1:length(waves2))){

              ix.wl=which.min(abs(waves2[i]-raw$waves))

              rho_wn[igood.rec, ix.wl]=rho_wn[igood.rec,ix.wl]*bandshift.fact[i]
              Lw[igood.rec,ix.wl ]=Lw[igood.rec, ix.wl]*bandshift.fact[i]
            }

          }else{
            print("DO NOT apply bandshift coefficient")
          }

        } else { # If it is not complete the variable will take NA values
          # The processing will continue with no error
          print("Some data is missing")
          print(raw$Ed0p[i,])
          print(raw$Lu0.86m[i,])

        }

      } else { # If it is not complete the variable will take NA values
        # The processing will continue with no error
        print("Some data is missing")
        print(raw$Ed0p[i,])
        print(raw$Lu0.86m[i,])

      }
    }



    #make some necesary Header ID for Mermaid submission
    MATCHUP_ID = paste(BuoyID,"_0",seq(1,length.out = length(raw$DateTime[ix.good])),"_",idexdat,sep="")
    site = rep(BuoyID,length(raw$DateTime[ix.good]))
    PI=rep(PI,length(raw$DateTime[ix.good]))
    PI_Email =rep(PI_Email ,length(raw$DateTime[ix.good]))
    land_dist_IS = rep(land_dist_IS,length(raw$DateTime[ix.good]))
    waves.fin=raw$waves


    if (nchar(PQC) == 10){
      PQC = rep(PQC,length(raw$DateTime[ix.good]))
    }else{
      print("Something wrong with PQC flags see Mermaid data format.")
    }

    if (nchar(MQC) == 19){
      MQC=  rep(MQC,length(raw$DateTime[ix.good]))
    }else{
      print("Something wrong with MQC flags see Mermaid data format.")
    }

    # Add new data and flags to the data frame
    result.df=data.frame(MATCHUP_ID,site,PI,PI_Email, # split the date and time for the
                         raw$lat[ix.good],
                         raw$lon[ix.good],
                         format(raw$DateTime[ix.good], format="%Y%m%dT%H%M%SZ"),
                         PQC,
                         MQC,
                         land_dist_IS,
                         sunzen,
                         Ed0p=raw$Ed0p[ix.good,],
                         Lw,
                         rho_wn,
                         raw$tilt[ix.good],
                         raw$FCHL[ix.good],
                         raw$FDOM[ix.good],
                         apply(Ed0p.cv.flag,2,as.numeric), # the flags are put at the end
                         apply(Lw.cv.flag,2,as.numeric),
                         as.numeric(tilt.flag))

    if (USE.BANDSHIFT.COEF){


      for (i in c(1:length(waves2))){
        ix.wl=which.min(abs(waves2[i]-raw$waves))
        waves.fin[ix.wl]=waves2[i]
      }
      # add names to the data frame # this is the most complicated part of the code!
      colnames(result.df) = c("MATCHUP_ID","Site","PI","PI(s)_Email","Lat_IS", "Lon_IS",
                              "TIME_IS",
                              "PQC",
                              "MQC",
                              "land_dist_IS",
                              "thetas_IS",
                              paste("Es_IS",raw$waves,sep="_"),
                              paste("Lw_IS",waves.fin,sep="_"),
                              paste("rho_w_IS",waves.fin,sep="_"),
                              "tilt",
                              "Fluor_chla_IS",
                              "Fluor_cdom_IS",
                              paste("Es_IS.cv.flag",waves.fin,sep="_"),
                              paste("Lw_IS.cv.flag",waves.fin,sep="_"),
                              "tilt_flag"
      )

    }else{
      colnames(result.df) = c("MATCHUP_ID","Site","PI","PI(s)_Email","Lat_IS", "Lon_IS",
                              "TIME_IS",
                              "PQC",
                              "MQC",
                              "land_dist_IS",
                              "thetas IS",
                              paste("Es_IS",raw$waves,sep="_"),
                              paste("Lw_IS",waves.fin,sep="_"),
                              paste("rho_w_IS",waves.fin,sep="_"),
                              "tilt",
                              "Fluor_chla_IS",
                              "Fluor_cdom_IS",
                              paste("Es_IS.cv.flag",raw$waves,sep="_"),
                              paste("Lw_IS.cv.flag",raw$waves,sep="_"),
                              "tilt_flag"
      )
    }



    # Select non-flagged data's
    result.df.index=which(result.df[,c(1:11,36:50)]==1,arr.ind = T)
    if( length(result.df.index)==0){
      result.df.MERMAID=result.df[,c(1:32)]
    }else{
      dfdata=result.df[,c(1:32)]
      for(i in c(1:nrow(result.df.index))){
        dfdata[result.df.index[i,1],result.df.index[i,2]] =NA

      }
      for(i in c(1:nrow(result.df.index))){
        if (result.df.index[i,2]<19){
        dfdata[result.df.index[i,1],result.df.index[i,2]+14] =NA
        }else{
        dfdata[result.df.index[i,1],result.df.index[i,2]+7] =NA

        }
      }

      result.df.MERMAID=dfdata

    }
    ####

    #
    if (length(row.names(result.df.MERMAID)) == 0){
      print("NO data in the time frame")
      return(0)
    }else{
    }
    # Write the data frame in an ASCII file
    if (USE.MEDIAN) {
      outfile <- paste(outputdir, "/rrs_med_", BuoyID,idexdat, ".csv", sep = "")
    }else outfile <- paste(outputdir, "/rrs_",  BuoyID,idexdat, ".csv", sep = "")
    write.table(result.df.MERMAID, file=outfile, sep = ";", row.names = F, quote = F)

    return(result.df.MERMAID)
  }else {
    print("NO data in the time frame")
    return(0)
  }
}

