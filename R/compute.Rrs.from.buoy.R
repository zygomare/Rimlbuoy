#' Compute remote sensing reflectance from uncorrected optical data from
#' the IML (VIKING) buoys using Bélanger et al (2016) algorithm
#'
#' Apply shadow correction, extrapol from 0.86m to surface and compute Rrs from raw optical data of IML Buoy
#'
#'
#' @param waves Wavelenghts of Ed0 and LuZ
#' @param Ed0 Spectral irradiance above water
#' @param LuZ Spectral upwelling radiance measured by the sensor at 0.86m
#' below sea surface.
#' @param DateTime A date and time string as POSIXct object
#' @param lon Longitude in degrees (negative for west),
#' @param lat Latitude in degrees
#'
#' @return Returns a data.frame with sun zenith angle (sunzen),
#' remote sensing refectances(Rrs),
#' water leaving radiance (Lw),
#' normalized water leaving radiance (nLw),
#'  and reflectance (rhown)
#'
#'@details The data processing includes mainly two corrections.
#'First the upwelling radiance is corrected for shadow effect using
#'an empirical algorithm developped at the IML4 site during summer 2015.
#'The algorithm is detailed in Bélanger et al
#'(manuscript in preparation, June 2016).
#'
#'
#'
#' @examples
#'
#' #read an optical data file from IML Buoy
#'setwd(path.package("Rimlbuoy"))
#'raw = read.OPTICD("data/IML-4_OPTICD_20160501_000320.DAT")
#'
#' # select an indice. here 65 is 16:30 GMT or 12:30 t local time
#'ix = 65
#'rrs = compute.Rrs.from.buoy(raw$waves,raw$Ed0p[ix,], raw$Lu0.86m[ix,], raw$DateTime[ix], raw$lon[ix], raw$lat[ix])
#'
#' # plot the Rrs spectrum
#'plot(raw$waves, rrs$Rrs, type="l", main=raw$DateTime[ix], ylim=c(0,max(rrs$Rrs, na.rm=TRUE)),sub=paste("Latitude:", signif(raw$lat[ix],6), "   Longitude:", signif(raw$lon[ix],6)), xlab="Wavelength")
#'
#'
#' @export compute.Rrs.from.buoy
#'
compute.Rrs.from.buoy <- function (waves,Ed0, LuZ, DateTime, lon, lat) {

  # extract information from DateTime and compute sun zenith angle
  julian.day <- as.numeric(format(DateTime, format = "%j"))
  month <- as.numeric(format(DateTime, format = "%m"))
  day <- as.numeric(format(DateTime, format = "%d"))
  time.poslt <- as.POSIXlt(DateTime)
  ftime <- time.poslt$hour + time.poslt$min/60 + time.poslt$sec/3600
  sunzen = possol(month,day,ftime,lon,lat)[1]

  # compute Band ratios from uncorrected reflectance
  rrs_uncor = LuZ/Ed0
  R0 = rrs_uncor[1]/rrs_uncor[5]
  R1 = rrs_uncor[2]/rrs_uncor[5]
  R2 = rrs_uncor[3]/rrs_uncor[5]
  R3 = rrs_uncor[4]/rrs_uncor[5]
  R4 = rrs_uncor[7]/rrs_uncor[6]
  R.lee = (rrs_uncor[2]+rrs_uncor[3])/(rrs_uncor[5]+5*rrs_uncor[6]/rrs_uncor[3]*rrs_uncor[6])


  # Use a MRL coefficient to compute diffuse attenuation of LuZ as derived from Bélanger et al in prep
  K = rep(NA, length(waves))
  kappa = mr.coeff.klu2m.r2.adj


  for (i in 1:7) {
    if (colnames(kappa)[i] == "xi") R = R.lee
    if (colnames(kappa)[i] == "412/555") R = R0
    if (colnames(kappa)[i] == "443/555") R = R1
    if (colnames(kappa)[i] == "490/555") R = R2
    if (colnames(kappa)[i] == "510/555") R = R3
    if (colnames(kappa)[i] == "683/670") R = R4

    K[i] =    10^(kappa[1,i] +
                    kappa[2,i]*log10(R) +
                    kappa[3,i]*sunzen)
  }


  # calculate the fraction of diffuse sky using G&C1990 model as implemented in COPS package
  ix490 = which.min(abs(waves-490))
  visibility <- 25
  GreggCarder.d = GreggCarder.data()
  egc <- GreggCarder.f(julian.day, lon, lat , sunzen, lam.sel = waves, Vi=visibility)
  ratio = egc$Ed[ix490]*100/Ed0[ix490]

  # Reduced progressively the visilibity to obtain the rigth
  while (ratio > 1.05 & visibility > 0.5) {
    egc <- GreggCarder.f(julian.day, lon, lat, sunzen,lam.sel = waves, Vi=visibility)
    ratio = egc$Ed[ix490]*100/Ed0[ix490]
    visibility = visibility - 0.5
  }
  Ed0.f = egc$Edif/egc$Ed


  # calculate epsilon from empirical relationships
  c_coef = mr.coeff.epsilon.r2.adj

  epsilon <- rep(NA, length(waves))
  for (i in 1:7) {

    if (colnames(c_coef)[i] == "xi") R = R.lee
    if (colnames(c_coef)[i] == "412/555") R = R0
    if (colnames(c_coef)[i] == "443/555") R = R1
    if (colnames(c_coef)[i] == "490/555") R = R2
    if (colnames(c_coef)[i] == "510/555") R = R3
    epsilon[i] =
      c_coef[1,i] +
      c_coef[2,i] * log10(R) +
      c_coef[3,i] * Ed0.f[i] +
      c_coef[4,i] * sunzen
  }

  # corect LuZ for shadow
  LuZ.corrected = LuZ / (1 - unlist(epsilon))

  # Extrapol corrected LuZ to surface
  Lu.0m = LuZ.corrected/exp(-K*0.86)

  # compute Rrs
  Lw = 0.54 * Lu.0m
  Rrs = Lw / Ed0

  # compute normalised water  reflectance
  rho_wn = pi*Rrs

  # compute normalised water leaving radiance
  # get extraterestrial irradiance at TOA from Thuillier
  F0 = etirr(waves)
  nLw = F0 * Rrs

  return(list(sunzen=sunzen,
              Rrs=Rrs,
              Lw=Lw,
              nLw=nLw,
              rho_wn=rho_wn))

}
