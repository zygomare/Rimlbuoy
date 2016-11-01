#' Get bandshift coefficient to remote sensing reflectance data from
#' the IML (VIKING) buoys using Zibordi et al (2009) algorithm
#
#'
#' @param Rrs0 : reference remote sensing reflectance that need to be coorected for bandshift (numeric vector).
#' @param waves0 : reference wavelength (numeric vector).
#' @param band.width0 : bandwidth of the reference wavelength (numeric vector).
#' @param waves : target wavelength for the bandshift correction (numeric vector).
#' @param band.width : bandwidth of a target wavelength (numeric vector).
#'
#'
#' @return Return list of data with the bandshift coefficient the length of
#' the list is the length of the reference wavelength. Each element have the length of
#' the bandwith center on the targeted wavelength.
#'
#' @details The correction use the coefficient of the linear regression between
#' in-situ remote sensing ratio and in-situ IOP measurement of bbp, Ag, A_{nap},
#' A_{ph} from the 2015 mission on the IML4 buoy. The correction protocol is describe
#' in Zibordi et al. (2009). The coefficient of the linear regression are used to estimate empirical
#' modelisation of the IOP to calculate total absorption and total backscattering. Then the ratio of
#' IOP and reference remote sensing reflectance is used as Eq.7 of Zibordi et al. (2009) to apply the
#' bandshift corrections.
#'
#'   \deqn{Rrs(\lambda)= Rrs(\lambda 0)*\frac{b_{b}(\lambda)}{a(\lambda)+b_{b}(\lambda)}*\frac{a(\lambda 0)+b_{b}(\lambda 0)}{b_{b}(\lambda 0)}}{%
#'   Rrs(\lambda)= Rrs(\lambda 0)
#'   *b_{b}(\lambda)/(a(\lambda)+b_{b}(\lambda))
#'   *(a(\lambda 0)+b_{b}(\lambda 0))/b_{b}(\lambda 0)}
#'
#'
#'  References :
#'
#'
#'  Zibordi, G., et al., Validation of satellite ocean color primary products at optically complex coastal sites: Northern
#'  Adriatic Sea, Northern Baltic Proper and Gulf of Finland, Remote Sensing of Environment (2009).
#'
#' @examples
#'
#' #read an optical data file from IML Buoy
#' setwd(path.package("Rimlbuoy"))
#' raw = read.OPTICD("data/IML-4_OPTICD_20160501_000320.DAT")
#'
#' #
#' waves0=c(412,443,490,510,555,670, 683)
#' band.width0 = rep(10,7)
#'
#' waves = c(560,665,681)
#' band.width = rep(10,3)
#' ix = 65
#' rrs = compute.Rrs.from.buoy(raw$waves,raw$Ed0p[ix,], raw$Lu0.86m[ix,], raw$DateTime[ix], raw$lon[ix], raw$lat[ix])
#'
#'  bdsfh.coef=get.bandshift.coefficient(rrs$Rrs,waves0,band.width0,waves,band.width)
#'
#'
#'
#'
#' @author Thomas Jaegler
#'
#' @export get.bandshift.coefficient

get.bandshift.coefficient <- function(Rrs0,
                                       waves0,
                                       band.width0,
                                       waves,
                                       band.width) {

  load(paste(path.package("Rimlbuoy"),"/data/coef.Anap.Rdata",sep=""))
  load(paste(path.package("Rimlbuoy"),"/data/coef.Ag.Rdata",sep=""))
  load(paste(path.package("Rimlbuoy"),"/data/coef.Aph.Rdata",sep=""))
  load(paste(path.package("Rimlbuoy"),"/data/coef.bbp.Rdata",sep=""))

  nb.waves0 = length(waves0)

  if (nb.waves0 != length(Rrs0)){
    print("Rrs0 and waves0 vectors have diffent length!!")
    return(0)
  }

  if (nb.waves0 != length(band.width0)){
    print("band.width0 and waves0 vectors have diffent length!!")
    return(0)
  }

  nb.waves = length(waves)
  if (nb.waves != length(band.width)){
    print("band.width and waves vectors have diffent length!!")
    return(0)
  }

  ####create empty list for collecting result
  bdsh.factor=list()
  ###

  #### Run over each wavelength

  for (i in 1:nb.waves) {
    print(paste("Apply bandshift correction to get Rrs at:", waves[i]))

    ix.waves0 = which.min(abs(waves[i] - waves0))
    waves0.vec = (waves0[ix.waves0]-band.width0[ix.waves0]/2):(waves0[ix.waves0]+band.width0[ix.waves0]/2)
    waves.vec =  (waves[i]-band.width[i]/2):(waves[i]+band.width[i]/2)

    ####section total scattering
    ####backscattering calculation
    ix.490=which.min(abs(as.numeric(unname(coef.bbp[1]))-as.numeric(names(Rrs0))))
    ix.555=which.min(abs(as.numeric(unname(coef.bbp[2]))-as.numeric(names(Rrs0))))

    R_490.555=Rrs0[ix.490]/Rrs0[ix.555]
    bbp.ref=coef.bbp[4]+coef.bbp[3]*R_490.555

    bbp.sp0=spectral.bbp(waves0.vec,10^as.numeric(unname(bbp.ref)),wl.ref = 532
                         ,nu=as.numeric(unname(coef.bbp[5])))

    bbp.sp=spectral.bbp(waves.vec,10^as.numeric(unname(bbp.ref)),wl.ref = 532
                        ,nu=as.numeric(unname(coef.bbp[5])))

    #####water scattering
    bb.w.sp0=spectral.bw(waves0.vec)
    bb.w.sp=spectral.bw(waves.vec)


    #####total back scattering calculation
    bb.total.lmbd0=bb.w.sp0+bbp.sp0
    bb.total.lmbd=bb.w.sp+bbp.sp

    ###
    ####total absorption section

    ####Anap Absorption
    ix.490=which.min(abs(as.numeric(unname(coef.Anap[1]))-as.numeric(names(Rrs0))))
    ix.665=which.min(abs(as.numeric(unname(coef.Anap[2]))-as.numeric(names(Rrs0))))

    R_490.665=Rrs0[ix.490]/Rrs0[ix.665]
    anap.ref=coef.Anap[4]+coef.Anap[3]*R_490.665

    anap.sp0=spectral.nap(waves0.vec,10^as.numeric(unname(anap.ref)),wl.ref = 412
                          ,S=as.numeric(unname(coef.Anap[5])))

    anap.sp=spectral.nap(waves.vec,10^as.numeric(unname(anap.ref)),wl.ref = 412
                         ,S=as.numeric(unname(coef.Anap[5])))

    ######pure water absorption
    a.w.sp0=spectral.aw(waves0.vec)
    a.w.sp=spectral.aw(waves.vec)

    ######Yellow absorption

    ix.490=which.min(abs(as.numeric(unname( coef.Ag[1]))-as.numeric(names(Rrs0))))
    ix.665=which.min(abs(as.numeric(unname( coef.Ag[2]))-as.numeric(names(Rrs0))))

    R_490.665=Rrs0[ix.490]/Rrs0[ix.665]
    ag.ref= coef.Ag[4]+ coef.Ag[3]*R_490.665

    ag.sp0=spectral.cdom(waves0.vec,10^as.numeric(unname(ag.ref)),wl.ref = 400
                         ,S=as.numeric(unname(coef.Ag[5])))

    ag.sp=spectral.cdom(waves.vec,10^as.numeric(unname(ag.ref)),wl.ref = 400
                        ,S=as.numeric(unname(coef.Ag[5])))


    ######Phyto absorption

    ix.490=which.min(abs(unique(coef.Aph$waves1)-as.numeric(names(Rrs0))))
    ix.555=which.min(abs(unique(coef.Aph$waves2)-as.numeric(names(Rrs0))))

    R_490.555=Rrs0[ix.490]/Rrs0[ix.555]
    aph.ref= coef.Aph[,3]+ coef.Aph[,2]*R_490.555

    ix.wave0.strt.aph=which.min(abs(head(waves0.vec)[1]-coef.Aph$aph.waves))
    ix.wave0.end.aph=which.min(abs(tail(waves0.vec,n=1)-coef.Aph$aph.waves))

    aph.sp0=10^aph.ref[ix.wave0.strt.aph:ix.wave0.end.aph]

    ix.wave.strt.aph=which.min(abs(head(waves.vec)[1]-coef.Aph$aph.waves))
    ix.wave.end.aph=which.min(abs(tail(waves.vec,n=1)-coef.Aph$aph.waves))

    aph.sp=10^aph.ref[ix.wave.strt.aph:ix.wave.end.aph]


    #####total absorption calculation
    a.total.lmbd0 = aph.sp0+ag.sp0+a.w.sp0+anap.sp0
    a.total.lmbd = aph.sp+ag.sp+a.w.sp+anap.sp


    ###


    bdsh.factor[[i]]=(bb.total.lmbd/(a.total.lmbd0+bb.total.lmbd))*((a.total.lmbd0+bb.total.lmbd0)/bb.total.lmbd0)

  }
  names(bdsh.factor) = as.character(waves)
  return(bdsh.factor)

}
