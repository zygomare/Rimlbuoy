#' Get bandshift coefficient to correct remote sensing reflectance.
#'
#'  Some of the differences in remote sensing reflectance between sensors
#'  result from differences in wavebands characteristic (spectral and band width).
#'  The method is modified from Zibordi et al (2009) based on empirical relationships
#'  determined in the St Lawrence Estuary.
#
#'
#' @param Rrs0 : reference remote sensing reflectance that need to be coorected for bandshift (numeric vector).
#' @param waves0 : reference wavelength (numeric vector).
#' @param band.width0 : bandwidth of the reference wavelength (numeric vector).
#' @param waves : target wavelength for the bandshift correction (numeric vector).
#' @param band.width : bandwidth of a target wavelength (numeric vector).
#'
#'
#' @return Return bandshift coefficients that need to be apply to  The length of
#' the list is the length of the reference wavelength. Each element have the length of
#' the sequence center on the targeted wavelength (bandwith dependant).
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
#' raw = read.OPTICD("extdata/IML-4_OPTICD_20160501_000320.DAT")
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
  bdsh.factor <- rep(NA, nb.waves)
  ###

  #### Run over each wavelength

  for (i in 1:nb.waves) {
    print(paste("Apply bandshift correction to get Rrs at:", waves[i]))

    ix.waves0 = which.min(abs(waves[i] - waves0))
    waves0.vec = (waves0[ix.waves0]-band.width0[ix.waves0]/2):(waves0[ix.waves0]+band.width0[ix.waves0]/2)
    waves.vec =  (waves[i]-band.width[i]/2):(waves[i]+band.width[i]/2)

    ####section total backscattering
    ####backscattering calculation
    ix.waves1=which.min(abs(bbp.coef.iml4$waves1-waves0))
    ix.waves2=which.min(abs(bbp.coef.iml4$waves2-waves0))


    Ratio <- Rrs0[ix.waves1]/Rrs0[ix.waves2]
    #bbp.ref=coef.bbp[4]+coef.bbp[3]*R_490.555
    bbp.ref=10^(bbp.coef.iml4$intercept+bbp.coef.iml4$slope*Ratio)

    bbp.0=mean(spectral.bbp(waves0.vec,bbp.ref,532, bbp.coef.iml4$Sbp),na.rm=T)
    bbp=mean(spectral.bbp(waves.vec,bbp.ref,532, bbp.coef.iml4$Sbp),na.rm=T)


    #####water scattering
    bbw.0=mean(spectral.bw(waves0.vec))
    bbw=mean(spectral.bw(waves.vec))

    #####total back scattering calculation
    bb.total.0 <- bbw.0+bbp.0
    bb.total   <- bbw  +bbp

    ###
    ####total absorption section

    ####Anap Absorption
    ix.waves1=which.min(abs(anap.coef.iml4$waves1-waves0))
    ix.waves2=which.min(abs(anap.coef.iml4$waves2-waves0))

    Ratio <- Rrs0[ix.waves1]/Rrs0[ix.waves2]
    anap.ref=10^(anap.coef.iml4$intercept+anap.coef.iml4$slope*Ratio)

    anap.0 = mean(spectral.nap(waves0.vec,anap.ref,412,anap.coef.iml4$Snap),na.rm=T)
    anap = mean(spectral.nap(waves.vec,anap.ref,412,anap.coef.iml4$Snap),na.rm=T)

    ######pure water absorption
    aw.0  <- mean(spectral.aw(waves0.vec))
    aw    <- mean(spectral.aw(waves.vec))

    ######Yellow substances absorption
    ix.waves1=which.min(abs(ag.coef.iml4$waves1-waves0))
    ix.waves2=which.min(abs(ag.coef.iml4$waves2-waves0))

    Ratio <- Rrs0[ix.waves1]/Rrs0[ix.waves2]
    ag.ref=10^(ag.coef.iml4$intercept+ag.coef.iml4$slope*Ratio)

    ag.0 = mean(spectral.cdom(waves0.vec,ag.ref,400,ag.coef.iml4$Sg),na.rm=T)
    ag = mean(spectral.cdom(waves.vec,ag.ref,400,ag.coef.iml4$Sg),na.rm=T)


    ######Phyto absorption
    # Compute Aph for the reference wavebands
    # get index for wavelenght 1
    # create a matrix with the reference wavelength vector
    x = matrix(waves0, nrow=length(aph.coef.iml4$aph.waves),ncol=nb.waves0, byrow = T)
    # compute the difference with the first wavelengnth used in the regression
    y1 = abs(aph.coef.iml4$waves1 - x)
    y2 = abs(aph.coef.iml4$waves2 - x)

    # retreived the index
    ix.waves1 <- apply(y1,1,which.min)
    ix.waves2 <- apply(y2,1,which.min)

    # compute spectral aph
    R=Rrs0[ix.waves1]/Rrs0[ix.waves2]
    aph.spec=  10^(aph.coef.iml4$intercept + aph.coef.iml4$slope * R)

    # Averaged the aph over the wavebands
    ix.aph0 <- match(waves0.vec, aph.coef.iml4$aph.waves)
    ix.aph <- match(waves.vec, aph.coef.iml4$aph.waves)

    aph.0 <- mean(aph.spec[ix.aph0],na.rm = T)
    aph   <- mean(aph.spec[ix.aph],na.rm = T)


    #####total absorption calculation
    a.total.0 <-  aph.0 + ag.0 + aw.0 + anap.0
    a.total   <-  aph   + ag   + aw   + anap


    ### Compute the bandshift factor that will multiply the Rrs0 to get Rrs

    bb.over.abb   <- bb.total   / (bb.total  +  a.total)
    bb.over.abb.0 <- bb.total.0 / (bb.total.0+a.total.0)

    bdsh.factor[i] <- bb.over.abb / bb.over.abb.0

  }
  return(bdsh.factor)

}
