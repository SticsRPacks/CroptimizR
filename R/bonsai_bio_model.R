#' @keywords internal
#'
#' @description the function to calculate LAI from the measured parameters.
#'
#' @param tzero : emergence date (°C)
#' @param Ti : maximum growth temperature threshold (°C)
#' @param deltaTs : sum of temperature between point of maximum growth and senescence (°C)
#' @param B: senescence speed
#' @param LAImax : maximum LAI
#' @param C : inflection point in the growing part
#' @param vT: chosen temperature list day by day for the calculation
#'
#' @return LAI: the list of calculated LAI
#'
#'
bonsai <- function(tzero,Ti,deltaTs,B,LAImax,C,vT) {
  # Initialisation of the 6 parameters supporting for calculating LAI
  # tzero, Ti, deltaTs, B, C and LAImax

  vT1=vT # the temperature list day by day
  k=which(vT1<0);vT1[k]=0 # To make sure the calculation is not interrupted
  # as well as negative temperature occurs very little
  # and has no significant effect on the calculation
  # all the negative temperatures will be changed to 0
  T=cumsum(vT1) # Cumulative temperature

  # Constraints to resolve the problems whether the log applies on 0 in the LAI formula.
  if (C!=0){
    if (exp((B/C)*(deltaTs+Ti))<=1) {A=0;
    } else {  A=(1.0/Ti)*(log(exp((B/C)*(deltaTs+Ti))-1))  }
  } else {A=0}
  # Calculating LAI based on the formula page 8
  LAI=LAImax*((1 + exp(-A*(T - tzero -Ti)))^(-C) - exp(B*(T - tzero-deltaTs - Ti)));
  I=which(LAI<0);
  if (length(I)>0){
    LAI[I] = 0;
  }
  return(LAI)}


######################################################
#' @keywords internal
#'
#'
#'
#' @param K : extinction coefficient of incident radiation
#' @param Eb : efficiency of transformation of radiation absorbed by dry matter (g/MJ)
#' @param Eimax : maximum efficiency of radiation interception
#' @param LAI_t: the LAI result at the day t
#' @param PAR_t: the photosynthetically active radiation on day t
#' @param biom_t: biomass on day t
#'
#' @return  the biomass at t+1
#'
biom <-function(Eb,Eimax,K,LAI_t,PAR_t,biom_t){
  # Initialisation the rest 3 parameters for calculating biomass: Eb, Eimax and K
  # Calculation of LAI based on the formula of "The biomasse model"
  biom_tp1=biom_t+Eb*Eimax*(1-exp(-K*LAI_t))*PAR_t;
  return(biom_tp1)
}



######################################################
#' @keywords internal
#'
#' @param t1 and tfin: the begin and end days of demanded period
#' @param tzero : emergence date (°C)
#' @param Ti : maximum growth temperature threshold (°C)
#' @param deltaTs : sum of temperature between point of maximum growth and senescence (°C)
#' @param B: senescence speed
#' @param LAImax : maximum LAI
#' @param C : inflection point in the growing part
#' @param Eb : efficiency of transformation of radiation absorbed by dry matter (g/MJ)
#' @param Eimax : maximum efficiency of radiation interception
#' @param K : extinction coefficient of incident radiation#' @param PAR: list of all the measured photosynthetically active radiation
#' @param T : chosen temperature list day by day for the calculation
#' @param PAR : the photosynthetically active radiation list day by day
#' @param biom_t0: biomass in the day before our observation and measurement, right here,
#' we consider it to be 0
#'
#'
#' @return the list of calculated biomass and LAI in the chosen period
#'

bonsai_bio <- function(t1,tfin,tzero,Ti,deltaTs,B,LAImax,C,Eb,Eimax,K,T,PAR,biom_t0)
{

  LAI_total=bonsai(tzero,Ti,deltaTs,B,LAImax,C,T); # Calculating the LAI list by day

  LAI=LAI_total[t1:tfin]; # The LAI in the demanded period

  biom_tmp=rep(0,tfin) # The biomass until the last day of demanded period

  # Calculating biomass in demanded period
  for (t in 2:tfin)
  { biom_tmp[t] = biom(Eb,Eimax,K,LAI_total[t-1],PAR[t-1],biom_tmp[t-1]);}
  # In the first day of vector biom_tmp: biom_tmp[1]= biom_t0 = 0
  # Update the results
  biomas=biom_tmp[t1:tfin];
  return(cbind(LAI,biomas))
}

