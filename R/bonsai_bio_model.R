#' @keywords internal
#'
#' @description the function to calculate LAI from the measured parameters.
#'
#' @param p: list of  9 parameters for calculating the LAI
#' @param vT: chosen temperature list day by day for the calculation
#'
#' @return LAI: the list of calculated LAI
#'
#'
bonsai <- function(p,vT) {
  # Initialisation of the 6 parameters supporting for calculating LAI
  # tzero, Ti, deltaT Ts, B and LAImax
  tzero = p["tzero"];Ti  = p["Ti"];
  deltaTs = p["deltaTs"];B = p["B"];
  LAImax= p["LAImax"];C = p["C"];

  vT1=vT # the temperature list day by day
  k=which(vT1<0);vT1[k]=0 # To make sure the calculationis not interrupted
  # as well as negative temperature occurs very little
  # and has no significant effect on the calculation
  # all the negative temperatures will be changed to 0
  T=cumsum(vT1) # Cummulative temperature

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
#' @param p: list of  9 parameters for calculating the LAI
#' @param LAI_t: the LAI result at the day t
#' @param PAR_t: the photosynthetically active radiation on day t
#'
#' @return  the list of calculated biomass
#'
biom <-function(p,LAI_t,PAR_t,biom_t){
  # Initialisation the rest 3 parameters for calculating biomass: Eb, Eimax and K
  Eb = p["Eb"];  Eimax = p["Eimax"];  K = p["K"];
  # Calculation of LAI based on the formula of "The biomasse model"
  biom_tp1=biom_t+Eb*Eimax*(1-exp(-K*LAI_t))*PAR_t;
  return(biom_tp1)
}



######################################################
#' @keywords internal
#'
#' @param t1 and tfin: the begin and end days of demanded period
#' @param p: 9 parameters to calculate the LAI and biomass
#' @param PAR: list of all the mesured photosynthetically active radiation
#' @param biom_t0: biomass in the day before our observation and mesurement, right here,
#' we consider it to be 0
#'
#' @return the list of calculated biomass and LAI in the chosen period
#'

bonsai_bio <- function(t1,tfin,p,T,PAR,biom_t0)
{

  LAI_total=bonsai(p,T); # Caculating the LAI list by day

  LAI=LAI_total[t1:tfin]; # The LAI in the demanded period

  biom_tmp=rep(0,tfin) # The biomass until the last day of demanded period

  # Calculating biomass in demanded periode
  for (t in 2:tfin)
  { biom_tmp[t] = biom(p,LAI_total[t-1],PAR[t-1],biom_tmp[t-1]);}
  # In the first day of vector biom_tmp: biom_tmp[1]= biom_t0 = 0
  # Update the results
  biom=biom_tmp[t1:tfin];
  return(cbind(LAI,biom))
}

