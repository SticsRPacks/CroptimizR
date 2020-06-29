#' @keywords internal
#'
#'
bonsai <- function(p,vT) {
  # Initialisation
  tzero = p["tzero"];
  Ti  = p["Ti"];
  deltaTs = p["deltaTs"];
  B = p["B"];
  LAImax= p["LAImax"];
  C = p["C"];



  vT1=vT
  k=which(vT1<0)
  vT1[k]=0
  T=cumsum(vT1)

  # Contraintes pour eviter des problemes numeriques avec le log dans la
  # formule du LAI.
  if (C!=0){
    if (exp((B/C)*(deltaTs+Ti))<=1) {
      A=0;
    } else {
      A=(1.0/Ti)*(log(exp((B/C)*(deltaTs+Ti))-1))
    }
  } else {
    A=0
  }
  # Calcul du LAI selon la formule (3.1)
  LAI=LAImax*((1 + exp(-A*(T - tzero -Ti)))^(-C) - exp(B*(T - tzero-deltaTs - Ti)));
  I=which(LAI<0);
  if (length(I)>0){
    LAI[I] = 0;
  }

  return(LAI)
}

#' @keywords internal

biom <-function(p,LAI_t,PAR_t,biom_t){
# Initialisation
  Eb = p["Eb"];
  Eimax = p["Eimax"];
  K = p["K"];

  biom_tp1=biom_t+Eb*Eimax*(1-exp(-K*LAI_t))*PAR_t;
  return(biom_tp1)
}

#' @keywords internal

bonsai_bio <- function(t1,tfin,p,T,PAR,biom_t0)
{

# On calcule le LAI pour tous les jours definis dans T.
  LAI_total=bonsai(p,T);

# On ne renvoie le LAI que pour la periode demandee.
  LAI=LAI_total[t1:tfin];

# Calcul de la biomasse pour la periode demandee

# Calcul sur le reste de la periode
  biom_tmp=rep(0,tfin)
  for (t in 2:tfin)
  {
    biom_tmp[t]=biom(p,LAI_total[t-1],PAR[t-1],biom_tmp[t-1]);
  }

# Mise en forme des resultats
  biom=biom_tmp[t1:tfin];

  return(cbind(LAI,biom))
}

