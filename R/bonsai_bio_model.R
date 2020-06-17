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
# Initialisation
  p_bonsai <- p
  p_biom <- p

# On calcule le LAI pour tous les jours definis dans T.
  biom_tmp=c(1:tfin)

  LAI_total=bonsai(p_bonsai,T);

# On ne renvoie le LAI que pour la periode demandee.
  LAI=LAI_total[t1:tfin];

# Calcul de la biomasse pour la periode demandee

# Cas t1=1
# Le premier jour, le LAI est different de 0, mais pas la biomasse.
# A partir du deuxieme jour, une valeur est calculee pour la biomasse (qui
# est donc supposee se faire dans la nuit, apres le LAI ...).
  if (t1==1){
    biom_tmp[1]=0;
  }else{
    biom_tmp[1]=biom_t0;
  }

# Calcul sur le reste de la periode
  i=2;
  for (t in max(2,t1):tfin)
  {
    biom_tmp[i]=biom(p_biom,LAI_total[t-1],PAR[t-1],biom_tmp[i-1]);
    i=i+1;
  }

# Mise en forme des resultats
  if (t1==1){
    biom=biom_tmp;
  }else{
    biom=biom_tmp[2:tfin-t1+2];
  }

  return(cbind(LAI,biom))
}
