# Packages installation
install.packages("D:/Home/Tvailhere/Interface/SticsEvalR_1.36_r1215.tar.gz",  repos = NULL)
install.packages("D:/Home/Tvailhere/Interface/SticsOnR_0.1_r1445.tar.gz",  repos = NULL)
install.packages("D:/Home/Tvailhere/Interface/Classes_1.1.tar.gz", repos = NULL)
install.packages("dplyr")
install.packages("lubridate")
install.packages("XML")
install.packages("nloptr")
install.packages("stringr")

library(XML)
library(dplyr)
library(lubridate)
library(SticsEvalR)
library(SticsOnR)
library(Classes)
library(nloptr)
library(stringr)




# ----
crit <- function(sim,obs,flag_log){
  res <- 0
  if (flag_log==TRUE){
    for (i in 1:length(obs)) {
      res <- res + (log(sim[i] + eps) - log(obs[i] + eps))^2
    }
  } else {
    for (i in 1:length(obs)) {
      res <- res + (sim[i] - obs[i])^2
    }
  }
  return (res)
}



# ----
Stic_Crit <- function(param_value, param_name, USM, USM_list, obs_val, obs_jul, flag_log){
  
  sim_value <- vector("numeric")
  sim_inter <- vector("list")
  
  print(param_value)
  for (i in 1:length(USM_list)){
    for (y in 1:length(USM)){
      if (USM_list[i] %in% USM[[y]]){
        param_name_inter<-append(param_name[1],param_name[y])
        param_value_inter<-append(param_value[1],param_value[y])
        gen_param_sti(paste0("D:/Home/Tvailhere/Interface/DonneesSticsCas1c/",USM_list[i]), param_name_inter, param_value_inter)
        run_system("D:/Home/Tvailhere/Interface/JavaSTICS-1.41-stics-9.0/bin/stics_modulo", "D:/Home/Tvailhere/Interface/DonneesSticsCas1c", USM_list[i]) 
      }
    }
    sim_inter<-get_daily_results(paste0("D:/Home/Tvailhere/Interface/DonneesSticsCas1c/",USM_list[i]), USM_list[i], doy_list = obs_jul[[i]])
    sim_value <- append(sim_value, sim_inter$lai.n.)
  }
  
  res <- crit(sim_value, obs_val, flag_log)
  print(res)
  return(res)
  
}


# --- SCRIPT
# Tous les USM
USM <- list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1"), c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"))

# Paramètre qui change selon les USM est toujours mis en dernier
# Maximum un seul paramètre qui change selon les USM
param_name <- c("dlaimax", "durvieF")
lb <- c(0.0005, 50)
ub <- c(0.0025, 400)



USM_list <- vector("character")
for (i in 1:length(USM)){
  USM_list <- append(USM_list, USM[[i]])
}

param_value <- c(runif(1,lb[1],ub[1]), runif(1,lb[2],ub[2]))

# Pas réussi à le faire sans le "for"
for (i in 1:(length(USM)-1)){
  param_name <- append(param_name, param_name[length(param_name)]) 
  lb <- append(lb, lb[length(lb)]) 
  ub <- append(ub, ub[length(ub)]) 
  param_value <- append(param_value, runif(1,lb[length(lb)],ub[length(ub)])) 
}

val_removed <- -999.00
flag_log <- FALSE
val_obs <- vector("list")
jul_obs <- vector("list")
val_obs_int <- vector("list")
all_obs <- vector("numeric")
all_jul_obs <- vector("list")
# Si epsilon plus petit, il est pas pris en compte et donne des -Inf dans les log
eps <- 1e-300  


# Valeurs observés (A modifier ensuite pour le cas "plusieurs variables")
for (a in 1:length(USM_list)) {
  obs <- getObsData(paste0("D:/Home/Tvailhere/Interface/DonneesSticsCas1c/",USM_list[a],"_csv.obs"))
  val_obs[[a]] <- obs$table$lai.n.
  jul_obs[[a]] <- obs$table$jul
  
  
  # Enlève les -999
  jul_obs2 <- vector("numeric")
  for (i in 1:length(jul_obs[[a]])) {  
    if (val_obs[[a]][i]!=val_removed) {
      val_obs2 <- val_obs[[a]][i]
      all_obs <- append(all_obs,val_obs2)
      jul_obs2 <- append(jul_obs2,jul_obs[[a]][i])
    }
  }
  all_jul_obs <- append(all_jul_obs,list(jul_obs2))
  
}

nloptr(x0 = param_value, eval_f = Stic_Crit, lb = lb, ub = ub, opts = list("algorithm"="NLOPT_LN_NELDERMEAD", "xtol_rel"=1e-08, "maxeval"=10), 
       param_name = param_name, USM = USM, USM_list = USM_list, obs_val = all_obs, obs_jul = all_jul_obs, flag_log = flag_log)


