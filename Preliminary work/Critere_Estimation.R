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

  # USM -> List of USM groups
  # USM_list -> List of all USM

  sim_value <- vector("numeric")
  sim_inter <- vector("list")

  param_value_inter <- vector("numeric")
  for (i in 1:length(USM_list)){

    count=1
    for (y in 1:length(USM)){
      if (is.null(USM[[y]])==FALSE){
        for (z in 1:length(USM[[y]])){
          #print(USM[y])
          if (USM_list[i] %in% USM[[y]][[z]]){
            param_value_inter[y] <- param_value[count]
            count=count+1          }
        }
      } else {
        param_value_inter[y] <- param_value[count]
        count=count+1
      }
    }

    #print(param_value_inter)

    gen_param_sti(paste0("D:/Home/Tvailhere/Interface/DonneesSticsCas1c/", USM_list[i]), param_name, param_value_inter)
    set_codeoptim(paste0("D:/Home/Tvailhere/Interface/DonneesSticsCas1c/", USM_list[i]))
    run_system("D:/Home/Tvailhere/Interface/JavaSTICS-1.41-stics-9.0/bin/stics_modulo", "D:/Home/Tvailhere/Interface/DonneesSticsCas1c", USM_list[i])

    sim_inter<-get_daily_results(paste0("D:/Home/Tvailhere/Interface/DonneesSticsCas1c/",USM_list[i]), USM_list[i], doy_list = obs_jul[[i]])
    sim_value <- append(sim_value, sim_inter$lai.n.)

  }

  res <- crit(sim_value, obs_val, flag_log)
  print(param_value)
  print(res)
  return(res)

}


# --- SCRIPT

#TEST 1
param_name <- c("dlaimax", "durvieF", "test1", "test2", "test3")
lb <- list(0.0005, c(50, 40), 15, c(5, 100, 30), c(0, 0))
ub <- list(0.0025, c(400, 500), 30, c(50, 500, 60), c(10, 15))
# Tous les USM
USM <- list(c(),
            list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1"), c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
            c(),
            list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1"), c("bo96iN+", "lu96iN+"), c("lu96iN6", "lu97iN+")),
            list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1", "bo96iN+", "lu96iN+", "lu96iN6"), c("lu97iN+")))


#TEST 2
param_name <- c("dlaimax", "durvieF")
lb <- list(0.0005, c(50, 40))
ub <- list(0.0025, c(400, 500))
# Tous les USM
USM <- list(c(),
            list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1"), c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")))


lb_vec <- unlist(lb)
ub_vec <- unlist(ub)


#USM_list <- vector("character")
#for (i in 1:length(USM)){
#if (is.list(USM[[i]])==TRUE){
#for (y in 1:length(USM[[i]]))
#USM_list <- append(USM_list, USM[[i]][[y]])
#}
#}

USM_list <- unlist(USM)

if (TRUE %in% duplicated(USM_list)){
  doublons<-which(duplicated(USM_list))
  USM_list<-USM_list[-doublons]
}




# Pas réussi à le faire sans le "for"
#for (i in 1:(length(USM)-1)){
#param_name <- append(param_name, param_name[length(param_name)])
#lb <- append(lb, lb[length(lb)])
#ub <- append(ub, ub[length(ub)])
#param_value <- append(param_value, runif(1,lb[length(lb)],ub[length(ub)]))
#}

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

# Valeurs aléatoires des paramètres
param_value <- list()
for (i in 1:length(USM)){
  inter <- vector("numeric")
  if (is.null(USM[[i]])==TRUE){
    param_value <- append(param_value, runif(1,lb[[i]],ub[[i]]))
  } else {
    for (y in 1:length(USM[[i]])){
      inter <- append(inter, runif(1,lb[[i]][y],ub[[i]][y]))
    }
    param_value <- append(param_value, list(inter))
  }
}

param_value_vec <- unlist(param_value)


nloptr(x0 = param_value_vec, eval_f = Stic_Crit, lb = lb_vec, ub = ub_vec, opts = list("algorithm"="NLOPT_LN_NELDERMEAD", "xtol_rel"=1e-08, "maxeval"=10),
       param_name = param_name, USM = USM, USM_list = USM_list, obs_val = all_obs, obs_jul = all_jul_obs, flag_log = flag_log)



# ---------------------------------------------------------------------------------------------------------

# Graphics


init <- list()
fin <- list()
for (i in 1:length(lb_vec)){
  init[[i]] <- 0
  fin[[i]] <- 0
}

T1<-Sys.time()

for (z in 1:2){
  param_value_vec <- vector("numeric")
  for (i in 1:length(lb_vec)){
    param_value_vec[i] <- runif(1,lb_vec[i], ub_vec[i])
    init[[i]][z] <- param_value_vec[i]
  }

  nlo <- nloptr(x0 = param_value_vec, eval_f = Stic_Crit, lb = lb_vec, ub = ub_vec, opts = list("algorithm"="NLOPT_LN_NELDERMEAD", "xtol_rel"=1e-08, "maxeval"=2),
                param_name = param_name, USM = USM, USM_list = USM_list, obs_val = all_obs, obs_jul = all_jul_obs, flag_log = flag_log)


  for (y in 1:length(lb_vec)){
    fin[[y]][z] <- nlo$solution[y]
  }
}

T2<-Sys.time()
difftime(T2, T1)


# PDF
pdf(file = "graph_test.pdf", width = 9, height = 9, pointsize = 10)

for (i in 1:length(init)) {
  n<-which.min(init[[i]])
  plot(init[[i]], fin[[i]], main = "Estimated vs Initial values of the
parameters for different repetitions", text(init[[i]], fin[[i]], pos=1,col="black"), xlim = c(lb_vec[i],ub_vec[i]), ylim =   c(lb_vec[i],ub_vec[i]), xlab = paste("Initial value for", param_name_plot[i]), ylab = paste("Estimated value for", param_name_plot[i]))
  text(init[[i]][n], fin[[i]][n], labels = n, pos=1,col="red")
}

dev.off()












#Apr�s si �a marche, arranger le param_value, utiliser des apply pour r�duire les for dans Stic_Crit().
