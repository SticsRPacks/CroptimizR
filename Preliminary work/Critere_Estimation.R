# Packages installation
install.packages("D:/Home/Tvailhere/Interface/SticsEvalR_1.36_r1215.tar.gz",  repos = NULL)
install.packages("D:/Home/Tvailhere/Interface/SticsOnR_0.1_r1445.tar.gz",  repos = NULL)
install.packages("D:/Home/Tvailhere/Interface/Classes_1.1.tar.gz", repos = NULL)
install.packages("dplyr")
install.packages("XML")
install.packages("nloptr")
install.packages("profvis")
install.packages("lineprof")

library(XML)
library(dplyr)
library(SticsEvalR)
library(SticsOnR)
library(Classes)
library(nloptr)
library(profvis)



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
Stic_Crit <- function(param_value_vec, param_name, USM, USM_list, obs_val, obs_jul, flag_log){

  # USM -> List of USM groups
  # USM_list -> List of all USM

  sim_value <- vector("numeric")
  sim_inter <- vector("list")


  param_value_inter <- vector("numeric")
  for (i in 1:length(USM_list)){
    count=1
    for (y in 1:length(USM)){
      # print(paste0("count :", count))
      if (length(USM[[y]])!=1){
        for (z in 1:length(USM[[y]])){
          if (USM_list[i] %in% USM[[y]][[z]]){
            param_value_inter[y] <- param_value_vec[count]
            count=count+1
          } else {
            count=count+1
          }
        }
      } else {
        param_value_inter[y] <- param_value_vec[count]
        count=count+1
      }


    }

    # print(param_value_inter)

    gen_param_sti(paste0(workspace_donne, USM_list[i]), param_name, param_value_inter)
    set_codeoptim(paste0(workspace_donne, USM_list[i]))
    run_system(workspace_modulo, workspace_donne, USM_list[i])

    sim_inter<-get_daily_results(paste0(workspace_donne, USM_list[i]), USM_list[i], doy_list = obs_jul[[i]])
    sim_value <- append(sim_value, sim_inter$lai.n.)

  }

  res <- crit(sim_value, obs_val, flag_log)
  #print(param_value_vec)
  #print(res)
  return(res)

}


# --- SCRIPT

# ------ Paramètres à rentrer par l'utilisateur

# TEST 1 (Ne fonctionne pas, seulement un test)
param_name <- c("dlaimax", "durvieF", "test1", "test2", "test3")
lb <- list(0.0005, c(50, 40), 15, c(5, 100, 30), c(0, 0))
ub <- list(0.0025, c(400, 500), 30, c(50, 500, 60), c(10, 15))
# Tous les USM
USM <- list(list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1", "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
            list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1"), c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
            list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1", "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
            list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1"), c("bo96iN+", "lu96iN+"), c("lu96iN6", "lu97iN+")),
            list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1", "bo96iN+", "lu96iN+", "lu96iN6"), c("lu97iN+")))


# TEST 2 (Fonctionne)
param_name <- c("dlaimax", "durvieF")
lb <- list(0.0005, c(50, 50))
ub <- list(0.0025, c(400, 400))
# Tous les USM
USM <- list(list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1", "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
            list(c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"), c("bou99t3", "bou00t3", "bou99t1", "bou00t1")))



# TEST 3 (Fonctionne mais plus interessant de tester avec le TEST 2)
param_name <- c("dlaimax")
lb <- list(0.0005)
ub <- list(0.0025)
# Tous les USM
USM <- list(list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1", "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")))



nb_rep <- 2 # Combien de fois on relance nloptr avec des paramètres différts (pour les graphiques)
xtol_rel <- 1e-05 # Tolérance
maxeval <- 5 # Nb max d'evaluation
workspace_donne <- "D:/Home/Tvailhere/Interface/DonneesSticsCas1c/"
workspace_modulo <- "D:/Home/Tvailhere/Interface/JavaSTICS-1.41-stics-9.0/bin/stics_modulo"


# ------ Calculs automatiques (à ne pas modifier par l'utilisateur)

lb_vec <- unlist(lb)
ub_vec <- unlist(ub)

param_name_plot <- vector("character")
for (i in 1:length(lb)){
  for (y in 1:length(lb[[i]])){
    param_name_plot <- append(param_name_plot, param_name[i])
  }
}

USM_list <- unlist(USM)

if (TRUE %in% duplicated(USM_list)){
  doublons<-which(duplicated(USM_list))
  USM_list<-USM_list[-doublons]
}



val_removed <- -999.00
flag_log <- TRUE
val_obs <- vector("list")
jul_obs <- vector("list")
val_obs_int <- vector("list")
all_obs <- vector("numeric")
all_jul_obs <- vector("list")
# Si epsilon plus petit, il est pas pris en compte et donne des -Inf dans les log
eps <- 1e-300


# Valeurs observÃ©s (A modifier ensuite pour le cas "plusieurs variables")
for (a in 1:length(USM_list)) {
  obs <- getObsData(paste0(workspace_donne, USM_list[a],"_csv.obs"))
  val_obs[[a]] <- obs$table$lai.n.
  jul_obs[[a]] <- obs$table$jul


  # EnlÃ¨ve les -999
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

# Valeurs aleatoires des parametres (Non necessaire je crois car fait plus tard dans la partie "Graphics")
param_value <- list()
for (i in 1:length(USM)){
  inter <- vector("numeric")
  if (length(USM[[i]])==1){
    param_value <- append(param_value, runif(1,lb[[i]],ub[[i]]))
  } else {
    for (y in 1:length(USM[[i]])){
      inter <- append(inter, runif(1,lb[[i]][y],ub[[i]][y]))
    }
    param_value <- append(param_value, list(inter))
  }
}
param_value_vec <- unlist(param_value)



# Test pour voir si Minimization marche, ne doit pas forcément être executee
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

# Debut calcul temps
T1 <- Sys.time()

nlo <- list()
for (z in 1:nb_rep){
  param_value_vec <- vector("numeric")
  for (i in 1:length(lb_vec)){
    param_value_vec[i] <- runif(1,lb_vec[i], ub_vec[i])
    init[[i]][z] <- param_value_vec[i]
  }

  nlo[[z]] <- nloptr(x0 = param_value_vec, eval_f = Stic_Crit, lb = lb_vec, ub = ub_vec, opts = list("algorithm"="NLOPT_LN_NELDERMEAD", "xtol_rel"=xtol_rel, "maxeval"=maxeval),
                param_name = param_name, USM = USM, USM_list = USM_list, obs_val = all_obs, obs_jul = all_jul_obs, flag_log = flag_log)


  for (y in 1:length(lb_vec)){
    fin[[y]][z] <- nlo[[z]]$solution[y]
  }
}

# Fin calcul temps
T2 <- Sys.time()

# Temps total
difftime(T2, T1)

save(nlo, file = "nlo.Rdata")

# PDF
pdf(file = "graph.pdf", width = 9, height = 9, pointsize = 10)

for (i in 1:length(init)) {
  n<-which.min(fin[[i]])
  plot(init[[i]], fin[[i]], main = "Estimated vs Initial values of the
parameters for different repetitions", text(init[[i]], fin[[i]], pos=1,col="black"), xlim = c(lb_vec[i],ub_vec[i]), ylim =   c(lb_vec[i],ub_vec[i]), xlab = paste("Initial value for", param_name_plot[i]), ylab = paste("Estimated value for", param_name_plot[i]))
  text(init[[i]][n], fin[[i]][n], labels = n, pos=1,col="red")
}

dev.off()

#Workspace a rentrer par l'utilisateur
#Après si ça marche, arranger le param_value, utiliser des apply pour réduire les for dans Stic_Crit().
