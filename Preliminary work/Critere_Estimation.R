

# Commentaires pour Samuel :

# Bug possible : Inversion dans les groupes de paramètres, à vérifier.
# Modifier le code pour qu'il puisse traiter plusieurs variables à la fois
# Faire un test sur run_system et gen param_sti renvoyer une erreur si c'est faux
# Sortir une erreur dans Stics-crit (regarder la doc de run system et genparamsti -> False) (déja commencé, voir en bas mais rien de fonctionnel)
# Arranger le param_value, utiliser des apply pour réduire les for dans Stic_Crit().
# Voir si l'ordre d'installation des packages ne pose pas problème sur un ordinateur
# sur lequel ces packages ne sont pas installés




#---------------------------------------------------------------------------------------------------------------





# Preliminary information :

# Run the Stics model once by USM and put the files created in a folder (temporary txt files, if we run the model
# on several USM, the files will be replaced and it will remain only those of the last USM).
# There must be a folder by USM with txt files created by Stics
# Outside USM folders, there must be "input file" (ini, obs, tec, sta, climatic data), "USMs" file (XML document) and the folder "plant".





# ---------------------------------------------------------------------------------------------------------------





# Input parameters given by the user


# TEST for the entire script (parameters must be given below)

# Vector of parameters names (sans doublons)
param_name <- c("dlaimax", "durvieF")

# Vector of lower and upper boundaries
# If there are several groups of USM for a parameter (e.g. Simultaneous estimation of varietal and specific parameters on several varieties),
# boundaries for the different USM groups must be put in a vector within the list
lb <- list(0.0005, c(50, 50)) # lower bounds
ub <- list(0.0025, c(400, 400)) # upper bounds

# List of USMs to use for each parameter
# If there is only one USM group for the parameter, make a list with a vector containing all USMs
# If there are several groups of USM for a parameter, make a list with several vectors (one per group) containing the USM of the corresponding group
# Each parameter must have a list of vectors containing USM groups
USM <- list(list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1", "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
            list(c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"), c("bou99t3", "bou00t3", "bou99t1", "bou00t1")))


variable <- "lai.n." # Warning to how the variable is written by Stics (ex : lai(n) -> lai.n.)
val_removed <- -999.00 # Missing data to be removed from observations
nb_rep <- 2 # How many times we run the minimization with different parameters
xtol_rel <- 1e-05 # Tolerance criterion between two iterations
maxeval <- 5 # Maximum number of iterations executed by the function
flag_log <- TRUE # TRUE -> With log ; FALSE -> Without log

# Workspace_donne -> workspace directory where are USM folders
# Write full path, relative path does not work
workspace_donne <- "D:/Home/Tvailhere/Interface/DonneesSticsCas1c/"

# path_modulo -> directory of the executable of the JavaStics application (bin folder)
# To find the name of the executable, check in JavaStics -> Tools -> Select/Add Stics model version -> Corresponding executable
path_modulo <- "D:/Home/Tvailhere/Interface/JavaSTICS-1.41-stics-9.0/bin/stics_modulo"

# path_pdf -> Path and name of graphic that will be register
path_pdf <- "D:/Home/Tvailhere/Interface/graph.pdf"


# TEST of the part "Automatic calculations" (don't Work with nloptr)
# Test to see if we can put many different parameters with several USM groups
# and with different bounds for each group.

# param_name <- c("dlaimax", "durvieF", "test1", "test2", "test3")
# lb <- list(0.0005, c(50, 40), 15, c(5, 100, 30), c(0, 0))
# ub <- list(0.0025, c(400, 500), 30, c(50, 500, 60), c(10, 15))
# USM <- list(list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1", "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
            #list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1"), c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
            #list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1", "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
            #list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1"), c("bo96iN+", "lu96iN+"), c("lu96iN6", "lu97iN+")),
            #list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1", "bo96iN+", "lu96iN+", "lu96iN6"), c("lu97iN+")))



# TEST of the part "Automatic calculations" (Works with nloptr)
# Test to see if we can put only one parameter with a single USM group

# param_name <- c("dlaimax")
# lb <- list(0.0005)
# ub <- list(0.0025)
# USM <- list(list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1", "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")))





#----------------------------------------------------------------------------------------------------------------------





# Packages installation

# Put the .gz files on his computer and change the path to install the 3 following packages
if(!require("SticsEvalR")){
  install.packages("D:/Home/Tvailhere/Interface/SticsEvalR_1.36_r1215.tar.gz",  repos = NULL)
}

if(!require("SticsOnR")){
  install.packages("D:/Home/Tvailhere/Interface/SticsOnR_0.1_r1445.tar.gz",  repos = NULL)
}

if(!require("Classes")){
  install.packages("D:/Home/Tvailhere/Interface/Classes_1.1.tar.gz", repos = NULL)
}

if(!require("dplyr")){
  install.packages("dplyr")
}

if(!require("XML")){
  install.packages("XML")
}

if(!require("nloptr")){
  install.packages("nloptr")
}




# Run every time we open the script again
library(XML)
library(dplyr)
library(SticsEvalR)
library(SticsOnR)
library(Classes)
library(nloptr)





# ----------------------------------------------------------------------------------------------------------------------





# Functions for minimization


# Function which calculates the criterion
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


# Function that run each time a new simulation and which returns the criterion
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
    run_system(path_modulo, workspace_donne, USM_list[i])

    sim_inter <- get_daily_results(paste0(workspace_donne, USM_list[i]), USM_list[i], doy_list = obs_jul[[i]])
    sim_value <- append(sim_value, eval(parse(text = paste0("sim_inter$", variable))))

  }

  res <- crit(sim_value, obs_val, flag_log)
  #print(param_value_vec)
  #print(res)
  return(res)

}





# ----------------------------------------------------------------------------------------------------------------------





# Automatic calculations (users should not modified this part of the code)

lb_vec <- unlist(lb)
ub_vec <- unlist(ub)

# Create a vector with the name of the parameters for each USM group
param_name_plot <- vector("character")
for (i in 1:length(lb)){
  for (y in 1:length(lb[[i]])){
    param_name_plot <- append(param_name_plot, param_name[i])
  }
}

# List with all USM (without duplicates)
USM_list <- unlist(USM)
if (TRUE %in% duplicated(USM_list)){
  doublons<-which(duplicated(USM_list))
  USM_list<-USM_list[-doublons]
}


val_obs <- vector("list")
jul_obs <- vector("list")
val_obs_int <- vector("list")
all_obs <- vector("numeric")
all_jul_obs <- vector("list")
# If epsilon smaller, give -Inf in the log (where flag_log -> TRUE)
eps <- 1e-300


# Observed values
for (a in 1:length(USM_list)) {
  obs <- getObsData(paste0(workspace_donne, USM_list[a],".obs"))
  val_obs[[a]] <- eval(parse(text = paste0("obs$table$", variable)))
  jul_obs[[a]] <- obs$table$jul


  # Removes the -999
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

# Random values of the parameters (Useful only for the test below)
# param_value <- list()
# for (i in 1:length(USM)){
#  inter <- vector("numeric")
#  if (length(USM[[i]])==1){
#    param_value <- append(param_value, runif(1,lb[[i]],ub[[i]]))
#  } else {
#    for (y in 1:length(USM[[i]])){
#      inter <- append(inter, runif(1,lb[[i]][y],ub[[i]][y]))
#    }
#    param_value <- append(param_value, list(inter))
#  }
# }
# param_value_vec <- unlist(param_value)



# Test to see if minimization works, useless for the graphics below
# nloptr(x0 = param_value_vec, eval_f = Stic_Crit, lb = lb_vec, ub = ub_vec, opts = list("algorithm"="NLOPT_LN_NELDERMEAD", "xtol_rel"=1e-08, "maxeval"=10),
       #param_name = param_name, USM = USM, USM_list = USM_list, obs_val = all_obs, obs_jul = all_jul_obs, flag_log = flag_log)





# ---------------------------------------------------------------------------------------------------------





# Graphics



# Creation of 3 vectors :
# The initial parameters, the final parameters and the minimized criterion
param_init <- list()
param_fin <- list()
all_crit <- vector("numeric")
for (i in 1:length(lb_vec)){
  param_init[[i]] <- 0
  param_fin[[i]] <- 0
}

# Start time calculation
T1 <- Sys.time()

# Run "nloptr" and save the results in differents vectors and in "nlo"
nlo <- list()
for (z in 1:nb_rep){
  param_value_vec <- vector("numeric")
  for (i in 1:length(lb_vec)){
    param_value_vec[i] <- runif(1,lb_vec[i], ub_vec[i])
    param_init[[i]][z] <- param_value_vec[i]
  }

  nlo[[z]] <- nloptr(x0 = param_value_vec, eval_f = Stic_Crit, lb = lb_vec, ub = ub_vec, opts = list("algorithm"="NLOPT_LN_NELDERMEAD", "xtol_rel"=xtol_rel, "maxeval"=maxeval),
                param_name = param_name, USM = USM, USM_list = USM_list, obs_val = all_obs, obs_jul = all_jul_obs, flag_log = flag_log)


  for (y in 1:length(lb_vec)){
    param_fin[[y]][z] <- nlo[[z]]$solution[y]
  }
  all_crit[z] <- nlo[[z]]$objective
}


# End time calculation
T2 <- Sys.time()

# Total time
difftime(T2, T1)

# Save the result of minimization for each repetition
save(nlo, file = "nlo.Rdata")

# Which repetion has the smallest criterion
min_pos<-which.min(all_crit)

# PDF
pdf(file = path_pdf, width = 9, height = 9, pointsize = 10)
for (i in 1:length(param_init)) {
  plot(param_init[[i]], param_fin[[i]], main = "Estimated vs Initial values of the
parameters for different repetitions", text(param_init[[i]], param_fin[[i]], pos=1,col="black"), xlim = c(lb_vec[i],ub_vec[i]),
       ylim =   c(lb_vec[i],ub_vec[i]), xlab = paste("Initial value for", param_name_plot[i]),
       ylab = paste("Estimated value for", param_name_plot[i]))
  text(param_init[[i]][min_pos], param_fin[[i]][min_pos], labels = min_pos, pos=1,col="red")
}
dev.off()

# Display of parameters for the repetion who have the smallest criterion
for (nb_param in 1:length(param_fin)){
  print(paste("L'estimation de", param_name_plot[nb_param], "est égal à :", param_fin[[nb_param]][min_pos]))
}
print(paste("Le critère minimum est égale à :", all_crit[min_pos]))













# Test message erreur pour gen_param_sti :
# if(gen_param_sti(paste0(workspace_donne, USM_list[i]), param_name, param_value_inter)==FALSE){
#   print(" ERROR : gen_param_sti")
#   stop()
# } else {
#   gen_param_sti(paste0(workspace_donne, USM_list[i]), param_name, param_value_inter)
# }
