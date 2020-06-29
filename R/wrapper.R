#' @title MyModel wrapper for CroptimizR
#'
#' @description This function runs my crop model and force it with the values
#' of the parameters defined in the param_values argument. It returns
#' the values of the simulated outputs.
#'
#' @param model_options List containing any information needed to run the model.There
#' are 3 elements : path for finding the file, the beginning day, the end.
#'
#' @param param_values Named 3D array that contains the value(s) and names of the
#' parameters to force for each situation to simulate. This array contains the different
#' parameters values (first dimension) for the different parameters (second dimension)
#' and for the different situations (third dimension).
#'
#' @return A list containing simulated values (`sim_list`: a vector of list (one
#' element per values of parameters) containing data.frames of simulated output values
#' for each simulated situation) and an error code (`error`) indicating if at least
#' one simulation ended with an error.
#'
#'
#'
#'@keywords internal
#'



bonsai_bio_wrapper <- function( model_options, param_values) {

  # Initializations
  results <- list()
  path <- model_options$path
  begin_end<-(model_options$begin_end)
  temper <- read.csv2(path)



  NUM_POSTE = as.numeric(as.vector(temper[,"NUM_POSTE"]))
  AN        = as.numeric(as.vector(temper[,"AN"]))
  MOIS      = as.numeric(as.vector(temper[,"MOIS"]))
  JOUR      = as.numeric(as.vector(temper[,"JOUR"]))
  TM        = as.numeric(as.vector(temper[,"TM"]))

  PAR       = as.numeric(as.vector(temper[,"PAR"]))
  table<-matrix(c(NUM_POSTE,AN,MOIS,JOUR,TM,PAR),ncol=6)
  colnames(table)<-c("NUM_POSTE","AN","MOIS","JOUR","TM","PAR")
  table<-as.data.frame(table)

  Poste_An<-(unique(data.frame(NUM_POSTE,AN)))




  situation_names=paste(Poste_An$NUM_POSTE,Poste_An$AN,sep="_")
  situation_names=paste(situation_names,begin_end,sep="_")
  situation_names<-intersect(situation_names,dimnames(param_values)[[3]])

  nb_paramValues <- dim(param_values)[1]
  param_names <- dimnames(param_values)[[2]]

  results$sim_list <-  vector("list",nb_paramValues)
  results$error=FALSE






  for (i in 1:nb_paramValues) {

    for (situation in situation_names) {

      # overwrite model input parameters of names contained in param_names with values retrieved in param_values[i,,situation]

      # run the model for the given situation
      t1  =as.numeric(substr(situation,15,17))
      tfin=as.numeric(substr(situation,18,20))
      if ( t1>tfin )
        {
          warning("problem_in_the_time_interval")
          results$error=TRUE
      }else{

        tab<-table[which(table$NUM_POSTE==as.numeric(substr(situation,1,8)) &
                           table$AN==as.numeric(substr(situation,10,13))),]
        LAI =CroptimizR:::bonsai_bio(t1,tfin,param_values[i,,situation],tab$TM,tab$PAR,0)[,"LAI"]
        biom=CroptimizR:::bonsai_bio(t1,tfin,param_values[i,,situation],tab$TM,tab$PAR,0)[,"biom"]

        results$sim_list[[i]][[situation]]=dplyr::tibble(Date=as.POSIXct(as.character(as.Date(t1:tfin,origin=paste0(as.numeric(substr(situation,10,13)),"-01-01"))),
                                                                  format="%Y-%m-%d",tz="UTC"),LAI=LAI,biom=biom)
      }



      }

      # read the results and store the data.frame in result$sim_list[[i]][[situation]]


  }

  return(results)

}

