#' @title Bonsai-bio wrapper to create LAI and biomass  result
#'
#' @description This function runs my crop model and force it with the values
#' of the parameters defined in the param_values argument. It returns
#' the values of the simulated outputs.
#'
#' @param sit_names vector of situations names for which results must be returned (name as sitenameyear.csv ie Sit2013 fir sitename=Sit and year=2013)
#'
#' @param model_options List containing any information needed to run the model.There
#' are 3 elements : path for finding the file, the beginning day, the end.
#'
#' @param param_values Named array that contains the value(s) and names of the
#' parameters to force for each situation to simulate. This array contains the different
#' parameters values for the different parameters .
#'
#' @return A list containing simulated values (`sim_list`: a vector of list (one
#' element per values of parameters) containing data.frames of simulated output values
#' for each simulated situation)
#'
#'
#'
#'@keywords internal
#'



bonsai_bio_wrapper <- function( model_options, sit_names, param_values,...) {

  # Update the values of parameters, if we have any new parameter values in the model_options,
  # they will be ajusted into the param_values_t (the parameter values by the time)

  # Initializations
  results <- list()
  path <- model_options$path
  t1 <- model_options$begin
  tfin <- model_options$end
  situation_names <- sit_names

  results$error=FALSE

  for (situation in situation_names) {
       St <- strsplit(situation,"_")
       AN <- St[[1]][1]
       Sit <- St[[1]][2]
       tzero <- as.numeric(St[[1]][3])

       temper <- read.csv2(system.file(path,paste0(Sit,AN,".csv"),package="CroptimizR"))
       NUM_POSTE = as.numeric(as.vector(temper[,"NUM_POSTE"]))
       #AN        = as.numeric(as.vector(temper[,"AN"]))
       MOIS      = as.numeric(as.vector(temper[,"MOIS"]))
       JOUR      = as.numeric(as.vector(temper[,"JOUR"]))
       TM        = as.numeric(as.vector(temper[,"TM"]))
       PAR       = as.numeric(as.vector(temper[,"PAR"]))
      # run the model for the given situation

      if ( t1>tfin )
        {
          warning("problem_in_the_time_interval")
          results$error=TRUE
      }else{

        # Calculating the LAI and biomass is based on Bonsai bio model
    		results$sim_list[[situation]]=bonsai_bio(t1,tfin,tzero,param_values["Ti"],param_values["deltaTs"],param_values["B"],param_values["LAImax"],param_values["C"],param_values["Eb"],param_values["Eimax"],param_values["K"],TM,PAR,0)#[,"LAI"]
        # Completing the list with exact times in the UTC standard form
        results$sim_list[[situation]]=dplyr::tibble(Date=as.POSIXct(as.character(as.Date(t1:tfin,origin=paste0(as.numeric(AN)-1,"-12-31"))),format="%Y-%m-%d",tz="UTC"),LAI=results$sim_list[[situation]][,"LAI"],biomas=results$sim_list[[situation]][,'biomas'])

      }

      # read the results and store the data.frame in result$sim_list[[i]][[situation]]
  }
  return(results)
}
