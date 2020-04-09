#' @title Generate CroptimizR vignettes by assembling R scripts
#'
#' @param croptimizR_path path to CroptimizR repository
#' @param vignettes vector of vignettes' names to generate (chose between those given by default)
#'
#' @return list of path of the generated RMD files
#'
#' @keywords internal
#'
generate_vignette <- function(croptimizR_path=getwd(),vignettes=c("stics_simple","stics_specific_varietal","apsim","dream")) {

  input_output_files = list( stics_simple = list(input_files=c("Parameter_estimation_Stics_intro.R","Parameter_estimation_Stics_install.R","Parameter_estimation_Stics_init.R","Parameter_estimation_estim.R","Parameter_estimation_post_treatment.R"),
                                                 output_file="Parameter_estimation_simple_case.R"),

                             stics_specific_varietal = list(input_files=c("Parameter_estimation_Specific_and_Varietal_intro.R","Parameter_estimation_Specific_and_Varietal_install.R","Parameter_estimation_Specific_and_Varietal_init.R","Parameter_estimation_estim.R","Parameter_estimation_Specific_and_Varietal_post_treatment.R"),
                                                            output_file="Parameter_estimation_Specific_and_Varietal.R"),

                             apsim = list(input_files=c("ApsimX_parameter_estimation_simple_case_install.R","ApsimX_parameter_estimation_simple_case_init.R","ApsimX_parameter_estimation_simple_case_estim.R","ApsimX_parameter_estimation_simple_case_post_treatment.R"),
                                          output_file="ApsimX_parameter_estimation_simple_case.R"),

                             dream = list(input_files=c("Parameter_estimation_DREAM_intro.R","Parameter_estimation_DREAM_install.R","Parameter_estimation_DREAM_init.R","Parameter_estimation_DREAM_estim.R","Parameter_estimation_DREAM_post_treatment.R"),
                                          output_file="Parameter_estimation_DREAM.R")
                            )

  lapply(vignettes, function(v) {
                                out_file <- file(file.path(croptimizR_path,"vignettes",input_output_files[[v]]$output_file), "w")
                                lapply(input_output_files[[v]]$input_files, function(in_file) writeLines(readLines(file.path(croptimizR_path,"vignettes",in_file)),out_file))
                                close(out_file)
                                knitr::spin(hair=file.path(croptimizR_path,"vignettes",input_output_files[[v]]$output_file),knit = FALSE,
                                            format = c("Rmd"))
                                })

}
