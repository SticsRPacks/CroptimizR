
library(testthat)
context("test_wrap")
#File patath for the examples datas
temp_data_path <- file.path("extdata", "Bonsai_bio")
#Option for running the model
model_options<-list()
model_options<-list(temp_data_path,1,232)
names(model_options)<-c("path","begin","end")
#Situations to experiment i.e. several tzero
sit_names<-c("2013_Sit_204","2013_Sit_210","2013_Sit_220")

#Model's parameter
param_values<-c(1560.870155,678.088316,0.002107,10.197236,0.035339,2.726781,0.971669,0.65497)
param_names<-c("Ti","deltaTs","B","LAImax","C","Eb","Eimax","K")
param_values<-setNames(param_values,param_names)

res<-CroptimizR:::bonsai_bio_wrapper(model_options,sit_names,param_values)
res$sim_list
res$error

#Here we test if the model run without error
test_that("bonsai_wrapper", {
  expect_equal(res$error,FALSE)
})


#Here we prepare the test of the wrapper vs the model
#Do they give the same results by the 2 way of run

#preparing usefull datas
model_options1<-list()
model_options1$path<-temp_data_path
model_options1$begin<-1
model_options1$end<-232

param_values1<-c(1560.870155,678.088316,0.002107,10.197236,0.035339,2.726781,0.971669,0.65497)
param_names<-c("Ti","deltaTs","B","LAImax","C","Eb","Eimax","K")
param_values1<-setNames(param_values1,param_names)

temper <- read.csv2(system.file(model_options1$path,"Sit2013.csv",package="CroptimizR"))
TM        = as.numeric(as.vector(temper[,"TM"]))
PAR       = as.numeric(as.vector(temper[,"PAR"]))
#running bonsai_bio
result=CroptimizR:::bonsai_bio(model_options1$begin,model_options1$end,204,param_values1["Ti"],param_values1["deltaTs"],param_values1["B"],param_values1["LAImax"],param_values1["C"],param_values1["Eb"],param_values1["Eimax"],param_values1["K"],TM,PAR,0)
#running by the wrapper
res1<-CroptimizR:::bonsai_bio_wrapper(model_options1,sit_names,param_values1)
LAI=result[,1]
Biom=result[,2]
LAI1<-res1$sim_list$`2013_Sit_204`[2]
LAI1<-as.data.frame(LAI1)
LAI_exp=LAI1$LAI

biom1<-res1$sim_list$`2013_Sit_204`[3]
biom1<-as.data.frame(biom1)
biom_exp=biom1$biom

#Determining if the error row by row is less than 1e-4
test_that("bonsai_wrapper", {
  expect_equal(res1$error,FALSE)
  expect_equal(LAI_exp,LAI,tolerance=1e-4)
  expect_equal(biom_exp,Biom,tolerance=1e-4)
})






