
context("test_wrap")

temp_data_path <- system.file(file.path("extdata", "Bonsai_bio", "Tmoy_France_INRA_STATION_33550003 - R_mJ_m2.csv"),
                                package="CroptimizR")
model_options<-list()
model_options$path<-temp_data_path
model_options$begin_end<-c("130260","100300","160360","050260","140270")

p<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=0.002107,LAImax=10.197236,C=0.035339)
p1<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=0,LAImax=10.197236,C=0)
p2<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=-500,LAImax=10.197236,C=500)
p3<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=0.002107,LAImax=0,C=0.035339)
x <- rbind(p,p1,p2,p3)
#param_values <- array( c(x[,"K"],x[,"Eimax"],x[,"Eb"],x[,"tzero"],x[,"Ti"],x[,"deltaTs"],x[,"B"],x[,"LAImax"],x[,"C"]),
param_values <- array( c(p,p1,p2,p3),
                       dim=c(1,9,4),
                       dimnames=list(NULL,c("K", "Eimax","Eb","tzero","Ti",
                                            "deltaTs","B","LAImax","C")
                                     ,c("84007004_2017_140270","21106002_2015_160360",
                                        "54113002_2016_160360","33550003_2019_050260")))

param_values
res<-CroptimizR:::bonsai_bio_wrapper(model_options,param_values)
res$sim_list
res$error
test_that("bonsai_wrapper", {
  expect_equal(res$error,FALSE)
})

test_data_path<-system.file(file.path("extdata", "Bonsai_bio", "temperature_PAR_wrapper.csv"),
                            package="CroptimizR")
result_data_path <- system.file(file.path("extdata", "Bonsai_bio", "bonsai_bio_result_site1.csv"),
                                package="CroptimizR")

result<-read.csv2(result_data_path,header=TRUE)
LAI=as.numeric(as.vector(result[,"LAI"]))
biom=as.numeric(as.vector(result[,"Biomass"]))


model_options1<-list()
model_options1$path<-test_data_path
model_options1$begin_end<-c("001232")

param_values1 <- array( c(p),
                       dim=c(1,9,1),
                       dimnames=list(NULL,c("K", "Eimax","Eb","tzero","Ti",
                                            "deltaTs","B","LAImax","C")
                                     ,c("21106002_2013_001232")))
param_values1

res1<-CroptimizR:::bonsai_bio_wrapper(model_options1,param_values1)

LAI1<-res1$sim_list[[1]]$`21106002_2013_001232`[2]
LAI1<-as.data.frame(LAI1)
LAI_exp=LAI1$LAI

biom1<-res1$sim_list[[1]]$`21106002_2013_001232`[3]
biom1<-as.data.frame(biom1)
biom_exp=biom1$biom


test_that("bonsai_wrapper", {
  expect_equal(res1$error,FALSE)
  expect_equal(LAI_exp,LAI,tolerance=1e-4)
  expect_equal(biom_exp,biom,tolerance=1e-4)
})






