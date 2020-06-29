context("Test the bonsai-bio model")

weather_data_path <- system.file(file.path("extdata", "Bonsai_bio", "temperature_PAR.csv"),
                       package="CroptimizR")

result_data_path <- system.file(file.path("extdata", "Bonsai_bio", "bonsai_bio_result_site1.csv"),
                                 package="CroptimizR")





p<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=0.002107,LAImax=10.197236,C=0.035339)
p1<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=0,LAImax=10.197236,C=0)
p2<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=-4^200,LAImax=10.197236,C=3^200)
p3<- c(K=0.65497,Eimax=0.971669,Eb=2.726781,tzero=204.5381,Ti=1560.870155,deltaTs=678.088316,B=0.002107,LAImax=0,C=0.035339)




temp <- read.csv2(weather_data_path,header = FALSE)
result<-read.csv2(result_data_path,header=TRUE)


T=as.numeric(as.vector(temp[,1]))
PaR=as.numeric(as.vector(temp[,2]))

LAI=as.numeric(as.vector(result[,"LAI"]))
biom=as.numeric(as.vector(result[,"Biomass"]))


test_that("bonsai", {
  expect_equal(CroptimizR:::bonsai(p,T), LAI,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai(p1,T), c(1:232)*0,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai(p2,T), c(1:232)*0,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai(p3,T), c(1:232)*0,tolerance=1e-4)
})

t1=1
tfin=232
biom_t0=0





test_that("bonsai_bio", { #t1 et tfin vont changer , biom est mesuré en 2 ans par exp (1,2,3 ca depend des cultures),
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,p,T,PaR,biom_t0)[,"biom"], biom,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,p,T,PaR,biom_t0)[,"LAI"], LAI,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,p1,T,PaR,biom_t0)[,"biom"], c(1:232)*0,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,p1,T,PaR,biom_t0)[,"LAI"], c(1:232)*0,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,p2,T,PaR,biom_t0)[,"biom"], c(1:232)*0,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,p2,T,PaR,biom_t0)[,"LAI"], c(1:232)*0,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,p3,T,PaR,biom_t0)[,"biom"], c(1:232)*0,tolerance=1e-4)
  expect_equal(CroptimizR:::bonsai_bio(t1,tfin,p3,T,PaR,biom_t0)[,"LAI"], c(1:232)*0,tolerance=1e-4)
})

#interface

